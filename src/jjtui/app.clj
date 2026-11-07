(ns jjtui.app
  "The application state machine.

   `update-state` is pure: it takes the state and a message and returns
   [state effects], where effects are data maps (see jjtui.effects). Keeping
   I/O out of here makes every flow testable without a terminal or a repo."
  (:require
   [charm.components.text-input :as ti]
   [clojure.string :as str]
   [jjtui.config :as config]
   [jjtui.graph :as graph]
   [jjtui.keys :as keys]
   [jjtui.log :as log]
   [jjtui.state :refer [cursor-entry cursor-index cursor-row entries picker-matches
                        row-cursor rows search-matches selectable-indices]]
   [jjtui.view :as view]))

;; ---------------------------------------------------------------------------
;; State

(defn repo-state
  "A freshly opened repo (a jjtui.repo/Opened), before its log has loaded.
   Switching repos replaces this wholesale and reloads."
  [config {:repo/keys [dir base-template]}]
  {:repo/dir dir
   :repo/base-template base-template
   :repo/revset (:config/revset config)
   :repo/good-revset (:config/revset config)
   :repo/log {:log/entries [] :log/rows []}
   :repo/loading? true
   :repo/cursor nil
   :repo/cursor-row 0
   :repo/offset 0
   :repo/marks #{}
   :repo/expanded #{}
   :repo/search nil
   :repo/preview nil
   :repo/preview-cache {}
   :repo/ignore-immutable? false})

(defn init-state
  "`repo` is the jjtui.repo/Opened to start in, or nil with `mode` the repo
   chooser."
  [{:keys [config config-error repo mode]}]
  {:app/config config
   :app/repo (some->> repo (repo-state config))
   :app/log-gen 0
   :app/show-files? (:config/show-files? config)
   :app/split? (:config/split? config)
   :app/split-ratio (:config/split-ratio config)
   :app/diff-tool (:config/diff-tool config)
   :app/mode mode
   :app/message (when config-error {:message/text config-error :message/level :error})
   :app/busy nil
   :app/width 80
   :app/height 24
   :app/dark? true
   :app/exec nil})

(defn- update-repo
  "Updates the open repo; a no-op while none is open."
  [state f & args]
  (if (:app/repo state)
    (apply update state :app/repo f args)
    state))

(defn- message [state text level]
  (assoc state :app/message {:message/text text :message/level level}))

(defn- info [state text] (message state text :info))
(defn- error [state text] (message state text :error))

(defn- change-cursor [entry]
  {:change/id (:change/id entry) :file/path nil})

(defn- set-cursor-index [state i]
  (if-let [row (get (rows state) i)]
    (update-repo state assoc :repo/cursor (row-cursor state row) :repo/cursor-row i)
    state))

(defn- move-cursor [state delta]
  (let [idxs (selectable-indices state)
        current (cursor-index state)]
    (if (or (empty? idxs) (nil? current))
      state
      (let [pos (or (first (keep-indexed (fn [p i] (when (= i current) p)) idxs)) 0)
            pos (max 0 (min (dec (count idxs)) (+ pos delta)))]
        (set-cursor-index state (nth idxs pos))))))

(defn log-height
  "Rows available to the log: the screen minus the status bar and the bottom
   panel. Shares view/panel-lines with the view so the two can't disagree."
  [state]
  (max 1 (- (:app/height state) 1 (count (view/panel-lines state)))))

(defn- move-rows [state delta]
  (let [idxs (selectable-indices state)
        current (or (cursor-index state) 0)
        target (+ current delta)
        pick (if (pos? delta)
               (or (first (filter #(>= % target) idxs)) (peek idxs))
               (or (last (filter #(<= % target) idxs)) (first idxs)))]
    (if pick (set-cursor-index state pick) state)))

(defn- block-end
  "Last row index of the change that row `i` belongs to."
  [rs i]
  (let [entry (:row/entry (get rs i))]
    (loop [j i]
      (let [next-row (get rs (inc j))]
        (if (and next-row (= entry (:row/entry next-row)) (not= :node (:row/kind next-row)))
          (recur (inc j))
          j)))))

(defn scroll-into-view
  "Adjusts the repo's offset so the cursor's change is visible, keeping a
   small margin."
  [state]
  (let [h (log-height state)
        rs (rows state)
        idx (cursor-index state)]
    (if (or (nil? idx) (empty? rs))
      (assoc-in state [:app/repo :repo/offset] 0)
      (let [hi (min (block-end rs idx) (+ idx h -1))
            margin (min 2 (quot (dec h) 4))
            off (get-in state [:app/repo :repo/offset])
            off (if (> (+ hi margin) (+ off h -1)) (- (+ hi margin) (dec h)) off)
            off (if (< (- idx margin) off) (- idx margin) off)]
        (assoc-in state [:app/repo :repo/offset] (max 0 (min off (- (count rs) h))))))))

;; ---------------------------------------------------------------------------
;; Selection

(defn marked-entries
  "Marked changes in log order (newest first)."
  [state]
  (filter #(contains? (get-in state [:app/repo :repo/marks]) (:change/id %)) (entries state)))

(defn- targets
  "The changes an action applies to: the marks, else the cursor's change."
  [state]
  (let [marked (marked-entries state)]
    (if (seq marked)
      (vec marked)
      (if-let [e (cursor-entry state)] [e] []))))

(defn- ids [es] (mapv :change/id es))

(defn- target-bookmarks
  "The local bookmarks on the target changes."
  [state]
  (vec (distinct (mapcat :change/bookmarks (targets state)))))

(defn- describe-entries [es]
  (str/join ", " (map :change/short-id es)))

(defn- described? [entry]
  (not (str/blank? (:change/description entry))))

(defn- ignore-immutable [state args]
  (cond-> args (get-in state [:app/repo :repo/ignore-immutable?]) (conj "--ignore-immutable")))

;; ---------------------------------------------------------------------------
;; Effects

(defn- log-params [{:app/keys [repo] :as state}]
  {:revset (:repo/revset repo)
   :base-template (:repo/base-template repo)
   :files (cond
            (:app/show-files? state) :all
            (seq (:repo/expanded repo)) (log/files-revset (:repo/expanded repo))
            :else nil)})

(defn reload
  "State and effect for reloading the log."
  [state]
  (let [gen (inc (:app/log-gen state))]
    [(-> (assoc state :app/log-gen gen) (update-repo assoc :repo/loading? true))
     [{:fx/type :load-log :fx/gen gen :fx/params (log-params state)}]]))

(defn- run-jj
  "Runs a non-interactive jj command in the background, reloading after.
   `then` holds further commands, run while each succeeds."
  [state args {:keys [label success clear-marks? then]}]
  (if (:app/busy state)
    [(error state (str "Busy: " (:app/busy state))) []]
    [(assoc state :app/busy (or label (str "jj " (first args))))
     [(cond-> {:fx/type :jj :fx/args args :fx/success success :fx/clear-marks? clear-marks?}
        (seq then) (assoc :fx/then (vec then)))]]))

(defn- run-exec
  "Suspends the TUI to run `argv` in the terminal."
  [state argv & {:keys [wait? reload? clear-marks?] :or {reload? true}}]
  [(cond-> (assoc state :app/exec {:exec/argv argv :exec/wait? wait? :exec/reload? reload?})
     clear-marks? (update-repo assoc :repo/marks #{}))
   [{:fx/type :quit}]])

(defn- open-repo
  "Opens the jj repo at `dir` (`op` :open), or sets jj up there first (:init
   colocates with the git repo at `dir`, :create makes a new one), replacing
   the open repo once it's ready."
  [state op dir]
  (if (:app/busy state)
    [(error state (str "Busy: " (:app/busy state))) []]
    [(assoc state :app/busy (if (= :open op) (str "Opening " dir) (str "Setting up jj in " dir)))
     [(if (= :open op)
        {:fx/type :open-repo :fx/dir dir}
        {:fx/type :init-repo :fx/dir dir :fx/colocate? (= :init op)})]]))

;; ---------------------------------------------------------------------------
;; Diffs

(defn- active-tool [state]
  (get-in state [:app/config :config/diff-tools (:app/diff-tool state)]))

(defn- change-target
  "The diff of one change, optionally limited to some files."
  [entry files]
  {:target/revset (:change/commit-id entry)
   :target/files files
   :target/from-sha (or (first (:change/parents entry)) "")
   :target/to-sha (:change/commit-id entry)})

(defn- diff-target
  "What the diff applies to: a file of the cursor change, the marks, or the
   cursor change. The shas are filled in when they're known without asking jj."
  [state]
  (let [row (cursor-row state)
        entry (cursor-entry state)
        marked (marked-entries state)]
    (cond
      (and (seq marked) (not= :file (:row/kind row)))
      (let [revset (str/join " | " (map :change/commit-id marked))]
        {:target/revset revset
         :target/files []
         :target/from-rev (str "roots(" revset ")")
         :target/to-rev (str "heads(" revset ")")})

      entry
      (let [{:file/keys [source path] :as file} (:row/file row)]
        (change-target entry (if file (vec (distinct (remove nil? [source path]))) []))))))

(defn- branch-target
  "The whole branch `ref` since its fork point with `base`, like a PR."
  [base ref]
  (let [fork (str "fork_point(" base " | " ref ")")]
    {:target/revset (str fork ".." ref)
     :target/files []
     :target/from-rev fork
     :target/from-commit? true
     :target/to-rev ref}))

(defn- bookmark-item [{:bookmark/keys [ref commit description]} value]
  {:item/label (str ref "  " commit "  " description) :item/value value})

(defn- change-label [{:change/keys [short-id commit-id bookmarks description]}]
  (str/join "  " (remove str/blank? [short-id
                                     (subs commit-id 0 (min 8 (count commit-id)))
                                     (str/join " " bookmarks)
                                     (if (str/blank? description) "(no description set)" description)])))

(defn- change-base-item [{:change/keys [commit-id] :as entry}]
  {:item/label (change-label entry) :item/value commit-id :item/change entry})

(defn- source-items
  "Branches to diff: the change at the cursor first, by its first bookmark
   when it has one, then the (other) bookmarks."
  [state bms]
  (let [cursor (when-let [{:change/keys [bookmarks commit-id] :as entry} (cursor-entry state)]
                 {:item/label (change-label entry)
                  :item/detail "cursor"
                  :item/value (or (first bookmarks) commit-id)})]
    (-> (if cursor [cursor] [])
        (into (comp (map #(bookmark-item % (:bookmark/ref %)))
                    (remove #(= (:item/value cursor) (:item/value %))))
              bms))))

(defn- parent-item
  "A bookmark `source` stacks on, tagged next to its name so it shows however
   narrow the picker is."
  [bms bname]
  (let [{:bookmark/keys [commit description]}
        (first (filter #(and (= bname (:bookmark/name %)) (nil? (:bookmark/remote %))) bms))]
    {:item/label (str/join "  " (remove str/blank? [(str bname " (parent)") commit description]))
     :item/value bname}))

(defn- base-items
  "What `source` (a bookmark, or the commit id of a change without one) can
   be diffed against, the default first: the bookmarks it stacks on, trunk,
   the other bookmarks, then the loaded changes."
  [state graph source bms]
  (let [parents (when graph
                  (graph/parent-bookmarks graph (if (graph/node graph source)
                                                  source
                                                  (:node/id (graph/bookmark-node graph source)))))
        parent? (set parents)]
    (-> (mapv #(parent-item bms %) parents)
        (conj {:item/label "trunk()" :item/value "trunk()"})
        (into (comp (map #(bookmark-item % (:bookmark/ref %)))
                    (remove #(or (= source (:item/value %)) (parent? (:item/value %)))))
              bms)
        (into (map change-base-item) (entries state)))))

(defn diff-vars [state target]
  {"revset" (:target/revset target)
   "files" (:target/files target)
   "from-sha" (:target/from-sha target)
   "to-sha" (:target/to-sha target)
   "width" (str (:app/width state))
   "repo" (get-in state [:app/repo :repo/dir])})

(defn- open-diff
  "Opens `target` in the active diff tool, first asking jj for the commit ids
   when the tool needs them and they aren't known yet."
  [state target]
  (let [{:tool/keys [cmd wait?]} (active-tool state)
        needs-shas? (or (config/uses-var? cmd "from-sha") (config/uses-var? cmd "to-sha"))]
    (cond
      (nil? cmd)
      [(error state (str "No diff tool named " (:app/diff-tool state))) []]

      (and needs-shas? (nil? (:target/to-sha target)))
      [(info state "Resolving commits…") [{:fx/type :resolve-diff :fx/target target}]]

      (and needs-shas? (str/blank? (:target/from-sha target)))
      [(error state "The root commit has no parent to diff against") []]

      :else
      (run-exec state (config/render-command cmd (diff-vars state target))
                :wait? wait? :reload? false))))

;; ---------------------------------------------------------------------------
;; Preview (split diff pane)

(defn preview-width [state]
  (- (:app/width state) (view/log-width state) 1))

(defn- preview-key
  "[argv width] of the preview the cursor wants, or nil."
  [state]
  (when-let [target (and (:app/split? state) (cursor-entry state) (diff-target state))]
    (let [cmd (get-in state [:app/config :config/preview-command])
          width (preview-width state)
          target (if (seq (marked-entries state))
                   ;; The pane follows the cursor, not the marks
                   (change-target (cursor-entry state) [])
                   target)]
      [(config/render-command cmd (assoc (diff-vars state target) "width" (str width))) width])))

(defn- sync-preview
  "Points the preview at the cursor: from the cache when possible, otherwise
   after a short debounce so holding j doesn't run jj for every row."
  [[state fx]]
  (let [k (preview-key state)
        current (get-in state [:app/repo :repo/preview :preview/key])]
    (cond
      (or (nil? k) (= k current)) [state fx]

      (contains? (get-in state [:app/repo :repo/preview-cache]) k)
      [(assoc-in state [:app/repo :repo/preview] {:preview/key k
                                                   :preview/lines (get-in state [:app/repo :repo/preview-cache k])
                                                   :preview/offset 0})
       fx]

      :else
      [(assoc-in state [:app/repo :repo/preview] {:preview/key k
                                                   :preview/lines (get-in state [:app/repo :repo/preview :preview/lines])
                                                   :preview/offset 0
                                                   :preview/loading? true})
       (conj (vec fx) {:fx/type :debounce
                       :fx/ms 60
                       :fx/msg {:type :msg/preview-fetch :msg/key k}})])))

(defn- scroll-preview [state delta]
  (if-not (and (:app/split? state) (get-in state [:app/repo :repo/preview]))
    state
    (let [n (count (get-in state [:app/repo :repo/preview :preview/lines]))
          h (log-height state)]
      (update-in state [:app/repo :repo/preview :preview/offset]
                 #(max 0 (min (+ (or % 0) delta) (- n h)))))))

;; ---------------------------------------------------------------------------
;; Modes

(defn- input [value prompt]
  (ti/text-input :prompt prompt :value (or value "")))

(defn- open-picker [state purpose title items & {:as extra}]
  (assoc state :app/mode (merge {:mode/type :picker
                                 :mode/purpose purpose
                                 :mode/title title
                                 :mode/items items
                                 :mode/selected 0
                                 :mode/input (input "" "› ")}
                                extra)))

(defn- open-menu [state purpose title options & {:as extra}]
  (assoc state :app/mode (merge {:mode/type :menu
                                 :mode/purpose purpose
                                 :mode/title title
                                 :mode/options options
                                 :mode/flags #{}}
                                extra)))

(defn- open-input [state purpose title value prompt & {:as extra}]
  (assoc state :app/mode (merge {:mode/type :input
                                 :mode/purpose purpose
                                 :mode/title title
                                 :mode/input (input value prompt)}
                                extra)))

(defn- confirm [state prompt on-yes]
  (assoc state :app/mode {:mode/type :confirm :mode/prompt prompt :mode/on-yes on-yes}))

(defn- start-pick [state action sources]
  (if (empty? sources)
    [(error state "No change at cursor") []]
    [(assoc state :app/mode {:mode/type :pick
                             :mode/action action
                             :mode/sources (ids sources)
                             :mode/source-flag "-r"})
     []]))

;; ---------------------------------------------------------------------------
;; Actions

(defn- squash-needs-editor?
  "jj squash opens an editor only when it has to combine two descriptions.
   An unknown parent counts as described, which errs toward the terminal."
  [sources target-entry]
  (let [target-described? (if target-entry (described? target-entry) true)]
    (> (+ (count (filter described? sources)) (if target-described? 1 0)) 1)))

(defn- squash [state sources target-entry args]
  (let [args (ignore-immutable state args)]
    (if (squash-needs-editor? sources target-entry)
      (run-exec state (into ["jj"] args) :clear-marks? true)
      (run-jj state args {:label "Squashing" :clear-marks? true}))))

(defn- entry-by-commit [state commit-id]
  (first (filter #(= commit-id (:change/commit-id %)) (entries state))))

(defn- entry-by-change [state change-id]
  (first (filter #(= change-id (:change/id %)) (entries state))))

(defn- rebase [state dest-flag]
  (let [{:mode/keys [sources source-flag]} (:app/mode state)
        target (cursor-entry state)]
    (cond
      (nil? target) [(error state "No change at cursor") []]
      (some #{(:change/id target)} sources) [(error state "Can't rebase a change onto itself") []]
      :else
      (run-jj (assoc state :app/mode nil)
              (ignore-immutable state (-> ["rebase"]
                                          (into (mapcat (fn [id] [source-flag id]) sources))
                                          (into [dest-flag (:change/id target)])))
              {:label "Rebasing" :clear-marks? true}))))

(defn- next-change
  "Moves the cursor to the next change, skipping its files. Stays put on the last."
  [state]
  (if-let [next-node (first (filter #(= :node (:row/kind (get (rows state) %)))
                                    (range (inc (or (cursor-index state) 0)) (count (rows state)))))]
    (set-cursor-index state next-node)
    state))

(defn- toggle-rebase-source
  "Adds the cursor's change to the changes being rebased, or removes it when
   already there, then steps to the next change like marking does. Removing
   the last one cancels the rebase, as esc does. Every source gets the same
   -r/-s/-b flag, as with marks."
  [state]
  (if-let [id (:change/id (cursor-entry state))]
    (let [sources (get-in state [:app/mode :mode/sources])]
      (cond
        (not (some #{id} sources))
        [(next-change (assoc-in state [:app/mode :mode/sources] (conj sources id))) []]
        (= [id] sources) [(info (assoc state :app/mode nil) "Cancelled") []]
        :else [(next-change (assoc-in state [:app/mode :mode/sources] (filterv #(not= id %) sources))) []]))
    [(error state "No change at cursor") []]))

(defn- finish-pick [state dest-flag]
  (let [{:mode/keys [action sources]} (:app/mode state)
        target (cursor-entry state)]
    (case action
      :rebase (rebase state dest-flag)
      :squash-into
      (cond
        (nil? target) [(error state "No change at cursor") []]
        (some #{(:change/id target)} sources) [(error state "Can't squash a change into itself") []]
        :else (squash (assoc state :app/mode nil)
                      (keep #(entry-by-change state %) sources)
                      target
                      ["squash" "--from" (str/join " | " sources) "--into" (:change/id target)])))))

(defn- with-entry [state f]
  (if-let [entry (cursor-entry state)]
    (f entry)
    [(error state "No change at cursor") []]))

(defn- clear-cursor-file
  "Moves the cursor from a file row to its change."
  [state]
  (update-repo state update :repo/cursor #(some-> % (assoc :file/path nil))))

(defn- toggle-files [state]
  (if-let [entry (cursor-entry state)]
    (let [id (:change/id entry)
          state (if (:app/show-files? state)
                  ;; Collapsing one change out of "all": expand all the others
                  (-> (assoc state :app/show-files? false)
                      (assoc-in [:app/repo :repo/expanded] (disj (set (ids (entries state))) id)))
                  (update-in state [:app/repo :repo/expanded] #(if (contains? % id) (disj % id) (conj % id))))]
      (reload (clear-cursor-file state)))
    [state []]))

(defn- ensure-split [state]
  (if (:app/split? state) state (info state "Split pane is closed, press | to open it")))

;; ---------------------------------------------------------------------------
;; Search

(defn- next-match
  "The match after entry index `from` going `dir` (1 or -1), wrapping around;
   `from` itself counts when `here?`."
  [matches from dir here?]
  (let [after? (if (pos? dir) (if here? >= >) (if here? <= <))
        candidates (if (pos? dir) matches (rseq matches))]
    (or (first (filter #(after? % from) candidates)) (first candidates))))

(defn- goto-entry [state i]
  (if-let [entry (get (entries state) i)]
    (assoc-in state [:app/repo :repo/cursor] (change-cursor entry))
    state))

(defn- step-search
  "Moves the cursor to the next (`dir` 1) or previous (-1) change matching
   `query`, wrapping around."
  [state query dir]
  (let [matches (search-matches state query)]
    (if (empty? matches)
      (error state (str "No change matches " (pr-str (str/trim (str query)))))
      (goto-entry state (next-match matches (or (:change/index (cursor-entry state)) 0) dir false)))))

(defn perform
  "Runs a normal-mode action."
  [state action]
  (case action
    :cursor/down [(move-cursor state 1) []]
    :cursor/up [(move-cursor state -1) []]
    :cursor/top [(set-cursor-index state (first (selectable-indices state))) []]
    :cursor/bottom [(set-cursor-index state (peek (selectable-indices state))) []]
    :cursor/half-down [(move-rows state (max 1 (quot (log-height state) 2))) []]
    :cursor/half-up [(move-rows state (- (max 1 (quot (log-height state) 2)))) []]
    :cursor/working-copy
    (if-let [wc (first (filter :change/working-copy? (entries state)))]
      [(assoc-in state [:app/repo :repo/cursor] (change-cursor wc)) []]
      [(error state "The working copy is not in this revset") []])
    :refresh (reload state)
    :repo/switch [(info state "Loading recent repos…")
                  [{:fx/type :recent-repos :fx/exclude #{(get-in state [:app/repo :repo/dir])}}]]
    :search [(assoc state :app/mode {:mode/type :search
                                     :mode/input (input (get-in state [:app/repo :repo/search]) "/")
                                     :mode/origin (get-in state [:app/repo :repo/cursor])})
             []]
    :search/next [(step-search state (get-in state [:app/repo :repo/search]) 1) []]
    :search/prev [(step-search state (get-in state [:app/repo :repo/search]) -1) []]
    :search/clear [(update-repo state assoc :repo/search nil) []]
    :help [(assoc state :app/mode {:mode/type :help :mode/offset 0
                                   :mode/search (ti/text-input :prompt "/" :focused false)})
           []]
    :quit [state [{:fx/type :quit}]]

    :files/toggle (toggle-files state)
    :files/toggle-all (reload (-> state
                                  (update :app/show-files? not)
                                  (assoc-in [:app/repo :repo/expanded] #{})
                                  (clear-cursor-file)))
    :split/toggle [(update state :app/split? not) []]
    :preview/down [(scroll-preview (ensure-split state) 3) []]
    :preview/up [(scroll-preview (ensure-split state) -3) []]
    :preview/page-down [(scroll-preview (ensure-split state) (log-height state)) []]
    :preview/page-up [(scroll-preview (ensure-split state) (- (log-height state))) []]
    :split/shrink [(update state :app/split-ratio #(max 0.2 (- % 0.05))) []]
    :split/grow [(update state :app/split-ratio #(min 0.8 (+ % 0.05))) []]
    :revset/set [(open-input state :revset "Log revset (empty for default)" (get-in state [:app/repo :repo/revset]) "revset › ") []]

    :diff/open (if-let [target (diff-target state)]
                 (open-diff state target)
                 [(error state "No change at cursor") []])
    :diff/branch [(info state "Loading bookmarks…")
                  [{:fx/type :bookmarks
                    :fx/purpose :branch-diff
                    :fx/graph (str "bookmarks()" (some->> (cursor-entry state) :change/commit-id (str " | ")))}]]
    :diff/pick-tool
    [(open-picker state :diff-tool "Diff tool"
                  (vec (for [k (sort (keys (get-in state [:app/config :config/diff-tools])))]
                         {:item/label (str (name k) (when (= k (:app/diff-tool state)) "  (current)"))
                          :item/value k})))
     []]

    :mark/toggle
    (if-let [id (:change/id (cursor-entry state))]
      [(next-change (update-in state [:app/repo :repo/marks] #(if (contains? % id) (disj % id) (conj % id)))) []]
      [state []])
    :mark/clear [(cond-> (assoc-in state [:app/repo :repo/marks] #{})
                   (seq (get-in state [:app/repo :repo/marks])) (info "Cleared marks"))
                 []]

    :describe (with-entry state
                (fn [entry]
                  [(info state "Loading description…")
                   [{:fx/type :description :fx/change-id (:change/id entry)}]]))
    :new (let [ts (targets state)]
           (if (empty? ts)
             [(error state "No change at cursor") []]
             (run-jj state (into ["new"] (ids ts))
                     {:label "Creating change" :clear-marks? true
                      :success (if (> (count ts) 1)
                                 (str "Created merge of " (describe-entries ts))
                                 (str "Created new change on " (describe-entries ts)))})))
    :new-menu (with-entry state
                (fn [entry]
                  [(open-menu state :new (str "New change relative to " (:change/short-id entry))
                              [{:option/key "a" :option/label "insert after (-A)"}
                               {:option/key "b" :option/label "insert before (-B)"}
                               {:option/key "e" :option/label "--no-edit" :option/toggle :no-edit}]
                              :mode/entry entry)
                   []]))
    :edit (with-entry state #(run-jj state (ignore-immutable state ["edit" (:change/id %)])
                                     {:label "Editing" :success (str "Editing " (:change/short-id %))}))
    :abandon (let [ts (targets state)]
               (if (empty? ts)
                 [(error state "No change at cursor") []]
                 [(confirm state
                           (into [(str "Abandon " (count ts) " change" (when (> (count ts) 1) "s") "?")]
                                 (map #(str "  " (:change/short-id %) "  "
                                            (if (described? %) (:change/description %) "(no description)"))
                                      ts))
                           [:abandon (ids ts)])
                  []]))
    :absorb (with-entry state #(run-jj state ["absorb" "--from" (:change/id %)] {:label "Absorbing"}))
    :squash (with-entry state
              (fn [entry]
                (let [marked (remove #(= (:change/id entry) (:change/id %)) (marked-entries state))]
                  (if (seq marked)
                    (squash state marked entry
                            ["squash" "--from" (str/join " | " (ids marked)) "--into" (:change/id entry)])
                    (squash state [entry] (entry-by-commit state (first (:change/parents entry)))
                            ["squash" "-r" (:change/id entry)])))))
    :squash-into (start-pick state :squash-into (targets state))
    :rebase (let [[state' fx] (start-pick state :rebase (targets state))]
              ;; Like a chained r: step past the change just made a source
              [(cond-> state'
                 (some #{(:change/id (cursor-entry state'))} (get-in state' [:app/mode :mode/sources]))
                 next-change)
               fx])
    :split-change (with-entry state #(run-exec state (ignore-immutable state ["jj" "split" "-r" (:change/id %)])))
    :resolve (with-entry state
               (fn [entry]
                 (if (:change/conflict? entry)
                   (let [path (get-in (cursor-row state) [:row/file :file/path])]
                     (run-exec state (ignore-immutable state (cond-> ["jj" "resolve" "-r" (:change/id entry)]
                                                               path (conj path)))))
                   [(error state (str (:change/short-id entry) " has no conflicts")) []])))
    :undo (run-jj state ["undo"] {:label "Undoing"})
    :redo (run-jj state ["redo"] {:label "Redoing"})
    :toggle-ignore-immutable
    (let [state (update-in state [:app/repo :repo/ignore-immutable?] not)]
      [(info state (if (get-in state [:app/repo :repo/ignore-immutable?]) "--ignore-immutable on" "--ignore-immutable off")) []])

    :bookmark/set (if (cursor-entry state)
                    [(info state "Loading bookmarks…") [{:fx/type :bookmarks :fx/purpose :bookmark-set}]]
                    [(error state "No change at cursor") []])
    :bookmark/menu
    (with-entry state
      (fn [entry]
        (let [bms (:change/bookmarks entry)]
          (if (empty? bms)
            [(error state (str "No bookmarks on " (:change/short-id entry))) []]
            [(open-menu state :bookmark (str "Bookmarks on " (:change/short-id entry) ": " (str/join ", " bms))
                        [{:option/key "d" :option/label "delete"}
                         {:option/key "r" :option/label "rename"}]
                        :mode/entry entry)
             []]))))
    :push (let [ts (targets state)]
            (run-jj state (into ["git" "push"] (mapcat (fn [id] ["-r" id]) (ids ts))) {:label "Pushing"}))
    :push-bookmarks
    (if-let [bms (seq (target-bookmarks state))]
      (run-jj state (into ["git" "push"] (mapcat (fn [b] ["-b" b]) bms)) {:label "Pushing"})
      [(info state "Loading bookmarks…") [{:fx/type :bookmarks :fx/purpose :push}]])
    :fetch (run-jj state ["git" "fetch"] {:label "Fetching"})
    :pull
    (if-let [bms (seq (target-bookmarks state))]
      (run-jj state (into ["git" "fetch"] (mapcat (fn [b] ["-b" b]) bms)) {:label "Pulling"})
      [(info state "Loading bookmarks…") [{:fx/type :bookmarks :fx/purpose :pull}]])

    [state []]))

(defn- bookmark-chosen
  "Continues the bookmark menu once a bookmark is picked."
  [state op bookmark]
  (case op
    :delete [(confirm state [(str "Delete bookmark " bookmark "?")] [:bookmark-delete bookmark]) []]
    :rename [(open-input state :rename (str "Rename bookmark " bookmark) bookmark "new name › "
                         :mode/bookmark bookmark)
             []]))

(defn- menu-choice [state {:mode/keys [purpose entry flags]} option]
  (let [state (assoc state :app/mode nil)
        after? (= "a" (:option/key option))]
    (case purpose
      :new (run-jj state (ignore-immutable state
                                           (cond-> ["new"]
                                             (contains? flags :no-edit) (conj "--no-edit")
                                             :always (conj (if after? "-A" "-B") (:change/id entry))))
                   {:label "Creating change"
                    :success (str "Created new change " (if after? "after " "before ") (:change/short-id entry))})
      :bookmark
      (let [op (if (= "d" (:option/key option)) :delete :rename)
            bms (:change/bookmarks entry)]
        (if (= 1 (count bms))
          (bookmark-chosen state op (first bms))
          [(open-picker state :bookmark-op (str (name op) " bookmark")
                        (mapv (fn [b] {:item/label b :item/value b}) bms)
                        :mode/op op)
           []])))))

(defn- picker-choice [state {:mode/keys [purpose op source bookmarks graph]} {:item/keys [value create?]}]
  (let [state (assoc state :app/mode nil)]
    (case purpose
      :diff-tool [(info (assoc state :app/diff-tool value) (str "Diff tool: " (name value))) []]
      :branch-diff [(open-picker state :branch-base
                                 (str "Diff " (or (:change/short-id (entry-by-commit state value)) value)
                                      " from its fork point with")
                                 (base-items state graph value bookmarks)
                                 :mode/source value)
                    []]
      :branch-base (open-diff state (branch-target value source))
      :bookmark-op (bookmark-chosen state op value)
      :repo (open-repo state :open value)
      :push (run-jj state ["git" "push" "-b" value] {:label "Pushing"})
      :pull
      (let [{bname :bookmark/name :bookmark/keys [remote ref tracked?]} value]
        (run-jj state ["git" "fetch" "--remote" remote "-b" bname]
                (if tracked?
                  {:label "Pulling"}
                  {:label "Pulling" :then [["bookmark" "track" ref]] :success (str "Pulled and tracked " ref)})))
      :bookmark-set
      (with-entry state
        (fn [entry]
          (if create?
            (run-jj state ["bookmark" "create" value "-r" (:change/id entry)]
                    {:label "Creating bookmark" :success (str "Created " value " at " (:change/short-id entry))})
            (run-jj state ["bookmark" "set" value "-r" (:change/id entry) "--allow-backwards"]
                    {:label "Moving bookmark" :success (str "Moved " value " to " (:change/short-id entry))})))))))

(defn- confirmed [state [op arg]]
  (let [state (assoc state :app/mode nil)]
    (case op
      :abandon (run-jj state (ignore-immutable state (into ["abandon"] arg))
                       {:label "Abandoning" :clear-marks? true
                        :success (str "Abandoned " (count arg) " change" (when (> (count arg) 1) "s"))})
      :bookmark-delete (run-jj state ["bookmark" "delete" arg]
                               {:label "Deleting bookmark" :success (str "Deleted bookmark " arg)}))))

(defn- input-submitted [state {:mode/keys [purpose bookmark input]}]
  (let [value (str/trim (ti/value input))
        state (assoc state :app/mode nil)]
    (case purpose
      :revset (reload (update-repo state assoc :repo/revset (not-empty value) :repo/marks #{}))
      :rename (if (or (str/blank? value) (= value bookmark))
                [state []]
                (run-jj state ["bookmark" "rename" bookmark value]
                        {:label "Renaming bookmark" :success (str "Renamed " bookmark " to " value)})))))

(defn- describe-message
  "The popup's first line on top of the rest of the original description."
  [{:mode/keys [input body]}]
  (str/trim (str (str/trim (ti/value input)) "\n" body)))

(defn- handle-describe-key [state k msg]
  (let [{:mode/keys [change-id short-id] :as mode} (:app/mode state)
        closed (assoc state :app/mode nil)]
    (case k
      "escape" [closed []]
      "enter" (run-jj closed (ignore-immutable state ["describe" "-r" change-id "-m" (describe-message mode)])
                      {:label "Describing" :success (str "Described " short-id)})
      ;; jj opens its configured editor ($JJ_EDITOR, ui.editor, $VISUAL, $EDITOR) on the popup's text
      "ctrl+e" (run-exec closed (ignore-immutable state ["jj" "describe" "-r" change-id "--editor" "-m" (describe-message mode)]))
      (let [[in _] (ti/text-input-update (:mode/input mode) msg)]
        [(assoc-in state [:app/mode :mode/input] in) []]))))

(defn- description-loaded [state {:msg/keys [change-id result]}]
  (if-let [err (:jj/error result)]
    [(error state err) []]
    (let [[subject body] (str/split (:jj/description result) #"\n" 2)]
      [(assoc state
              :app/message nil
              :app/mode {:mode/type :describe
                         :mode/change-id change-id
                         :mode/short-id (or (:change/short-id (entry-by-change state change-id)) change-id)
                         :mode/input (input subject "")
                         :mode/body (or body "")})
       []])))

;; ---------------------------------------------------------------------------
;; Repo chooser

(defn- ago [ms]
  (let [minutes (quot ms 60000)]
    (cond
      (< minutes 1) "just now"
      (< minutes 60) (str minutes "m ago")
      (< minutes (* 60 24)) (str (quot minutes 60) "h ago")
      :else (str (quot minutes (* 60 24)) "d ago"))))

(defn- visit-detail [{n :visit/count last-ms :visit/last} now]
  (if n
    (str n (if (= 1 n) " visit · " " visits · ") (ago (- now last-ms)))
    "not opened yet"))

(defn- relative [dir path]
  (let [prefix (str (str/replace dir #"/$" "") "/")]
    (if (str/starts-with? path prefix) (subs path (count prefix)) path)))

(defn chooser
  "The repo chooser for starting outside a jj repo, in `dir`. Offers to set
   jj up in the git repo around it (`git-root`) or else in `dir`, to open the
   jj repo the git repo sits in (`parent`), and the repos under `dir`
   (`nearby`) and up to nine `recent` others (the :nearby and :others of
   jjtui.history/nearby-first). The first nine repos get hotkeys 1-9.
   Outside a git repo the first repo starts selected, so enter doesn't
   create a repo by accident."
  [{:keys [dir git-root parent nearby recent now]}]
  (let [setup (cond-> [(merge {:choice/key "i" :choice/group :setup}
                              (if git-root
                                {:choice/label "Set up jj in this git repo"
                                 :choice/detail (str "jj git init --colocate in " git-root)
                                 :choice/action [:init git-root]}
                                {:choice/label "Create a jj repo here"
                                 :choice/detail (str "jj git init in " dir)
                                 :choice/action [:create dir]}))]
                parent (conj {:choice/key "p"
                              :choice/group :setup
                              :choice/label "Open the jj repo it's inside"
                              :choice/detail parent
                              :choice/action [:open parent]}))
        repos (concat (map vector (repeat :nearby) nearby)
                      (map vector (repeat :recent) (take 9 recent)))]
    {:mode/type :no-repo
     :mode/dir dir
     :mode/git-root git-root
     :mode/selected (if (or git-root (empty? repos)) 0 (count setup))
     :mode/choices
     (into setup
           (map-indexed (fn [i [group {repo :visit/dir :as visit}]]
                          {:choice/key (when (< i 9) (str (inc i)))
                           :choice/group group
                           :choice/label (if (= :nearby group) (relative dir repo) repo)
                           :choice/detail (visit-detail visit now)
                           :choice/action [:open repo]}))
           repos)}))

(defn- choose [state {[op dir] :choice/action}]
  (open-repo state op dir))

(defn- handle-chooser-key [state k]
  (let [{:mode/keys [choices selected]} (:app/mode state)
        clamp #(max 0 (min % (dec (count choices))))]
    (cond
      (#{"q" "escape"} k) [state [{:fx/type :quit}]]
      (#{"j" "down"} k) [(assoc-in state [:app/mode :mode/selected] (clamp (inc selected))) []]
      (#{"k" "up"} k) [(assoc-in state [:app/mode :mode/selected] (clamp (dec selected))) []]
      (= "enter" k) (choose state (get choices selected))
      :else (if-let [choice (first (filter #(= k (:choice/key %)) choices))]
              (choose state choice)
              [state []]))))

(defn- repo-opened
  "Switches to the opened repo: its state starts fresh and its log loads."
  [state {:msg/keys [result]}]
  (let [state (assoc state :app/busy nil)]
    (if-let [err (:jj/error result)]
      [(error state err) []]
      (reload (-> state
                  (assoc :app/repo (repo-state (:app/config state) result) :app/mode nil)
                  (info (str "Opened " (:repo/dir result))))))))

;; ---------------------------------------------------------------------------
;; Keys per mode

(def ^:private nav-actions
  #{:cursor/down :cursor/up :cursor/top :cursor/bottom :cursor/half-down :cursor/half-up
    :cursor/working-copy :preview/down :preview/up :preview/page-down :preview/page-up
    :split/toggle :files/toggle :files/toggle-all :split/shrink :split/grow :help})

(defn- search-action
  "What `k` does to the active search, if there is one."
  [state k]
  (when (get-in state [:app/repo :repo/search])
    (get keys/search-keymap k)))

(defn- handle-pick-key [state k]
  (let [action (get keys/keymap k)
        search-step (#{:search/next :search/prev} (search-action state k))
        rebase? (= :rebase (get-in state [:app/mode :mode/action]))]
    (cond
      (#{"escape" "q"} k) [(info (assoc state :app/mode nil) "Cancelled") []]
      (#{"enter" "o"} k) (finish-pick state "-d")
      (and rebase? (= "a" k)) (finish-pick state "-A")
      (and rebase? (= "b" k)) (finish-pick state "-B")
      (and rebase? (= "s" k))
      [(update-in state [:app/mode :mode/source-flag] {"-r" "-s" "-s" "-b" "-b" "-r"}) []]
      (and rebase? (= "r" k)) (toggle-rebase-source state)
      search-step (perform state search-step)
      (contains? nav-actions action) (perform state action)
      :else [state []])))

(defn- handle-picker-key [state k msg]
  (let [mode (:app/mode state)
        matches (picker-matches mode)
        clamp #(max 0 (min % (dec (count matches))))]
    (cond
      (= "escape" k) [(assoc state :app/mode nil) []]
      (= "enter" k) (if-let [item (get matches (clamp (:mode/selected mode)))]
                      (picker-choice state mode item)
                      [state []])
      (#{"down" "ctrl+n" "ctrl+j"} k) [(assoc-in state [:app/mode :mode/selected] (clamp (inc (:mode/selected mode)))) []]
      (#{"up" "ctrl+p" "ctrl+k"} k) [(assoc-in state [:app/mode :mode/selected] (clamp (dec (:mode/selected mode)))) []]
      :else
      (let [[in _] (ti/text-input-update (:mode/input mode) msg)]
        [(update state :app/mode assoc :mode/input in :mode/selected 0) []]))))

(defn- handle-menu-key [state k]
  (let [mode (:app/mode state)
        option (first (filter #(= k (:option/key %)) (:mode/options mode)))
        flag (:option/toggle option)]
    (cond
      (#{"escape" "q"} k) [(assoc state :app/mode nil) []]
      (nil? option) [state []]
      flag [(update-in state [:app/mode :mode/flags] #(if (contains? % flag) (disj % flag) (conj % flag))) []]
      :else (menu-choice state mode option))))

(defn- handle-search-key
  "Typing jumps to the first match from where the search started, ↑/↓ step
   through the matches, enter makes it the active search and escape goes
   back, leaving the active search as it was."
  [state k msg]
  (let [{:mode/keys [input origin]} (:app/mode state)
        query (str/trim (ti/value input))
        restore #(assoc-in % [:app/repo :repo/cursor] origin)]
    (cond
      (= "escape" k) [(restore (assoc state :app/mode nil)) []]
      (= "enter" k) (let [found? (seq (search-matches state query))]
                      [(cond-> (update-repo (assoc state :app/mode nil) assoc :repo/search (when found? query))
                         (and (not found?) (not (str/blank? query)))
                         (error (str "No change matches " (pr-str query))))
                       []])
      (#{"down" "ctrl+n" "ctrl+j"} k) [(step-search state query 1) []]
      (#{"up" "ctrl+p" "ctrl+k"} k) [(step-search state query -1) []]
      :else
      (let [[in _] (ti/text-input-update input msg)
            state (restore (assoc-in state [:app/mode :mode/input] in))
            from (or (:change/index (some->> origin :change/id (entry-by-change state))) 0)]
        [(goto-entry state (next-match (search-matches state (ti/value in)) from 1 true)) []]))))

(defn- handle-help-key [state k msg]
  (let [search (get-in state [:app/mode :mode/search])
        set-search #(update state :app/mode assoc :mode/search % :mode/offset 0)
        scroll #(update-in state [:app/mode :mode/offset] (fn [o] (max 0 (+ o %))))]
    (cond
      (#{"down" "ctrl+n"} k) [(scroll 1) []]
      (#{"up" "ctrl+p"} k) [(scroll -1) []]

      (ti/focused? search)
      (case k
        "escape" [(set-search (ti/blur (ti/reset search))) []]
        "enter" [(set-search (ti/blur search)) []]
        [(set-search (first (ti/text-input-update search msg))) []])

      (= "/" k) [(set-search (ti/focus search)) []]
      (and (= "escape" k) (seq (ti/value search))) [(set-search (ti/reset search)) []]
      (#{"escape" "q" "?"} k) [(assoc state :app/mode nil) []]
      (= "j" k) [(scroll 1) []]
      (= "k" k) [(scroll -1) []]
      :else [state []])))

(defn- handle-key [state k msg]
  (let [state (assoc state :app/message nil)
        mode (:app/mode state)]
    (case (:mode/type mode)
      :no-repo (handle-chooser-key state k)
      :help (handle-help-key state k msg)
      :pick (handle-pick-key state k)
      :picker (handle-picker-key state k msg)
      :menu (handle-menu-key state k)
      :confirm (cond
                 (#{"y" "enter"} k) (confirmed state (:mode/on-yes mode))
                 (#{"n" "escape" "q"} k) [(info (assoc state :app/mode nil) "Cancelled") []]
                 :else [state []])
      :describe (handle-describe-key state k msg)
      :search (handle-search-key state k msg)
      :input (cond
               (= "escape" k) [(assoc state :app/mode nil) []]
               (= "enter" k) (input-submitted state mode)
               :else (let [[in _] (ti/text-input-update (:mode/input mode) msg)]
                       [(assoc-in state [:app/mode :mode/input] in) []]))
      (if-let [action (or (search-action state k) (get keys/keymap k))]
        (perform state action)
        [state []]))))

;; ---------------------------------------------------------------------------
;; Messages

(defn- first-line [s]
  (first (remove str/blank? (str/split-lines (or s "")))))

(defn- log-loaded [state {:msg/keys [gen result]}]
  (cond
    (not= gen (:app/log-gen state)) [state []]

    (:jj/error result)
    (let [state (-> state (update-repo assoc :repo/loading? false) (error (:jj/error result)))
          {:repo/keys [revset good-revset]} (:app/repo state)]
      (if (not= revset good-revset)
        ;; The new revset failed: go back to the last one that worked
        (reload (update-repo state assoc :repo/revset good-revset))
        [state []]))

    :else
    (let [loaded (set (ids (:log/entries result)))
          first-load? (and (nil? (get-in state [:app/repo :repo/cursor])) (empty? (entries state)))
          state (update-repo state #(-> %
                                        (assoc :repo/log result :repo/loading? false :repo/good-revset (:repo/revset %))
                                        (update :repo/marks (fn [ms] (into #{} (filter loaded) ms)))
                                        (update :repo/expanded (fn [es] (into #{} (filter loaded) es)))))]
      [(if first-load?
         (if-let [wc (first (filter :change/working-copy? (:log/entries result)))]
           (assoc-in state [:app/repo :repo/cursor] (change-cursor wc))
           (set-cursor-index state (first (selectable-indices state))))
         state)
       []])))

(defn- jj-done [state {:msg/keys [effect result]}]
  (let [{:jj/keys [exit out err]} result
        {:fx/keys [success clear-marks?]} effect
        state (assoc state :app/busy nil)
        ok? (zero? exit)
        state (cond
                (not ok?) (error state (str/trim (str err)))
                success (info state success)
                :else (info state (or (first-line err) (first-line out) "Done")))
        state (cond-> state (and ok? clear-marks?) (update-repo assoc :repo/marks #{}))]
    (reload state)))

(defn- outdated?
  "A local bookmark the remote lacks or has elsewhere."
  [bms {bname :bookmark/name :bookmark/keys [remote synced?]}]
  (and (nil? remote)
       (or (not synced?)
           (not-any? #(and (= bname (:bookmark/name %)) (:bookmark/remote %) (:bookmark/tracked? %)) bms))))

(defn- bookmark-picker
  "[title items message-when-empty] of the picker for `purpose`."
  [state purpose bms]
  (case purpose
    :bookmark-set ["Set bookmark (type a new name to create)"
                   (mapv #(bookmark-item % (:bookmark/ref %)) (remove :bookmark/remote bms))
                   nil]
    :branch-diff ["Diff branch" (source-items state bms) nil]
    :push ["Push bookmark"
           (vec (for [b bms :when (outdated? bms b)]
                  (assoc (bookmark-item b (:bookmark/ref b))
                         :item/detail (if (:bookmark/synced? b) "not on remote" "out of sync"))))
           "Every bookmark is pushed"]
    :pull ["Pull bookmark"
           (vec (for [b bms :when (:bookmark/remote b)]
                  (cond-> (bookmark-item b b)
                    (not (:bookmark/tracked? b)) (assoc :item/detail "untracked, pulling tracks it"))))
           "No remote bookmarks"]))

(defn- bookmarks-loaded [state {:msg/keys [purpose result graph]}]
  (if-let [err (or (:jj/error result) (:jj/error graph))]
    [(error state err) []]
    (let [bms (:jj/bookmarks result)
          [title items empty-text] (bookmark-picker state purpose bms)]
      (if (and empty-text (empty? items))
        [(info state empty-text) []]
        [(cond-> (open-picker (assoc state :app/message nil) purpose title items)
           (= :branch-diff purpose) (update :app/mode assoc :mode/bookmarks bms)
           (:jj/graph graph) (update :app/mode assoc :mode/graph (:jj/graph graph)))
         []]))))

(defn- recent-repos-loaded
  "Offers the visited repos, best first; picking one opens it."
  [state {:msg/keys [repos now]}]
  (if (empty? repos)
    [(info state "No other repos visited yet") []]
    [(open-picker (assoc state :app/message nil) :repo "Switch repo"
                  (mapv (fn [{dir :visit/dir :as visit}]
                          {:item/label dir :item/detail (visit-detail visit now) :item/value dir})
                        repos))
     []]))

(defn after-exec
  "Called by the main loop once an interactive command finished."
  [state {:keys [exit]}]
  (let [reload? (get-in state [:app/exec :exec/reload?])
        state (-> (assoc state :app/exec nil)
                  (update-repo assoc :repo/preview-cache {} :repo/preview nil))
        state (if (and exit (not (zero? exit)))
                (error state (str "Command exited with " exit))
                state)]
    (if reload? (reload state) [state []])))

(defn update-state
  "[state msg] -> [state effects]"
  [state msg]
  (let [[state fx]
        (case (:type msg)
          ;; charm's messages
          :key-press (handle-key state (keys/key-str msg) msg)
          :window-size [(assoc state :app/width (:width msg) :app/height (:height msg)) []]
          :environment [(assoc state :app/dark? (boolean (:dark-background? msg))) []]
          :error [(error state (str "Error: " (ex-message (:error msg)))) []]

          ;; jjtui.effects' messages
          :msg/log-loaded (log-loaded state msg)
          :msg/jj-done (jj-done state msg)
          :msg/bookmarks-loaded (bookmarks-loaded state msg)
          :msg/recent-repos-loaded (recent-repos-loaded state msg)
          :msg/description-loaded (description-loaded state msg)
          :msg/repo-opened (repo-opened state msg)
          :msg/diff-resolved (let [result (:msg/result msg)]
                               (if-let [err (:jj/error result)]
                                 [(error state err) []]
                                 (open-diff (assoc state :app/message nil) result)))
          :msg/preview-fetch (let [k (:msg/key msg)]
                               [state (if (= k (get-in state [:app/repo :repo/preview :preview/key]))
                                        [{:fx/type :preview :fx/key k}]
                                        [])])
          :msg/preview-loaded
          (let [{:msg/keys [key lines]} msg
                state (update-repo state update :repo/preview-cache #(assoc (if (> (count %) 64) {} %) key lines))]
            [(if (= key (get-in state [:app/repo :repo/preview :preview/key]))
               (assoc-in state [:app/repo :repo/preview] {:preview/key key :preview/lines lines :preview/offset 0})
               state)
             []])

          [state []])]
    (if (:app/repo state)
      (sync-preview [(scroll-into-view state) fx])
      [state fx])))

(defn start
  "Initial [state effects] for a (re)started program."
  [state]
  (if (:app/repo state) (reload state) [state []]))
