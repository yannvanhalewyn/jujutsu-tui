(ns jjtui.app
  "The application state machine.

   `update-state` is pure: it takes the state and a message and returns
   [state effects], where effects are data maps (see jjtui.effects). Keeping
   I/O out of here makes every flow testable without a terminal or a repo."
  (:require
   [charm.components.text-input :as ti]
   [clojure.string :as str]
   [jjtui.config :as config]
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
   :repo/revset (:revset config)
   :repo/good-revset (:revset config)
   :repo/log {:log/entries [] :log/rows []}
   :repo/loading? true
   :repo/cursor nil
   :repo/cursor-row 0
   :repo/offset 0
   :repo/marks #{}
   :repo/expanded #{}
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
   :app/show-files? (boolean (:show-files config))
   :app/split? (boolean (:split config))
   :app/split-ratio (:split-ratio config)
   :app/diff-tool (:diff-tool config)
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
  "Runs a non-interactive jj command in the background, reloading after."
  [state args {:keys [label success clear-marks?]}]
  (if (:app/busy state)
    [(error state (str "Busy: " (:app/busy state))) []]
    [(assoc state :app/busy (or label (str "jj " (first args))))
     [{:fx/type :jj :fx/args args :fx/success success :fx/clear-marks? clear-marks?}]]))

(defn- run-exec
  "Suspends the TUI to run `argv` in the terminal."
  [state argv & {:keys [wait? reload? clear-marks?] :or {reload? true}}]
  [(cond-> (assoc state :app/exec {:exec/argv argv :exec/wait? wait? :exec/reload? reload?})
     clear-marks? (update-repo assoc :repo/marks #{}))
   [{:fx/type :quit}]])

;; ---------------------------------------------------------------------------
;; Diffs

(defn- tool-spec [state]
  (config/tool-spec (get-in state [:app/config :diff-tools (:app/diff-tool state)])))

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

(defn- branch-target [bookmark-ref]
  {:target/revset (str "fork_point(trunk() | " bookmark-ref ").." bookmark-ref)
   :target/files []
   :target/from-rev (str "fork_point(trunk() | " bookmark-ref ")")
   :target/from-commit? true
   :target/to-rev bookmark-ref})

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
  (let [{:keys [cmd wait?]} (tool-spec state)
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
    (let [cmd (get-in state [:app/config :preview-command])
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
    :search [(assoc state :app/mode {:mode/type :search
                                     :mode/input (input "" "/")
                                     :mode/origin (get-in state [:app/repo :repo/cursor])})
             []]
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
    :diff/branch [(info state "Loading bookmarks…") [{:fx/type :bookmarks :fx/purpose :branch-diff}]]
    :diff/pick-tool
    [(open-picker state :diff-tool "Diff tool"
                  (vec (for [k (sort (keys (get-in state [:app/config :diff-tools])))]
                         {:item/label (str (name k) (when (= k (:app/diff-tool state)) "  (current)"))
                          :item/value k})))
     []]

    :mark/toggle
    (if-let [entry (cursor-entry state)]
      (let [id (:change/id entry)
            state (update-in state [:app/repo :repo/marks] #(if (contains? % id) (disj % id) (conj % id)))]
        ;; Step to the next change, skipping its files
        [(if-let [next-node (first (filter #(= :node (:row/kind (get (rows state) %)))
                                           (range (inc (or (cursor-index state) 0)) (count (rows state)))))]
           (set-cursor-index state next-node)
           state)
         []])
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
    :rebase (start-pick state :rebase (targets state))
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
    :push-new (let [ts (targets state)]
                (run-jj state (into ["git" "push"] (mapcat (fn [id] ["-c" id]) (ids ts))) {:label "Pushing"}))
    :fetch (run-jj state ["git" "fetch"] {:label "Fetching"})

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

(defn- picker-choice [state {:mode/keys [purpose op]} {:item/keys [value create?]}]
  (let [state (assoc state :app/mode nil)]
    (case purpose
      :diff-tool [(info (assoc state :app/diff-tool value) (str "Diff tool: " (name value))) []]
      :branch-diff (open-diff state (branch-target value))
      :bookmark-op (bookmark-chosen state op value)
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

(defn chooser
  "The repo chooser for a git repo without jj: set jj up in it, open the jj
   repo it sits in (`parent`), or one of the `recent` repos
   (jjtui.history/suggestions)."
  [{:keys [git-root parent recent now]}]
  {:mode/type :no-repo
   :mode/git-root git-root
   :mode/selected 0
   :mode/choices
   (into (cond-> [{:choice/key "i"
                   :choice/label "Set up jj in this git repo"
                   :choice/detail (str "jj git init --colocate in " git-root)
                   :choice/action [:init git-root]}]
           parent (conj {:choice/key "p"
                         :choice/label "Open the jj repo it's inside"
                         :choice/detail parent
                         :choice/action [:open parent]}))
         (map-indexed (fn [i {dir :visit/dir n :visit/count last-ms :visit/last}]
                        {:choice/key (str (inc i))
                         :choice/label dir
                         :choice/detail (str n (if (= 1 n) " visit · " " visits · ") (ago (- now last-ms)))
                         :choice/action [:open dir]}))
         (take 9 recent))})

(defn- choose [state {[op dir] :choice/action}]
  (if (:app/busy state)
    [state []]
    [(assoc state :app/busy (if (= :init op) (str "Setting up jj in " dir) (str "Opening " dir)))
     [{:fx/type (if (= :init op) :init-repo :open-repo) :fx/dir dir}]]))

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

(defn- handle-pick-key [state k]
  (let [action (get keys/keymap k)
        rebase? (= :rebase (get-in state [:app/mode :mode/action]))]
    (cond
      (#{"escape" "q"} k) [(info (assoc state :app/mode nil) "Cancelled") []]
      (#{"enter" "o"} k) (finish-pick state "-d")
      (and rebase? (= "a" k)) (finish-pick state "-A")
      (and rebase? (= "b" k)) (finish-pick state "-B")
      (and rebase? (= "s" k))
      [(update-in state [:app/mode :mode/source-flag] {"-r" "-s" "-s" "-b" "-b" "-r"}) []]
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

(defn- handle-search-key
  "Typing jumps to the first match from where the search started, ↑/↓ step
   through the matches, enter keeps the cursor there and escape goes back."
  [state k msg]
  (let [{:mode/keys [input origin]} (:app/mode state)
        query (ti/value input)
        restore #(assoc-in % [:app/repo :repo/cursor] origin)
        step #(goto-entry state (next-match (search-matches state query)
                                            (or (:change/index (cursor-entry state)) 0) % false))]
    (cond
      (= "escape" k) [(restore (assoc state :app/mode nil)) []]
      (= "enter" k) [(cond-> (assoc state :app/mode nil)
                       (and (not (str/blank? query)) (empty? (search-matches state query)))
                       (error (str "No change matches " (pr-str (str/trim query)))))
                     []]
      (#{"down" "ctrl+n" "ctrl+j"} k) [(step 1) []]
      (#{"up" "ctrl+p" "ctrl+k"} k) [(step -1) []]
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
      (if-let [action (get keys/keymap k)]
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

(defn- bookmarks-loaded [state {:msg/keys [purpose result]}]
  (if-let [err (:jj/error result)]
    [(error state err) []]
    (let [set? (= :bookmark-set purpose)
          bms (cond->> (:jj/bookmarks result) set? (remove :bookmark/remote))
          items (vec (for [{:bookmark/keys [ref commit description]} bms]
                       {:item/label (str ref "  " commit "  " description)
                        :item/value ref}))]
      [(-> (assoc state :app/message nil)
           (open-picker purpose (if set? "Set bookmark (type a new name to create)" "Diff branch") items))
       []])))

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
