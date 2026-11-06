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
                        row-cursor rows selectable-indices]]
   [jjtui.view :as view]))

;; ---------------------------------------------------------------------------
;; State

(defn init-state
  [{:keys [dir config revset base-template config-error]}]
  {:app/dir dir
   :app/config config
   :app/base-template base-template
   :app/revset (or revset (:revset config))
   :app/good-revset (or revset (:revset config))
   :app/log {:entries [] :rows []}
   :app/log-gen 0
   :app/loading? true
   :app/cursor nil
   :app/cursor-row 0
   :app/offset 0
   :app/marks #{}
   :app/expanded #{}
   :app/show-files? (boolean (:show-files config))
   :app/split? (boolean (:split config))
   :app/split-ratio (:split-ratio config)
   :app/preview nil
   :app/preview-cache {}
   :app/ignore-immutable? false
   :app/diff-tool (:diff-tool config)
   :app/mode nil
   :app/message (when config-error {:text config-error :level :error})
   :app/busy nil
   :app/width 80
   :app/height 24
   :app/dark? true
   :app/exec nil})

(defn- message [state text level]
  (assoc state :app/message {:text text :level level}))

(defn- info [state text] (message state text :info))
(defn- error [state text] (message state text :error))

(defn- set-cursor-index [state i]
  (if-let [row (get (rows state) i)]
    (assoc state :app/cursor (row-cursor state row) :app/cursor-row i)
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
  (let [entry (:entry (get rs i))]
    (loop [j i]
      (let [next-row (get rs (inc j))]
        (if (and next-row (= entry (:entry next-row)) (not= :node (:kind next-row)))
          (recur (inc j))
          j)))))

(defn scroll-into-view
  "Adjusts :app/offset so the cursor's change is visible, keeping a small margin."
  [state]
  (let [h (log-height state)
        rs (rows state)
        idx (cursor-index state)]
    (if (or (nil? idx) (empty? rs))
      (assoc state :app/offset 0)
      (let [hi (min (block-end rs idx) (+ idx h -1))
            margin (min 2 (quot (dec h) 4))
            off (:app/offset state)
            off (if (> (+ hi margin) (+ off h -1)) (- (+ hi margin) (dec h)) off)
            off (if (< (- idx margin) off) (- idx margin) off)]
        (assoc state :app/offset (max 0 (min off (- (count rs) h))))))))

;; ---------------------------------------------------------------------------
;; Selection

(defn marked-entries
  "Marked changes in log order (newest first)."
  [state]
  (filter #(contains? (:app/marks state) (:change-id %)) (entries state)))

(defn- targets
  "The changes an action applies to: the marks, else the cursor's change."
  [state]
  (let [marked (marked-entries state)]
    (if (seq marked)
      (vec marked)
      (if-let [e (cursor-entry state)] [e] []))))

(defn- ids [es] (mapv :change-id es))

(defn- describe-entries [es]
  (str/join ", " (map :short-id es)))

(defn- ignore-immutable [state args]
  (cond-> args (:app/ignore-immutable? state) (conj "--ignore-immutable")))

;; ---------------------------------------------------------------------------
;; Effects

(defn- log-params [state]
  {:revset (:app/revset state)
   :base-template (:app/base-template state)
   :files (cond
            (:app/show-files? state) :all
            (seq (:app/expanded state)) (log/files-revset (:app/expanded state))
            :else nil)})

(defn reload
  "State and effect for reloading the log."
  [state]
  (let [gen (inc (:app/log-gen state))]
    [(assoc state :app/log-gen gen :app/loading? true)
     [{:fx :load-log :gen gen :params (log-params state)}]]))

(defn- run-jj
  "Runs a non-interactive jj command in the background, reloading after."
  [state args {:keys [label success clear-marks?]}]
  (if (:app/busy state)
    [(error state (str "Busy: " (:app/busy state))) []]
    [(assoc state :app/busy (or label (str "jj " (first args))))
     [{:fx :jj :args args :success success :clear-marks? clear-marks?}]]))

(defn- run-exec
  "Suspends the TUI to run `argv` in the terminal."
  [state argv & {:keys [wait? reload? clear-marks?] :or {reload? true}}]
  [(cond-> (assoc state :app/exec {:argv argv :wait? wait? :reload? reload?})
     clear-marks? (assoc :app/marks #{}))
   [{:fx :quit}]])

;; ---------------------------------------------------------------------------
;; Diffs

(defn- tool-spec [state]
  (config/tool-spec (get-in state [:app/config :diff-tools (:app/diff-tool state)])))

(defn- diff-target
  "What the diff applies to: a file of the cursor change, the marks, or the
   cursor change. Returns {:revset :files :from-rev :to-rev :from-sha :to-sha}
   with the shas filled in when they're known without asking jj."
  [state]
  (let [row (cursor-row state)
        entry (cursor-entry state)
        marked (marked-entries state)]
    (cond
      (and (seq marked) (not= :file (:kind row)))
      (let [revset (str/join " | " (map :commit-id marked))]
        {:revset revset
         :files []
         :from-rev (str "roots(" revset ")")
         :to-rev (str "heads(" revset ")")
         :title (str (count marked) " marked changes")})

      entry
      (let [file (:file row)]
        {:revset (:commit-id entry)
         :files (if file (vec (distinct (remove nil? [(:source file) (:path file)]))) [])
         :from-sha (or (first (:parents entry)) "")
         :to-sha (:commit-id entry)
         :title (str (:short-id entry) (when file (str " " (:path file))))}))))

(defn- branch-target [bookmark-ref]
  (let [revset (str "fork_point(trunk() | " bookmark-ref ").." bookmark-ref)]
    {:revset revset
     :files []
     :from-rev (str "fork_point(trunk() | " bookmark-ref ")")
     :from-is-commit? true
     :to-rev bookmark-ref
     :title bookmark-ref}))

(defn diff-vars [state target]
  {"revset" (:revset target)
   "files" (:files target)
   "from-sha" (:from-sha target)
   "to-sha" (:to-sha target)
   "width" (str (:app/width state))
   "repo" (:app/dir state)})

(defn- open-diff
  "Opens `target` in the active diff tool, first asking jj for the commit ids
   when the tool needs them and they aren't known yet."
  [state target]
  (let [{:keys [cmd wait?]} (tool-spec state)
        needs-shas? (or (config/uses-var? cmd "from-sha") (config/uses-var? cmd "to-sha"))]
    (cond
      (nil? cmd)
      [(error state (str "No diff tool named " (:app/diff-tool state))) []]

      (and needs-shas? (nil? (:to-sha target)))
      [(info state "Resolving commits…") [{:fx :resolve-diff :target target}]]

      (and needs-shas? (str/blank? (:from-sha target)))
      [(error state "The root commit has no parent to diff against") []]

      :else
      (run-exec state (config/render-command cmd (diff-vars state target))
                :wait? wait? :reload? false))))

;; ---------------------------------------------------------------------------
;; Preview (split diff pane)

(defn preview-width [state]
  (- (:app/width state) (view/log-width state) 1))

(defn- preview-request
  "The preview the cursor wants: {:key :argv}, or nil."
  [state]
  (when-let [target (and (:app/split? state) (cursor-entry state) (diff-target state))]
    (let [cmd (get-in state [:app/config :preview-command])
          width (preview-width state)
          target (if (seq (marked-entries state))
                   ;; The pane follows the cursor, not the marks
                   (let [entry (cursor-entry state)]
                     {:revset (:commit-id entry) :files []
                      :from-sha (or (first (:parents entry)) "") :to-sha (:commit-id entry)})
                   target)
          argv (config/render-command cmd (assoc (diff-vars state target) "width" (str width)))]
      {:key [argv width] :argv argv :width width})))

(defn- sync-preview
  "Points the preview at the cursor: from the cache when possible, otherwise
   after a short debounce so holding j doesn't run jj for every row."
  [[state fx]]
  (let [{:keys [key] :as req} (preview-request state)
        current (get-in state [:app/preview :key])]
    (cond
      (or (nil? req) (= key current)) [state fx]

      (contains? (:app/preview-cache state) key)
      [(assoc state :app/preview {:key key :lines (get-in state [:app/preview-cache key]) :offset 0}) fx]

      :else
      [(assoc state :app/preview {:key key
                                  :lines (get-in state [:app/preview :lines])
                                  :offset 0
                                  :loading? true})
       (conj (vec fx) {:fx :debounce :ms 60 :msg {:type :preview-fetch :key key}})])))

(defn- scroll-preview [state delta]
  (if-not (and (:app/split? state) (:app/preview state))
    state
    (let [n (count (get-in state [:app/preview :lines]))
          h (log-height state)]
      (update-in state [:app/preview :offset]
                 #(max 0 (min (+ (or % 0) delta) (- n h)))))))

;; ---------------------------------------------------------------------------
;; Modes

(defn- input [value prompt]
  (ti/text-input :prompt prompt :value (or value "")))

(defn- open-picker [state purpose title items & {:as extra}]
  (assoc state :app/mode (merge {:type :picker
                                 :purpose purpose
                                 :title title
                                 :items items
                                 :selected 0
                                 :input (input "" "› ")}
                                extra)))

(defn- open-menu [state purpose title options & {:as extra}]
  (assoc state :app/mode (merge {:type :menu :purpose purpose :title title :options options :flags #{}}
                                extra)))

(defn- confirm [state prompt on-yes]
  (assoc state :app/mode {:type :confirm :prompt prompt :on-yes on-yes}))

(defn- start-pick [state action sources]
  (if (empty? sources)
    [(error state "No change at cursor") []]
    [(assoc state :app/mode {:type :pick :action action :sources (ids sources) :source-flag "-r"}) []]))

;; ---------------------------------------------------------------------------
;; Actions

(defn- squash-needs-editor?
  "jj squash opens an editor only when it has to combine two descriptions.
   An unknown parent counts as described, which errs toward the terminal."
  [sources target-entry]
  (let [described? #(not (str/blank? (:description %)))
        target-described? (if target-entry (described? target-entry) true)]
    (> (+ (count (filter described? sources)) (if target-described? 1 0)) 1)))

(defn- squash [state sources target-entry args]
  (let [args (ignore-immutable state args)]
    (if (squash-needs-editor? sources target-entry)
      (run-exec state (into ["jj"] args) :clear-marks? true)
      (run-jj state args {:label "Squashing" :clear-marks? true}))))

(defn- entry-by-commit [state commit-id]
  (first (filter #(= commit-id (:commit-id %)) (entries state))))

(defn- entry-by-change [state change-id]
  (first (filter #(= change-id (:change-id %)) (entries state))))

(defn- rebase [state dest-flag]
  (let [{:keys [sources source-flag]} (:app/mode state)
        target (cursor-entry state)]
    (cond
      (nil? target) [(error state "No change at cursor") []]
      (some #{(:change-id target)} sources) [(error state "Can't rebase a change onto itself") []]
      :else
      (run-jj (assoc state :app/mode nil)
              (ignore-immutable state (-> ["rebase"]
                                          (into (mapcat (fn [id] [source-flag id]) sources))
                                          (into [dest-flag (:change-id target)])))
              {:label "Rebasing" :clear-marks? true}))))

(defn- finish-pick [state dest-flag]
  (let [{:keys [action sources]} (:app/mode state)
        target (cursor-entry state)]
    (case action
      :rebase (rebase state dest-flag)
      :squash-into
      (cond
        (nil? target) [(error state "No change at cursor") []]
        (some #{(:change-id target)} sources) [(error state "Can't squash a change into itself") []]
        :else (squash (assoc state :app/mode nil)
                      (keep #(entry-by-change state %) sources)
                      target
                      ["squash" "--from" (str/join " | " sources) "--into" (:change-id target)])))))

(defn- with-entry [state f]
  (if-let [entry (cursor-entry state)]
    (f entry)
    [(error state "No change at cursor") []]))

(defn- clear-cursor-file
  "Moves the cursor from a file row to its change."
  [state]
  (update state :app/cursor #(some-> % (assoc :file nil))))

(defn- toggle-files [state]
  (if-let [entry (cursor-entry state)]
    (let [id (:change-id entry)
          state (if (:app/show-files? state)
                  ;; Collapsing one change out of "all": expand all the others
                  (assoc state :app/show-files? false
                         :app/expanded (disj (set (map :change-id (entries state))) id))
                  (update state :app/expanded #(if (contains? % id) (disj % id) (conj % id))))]
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
    (if-let [wc (first (filter :working-copy? (entries state)))]
      [(assoc state :app/cursor {:change-id (:change-id wc) :file nil}) []]
      [(error state "The working copy is not in this revset") []])
    :refresh (reload state)
    :help [(assoc state :app/mode {:type :help :offset 0}) []]
    :quit [state [{:fx :quit}]]

    :files/toggle (toggle-files state)
    :files/toggle-all (reload (-> state
                                  (update :app/show-files? not)
                                  (assoc :app/expanded #{})
                                  (clear-cursor-file)))
    :split/toggle [(update state :app/split? not) []]
    :preview/down [(scroll-preview (ensure-split state) 3) []]
    :preview/up [(scroll-preview (ensure-split state) -3) []]
    :preview/page-down [(scroll-preview (ensure-split state) (log-height state)) []]
    :preview/page-up [(scroll-preview (ensure-split state) (- (log-height state))) []]
    :split/shrink [(update state :app/split-ratio #(max 0.2 (- % 0.05))) []]
    :split/grow [(update state :app/split-ratio #(min 0.8 (+ % 0.05))) []]
    :revset/set [(assoc state :app/mode {:type :input
                                         :purpose :revset
                                         :title "Log revset (empty for default)"
                                         :input (input (:app/revset state) "revset › ")})
                 []]

    :diff/open (if-let [target (diff-target state)]
                 (open-diff state target)
                 [(error state "No change at cursor") []])
    :diff/branch [(info state "Loading bookmarks…") [{:fx :bookmarks :purpose :branch-diff}]]
    :diff/pick-tool
    [(open-picker state :diff-tool "Diff tool"
                  (vec (for [k (sort (keys (get-in state [:app/config :diff-tools])))]
                         {:label (str (name k) (when (= k (:app/diff-tool state)) "  (current)"))
                          :value k})))
     []]

    :mark/toggle
    (if-let [entry (cursor-entry state)]
      (let [id (:change-id entry)
            state (update state :app/marks #(if (contains? % id) (disj % id) (conj % id)))]
        ;; Step to the next change, skipping its files
        [(if-let [next-node (first (filter #(= :node (:kind (get (rows state) %)))
                                           (range (inc (or (cursor-index state) 0)) (count (rows state)))))]
           (set-cursor-index state next-node)
           state)
         []])
      [state []])
    :mark/clear [(cond-> (assoc state :app/marks #{})
                   (seq (:app/marks state)) (info "Cleared marks"))
                 []]

    :describe (with-entry state #(run-exec state (ignore-immutable state ["jj" "describe" "-r" (:change-id %)])))
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
                  [(open-menu state :new (str "New change relative to " (:short-id entry))
                              [{:key "a" :label "insert after (-A)"}
                               {:key "b" :label "insert before (-B)"}
                               {:key "e" :label "--no-edit" :toggle :no-edit}]
                              :entry entry)
                   []]))
    :edit (with-entry state #(run-jj state (ignore-immutable state ["edit" (:change-id %)])
                                     {:label "Editing" :success (str "Editing " (:short-id %))}))
    :abandon (let [ts (targets state)]
               (if (empty? ts)
                 [(error state "No change at cursor") []]
                 [(confirm state
                           (into [(str "Abandon " (count ts) " change" (when (> (count ts) 1) "s") "?")]
                                 (map #(str "  " (:short-id %) "  " (if (str/blank? (:description %)) "(no description)" (:description %))) ts))
                           [:abandon (ids ts)])
                  []]))
    :absorb (with-entry state #(run-jj state ["absorb" "--from" (:change-id %)] {:label "Absorbing"}))
    :squash (with-entry state
              (fn [entry]
                (let [marked (remove #(= (:change-id entry) (:change-id %)) (marked-entries state))]
                  (if (seq marked)
                    (squash state marked entry
                            ["squash" "--from" (str/join " | " (ids marked)) "--into" (:change-id entry)])
                    (squash state [entry] (entry-by-commit state (first (:parents entry)))
                            ["squash" "-r" (:change-id entry)])))))
    :squash-into (start-pick state :squash-into (targets state))
    :rebase (start-pick state :rebase (targets state))
    :split-change (with-entry state #(run-exec state (ignore-immutable state ["jj" "split" "-r" (:change-id %)])))
    :undo (run-jj state ["undo"] {:label "Undoing"})
    :redo (run-jj state ["redo"] {:label "Redoing"})
    :toggle-ignore-immutable
    (let [state (update state :app/ignore-immutable? not)]
      [(info state (if (:app/ignore-immutable? state) "--ignore-immutable on" "--ignore-immutable off")) []])

    :bookmark/set (if (cursor-entry state)
                    [(info state "Loading bookmarks…") [{:fx :bookmarks :purpose :bookmark-set}]]
                    [(error state "No change at cursor") []])
    :bookmark/menu
    (with-entry state
      (fn [entry]
        (if (empty? (:bookmarks entry))
          [(error state (str "No bookmarks on " (:short-id entry))) []]
          [(open-menu state :bookmark (str "Bookmarks on " (:short-id entry) ": " (str/join ", " (:bookmarks entry)))
                      [{:key "d" :label "delete"}
                       {:key "r" :label "rename"}]
                      :entry entry)
           []])))
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
    :rename [(assoc state :app/mode {:type :input
                                     :purpose :rename
                                     :title (str "Rename bookmark " bookmark)
                                     :bookmark bookmark
                                     :input (input bookmark "new name › ")})
             []]))

(defn- menu-choice [state {:keys [purpose entry flags]} option]
  (let [state (assoc state :app/mode nil)]
    (case purpose
      :new (run-jj state (ignore-immutable state
                                           (cond-> ["new"]
                                             (contains? flags :no-edit) (conj "--no-edit")
                                             :always (conj (if (= "a" (:key option)) "-A" "-B") (:change-id entry))))
                   {:label "Creating change"
                    :success (str "Created new change " (if (= "a" (:key option)) "after " "before ") (:short-id entry))})
      :bookmark
      (let [op (if (= "d" (:key option)) :delete :rename)
            bms (:bookmarks entry)]
        (if (= 1 (count bms))
          (bookmark-chosen state op (first bms))
          [(open-picker state :bookmark-op (str (name op) " bookmark")
                        (mapv (fn [b] {:label b :value b}) bms)
                        :op op)
           []])))))

(defn- picker-choice [state {:keys [purpose op]} item]
  (let [state (assoc state :app/mode nil)]
    (case purpose
      :diff-tool [(info (assoc state :app/diff-tool (:value item)) (str "Diff tool: " (name (:value item)))) []]
      :branch-diff (open-diff state (branch-target (:value item)))
      :bookmark-op (bookmark-chosen state op (:value item))
      :bookmark-set
      (with-entry state
        (fn [entry]
          (if (:create? item)
            (run-jj state ["bookmark" "create" (:value item) "-r" (:change-id entry)]
                    {:label "Creating bookmark" :success (str "Created " (:value item) " at " (:short-id entry))})
            (run-jj state ["bookmark" "set" (:value item) "-r" (:change-id entry) "--allow-backwards"]
                    {:label "Moving bookmark" :success (str "Moved " (:value item) " to " (:short-id entry))})))))))

(defn- confirmed [state [op arg]]
  (let [state (assoc state :app/mode nil)]
    (case op
      :abandon (run-jj state (ignore-immutable state (into ["abandon"] arg))
                       {:label "Abandoning" :clear-marks? true
                        :success (str "Abandoned " (count arg) " change" (when (> (count arg) 1) "s"))})
      :bookmark-delete (run-jj state ["bookmark" "delete" arg]
                               {:label "Deleting bookmark" :success (str "Deleted bookmark " arg)}))))

(defn- input-submitted [state {:keys [purpose bookmark input]}]
  (let [value (str/trim (ti/value input))
        state (assoc state :app/mode nil)]
    (case purpose
      :revset (reload (assoc state :app/revset (not-empty value) :app/marks #{}))
      :rename (if (or (str/blank? value) (= value bookmark))
                [state []]
                (run-jj state ["bookmark" "rename" bookmark value]
                        {:label "Renaming bookmark" :success (str "Renamed " bookmark " to " value)})))))

;; ---------------------------------------------------------------------------
;; Keys per mode

(def ^:private nav-actions
  #{:cursor/down :cursor/up :cursor/top :cursor/bottom :cursor/half-down :cursor/half-up
    :cursor/working-copy :preview/down :preview/up :preview/page-down :preview/page-up
    :split/toggle :files/toggle :files/toggle-all :split/shrink :split/grow :help})

(defn- handle-pick-key [state k]
  (let [action (get keys/keymap k)
        rebase? (= :rebase (get-in state [:app/mode :action]))]
    (cond
      (#{"escape" "q"} k) [(info (assoc state :app/mode nil) "Cancelled") []]
      (#{"enter" "o"} k) (finish-pick state "-d")
      (and rebase? (= "a" k)) (finish-pick state "-A")
      (and rebase? (= "b" k)) (finish-pick state "-B")
      (and rebase? (= "s" k))
      [(update-in state [:app/mode :source-flag] {"-r" "-s" "-s" "-b" "-b" "-r"}) []]
      (contains? nav-actions action) (perform state action)
      :else [state []])))

(defn- handle-picker-key [state k msg]
  (let [mode (:app/mode state)
        matches (picker-matches mode)
        clamp #(max 0 (min % (dec (count matches))))]
    (cond
      (= "escape" k) [(assoc state :app/mode nil) []]
      (= "enter" k) (if-let [item (get matches (clamp (:selected mode)))]
                      (picker-choice state mode item)
                      [state []])
      (#{"down" "ctrl+n" "ctrl+j"} k) [(assoc-in state [:app/mode :selected] (clamp (inc (:selected mode)))) []]
      (#{"up" "ctrl+p" "ctrl+k"} k) [(assoc-in state [:app/mode :selected] (clamp (dec (:selected mode)))) []]
      :else
      (let [[in _] (ti/text-input-update (:input mode) msg)]
        [(update state :app/mode assoc :input in :selected 0) []]))))

(defn- handle-menu-key [state k]
  (let [mode (:app/mode state)
        option (first (filter #(= k (:key %)) (:options mode)))]
    (cond
      (#{"escape" "q"} k) [(assoc state :app/mode nil) []]
      (nil? option) [state []]
      (:toggle option) [(update-in state [:app/mode :flags]
                                   #(if (contains? % (:toggle option)) (disj % (:toggle option)) (conj % (:toggle option))))
                        []]
      :else (menu-choice state mode option))))

(defn- handle-key [state k msg]
  (let [state (assoc state :app/message nil)
        mode (:app/mode state)]
    (case (:type mode)
      :help (cond
              (#{"escape" "q" "?"} k) [(assoc state :app/mode nil) []]
              (#{"j" "down"} k) [(update-in state [:app/mode :offset] inc) []]
              (#{"k" "up"} k) [(update-in state [:app/mode :offset] #(max 0 (dec %))) []]
              :else [state []])
      :pick (handle-pick-key state k)
      :picker (handle-picker-key state k msg)
      :menu (handle-menu-key state k)
      :confirm (cond
                 (#{"y" "enter"} k) (confirmed state (:on-yes mode))
                 (#{"n" "escape" "q"} k) [(info (assoc state :app/mode nil) "Cancelled") []]
                 :else [state []])
      :input (cond
               (= "escape" k) [(assoc state :app/mode nil) []]
               (= "enter" k) (input-submitted state mode)
               :else (let [[in _] (ti/text-input-update (:input mode) msg)]
                       [(assoc-in state [:app/mode :input] in) []]))
      (if-let [action (get keys/keymap k)]
        (perform state action)
        [state []]))))

;; ---------------------------------------------------------------------------
;; Messages

(defn- first-line [s]
  (first (remove str/blank? (str/split-lines (or s "")))))

(defn- log-loaded [state {:keys [gen result]}]
  (cond
    (not= gen (:app/log-gen state)) [state []]

    (:error result)
    (let [state (-> state (assoc :app/loading? false) (error (:error result)))]
      (if (not= (:app/revset state) (:app/good-revset state))
        ;; The new revset failed: go back to the last one that worked
        (reload (assoc state :app/revset (:app/good-revset state)))
        [state []]))

    :else
    (let [ids (set (map :change-id (:entries result)))
          first-load? (and (nil? (:app/cursor state)) (empty? (entries state)))
          state (-> state
                    (assoc :app/log result :app/loading? false :app/good-revset (:app/revset state))
                    (update :app/marks #(into #{} (filter ids) %))
                    (update :app/expanded #(into #{} (filter ids) %)))]
      [(if first-load?
         (if-let [wc (first (filter :working-copy? (:entries result)))]
           (assoc state :app/cursor {:change-id (:change-id wc) :file nil})
           (set-cursor-index state (first (selectable-indices state))))
         state)
       []])))

(defn- jj-done [state {:keys [fx result]}]
  (let [state (assoc state :app/busy nil)
        ok? (zero? (:exit result))
        state (cond
                (not ok?) (error state (str/trim (str (:err result))))
                (:success fx) (info state (:success fx))
                :else (info state (or (first-line (:err result)) (first-line (:out result)) "Done")))
        state (cond-> state (and ok? (:clear-marks? fx)) (assoc :app/marks #{}))]
    (reload state)))

(defn after-exec
  "Called by the main loop once an interactive command finished."
  [state {:keys [exit]}]
  (let [{:keys [reload?]} (:app/exec state)
        state (assoc state :app/exec nil :app/preview-cache {} :app/preview nil)
        state (if (and exit (not (zero? exit)))
                (error state (str "Command exited with " exit))
                state)]
    (if reload? (reload state) [state []])))

(defn update-state
  "[state msg] -> [state effects]"
  [state msg]
  (let [[state fx]
        (case (:type msg)
          :key-press (handle-key state (keys/key-str msg) msg)
          :window-size [(assoc state :app/width (:width msg) :app/height (:height msg)) []]
          :environment [(assoc state :app/dark? (boolean (:dark-background? msg))) []]
          :log-loaded (log-loaded state msg)
          :jj-done (jj-done state msg)
          :bookmarks-loaded
          (let [{:keys [purpose result]} msg]
            (if (:error result)
              [(error state (:error result)) []]
              (let [bms (:bookmarks result)
                    items (for [b (if (= :bookmark-set purpose) (remove :remote bms) bms)]
                            {:label (str (:ref b) "  " (:commit b) "  " (:description b))
                             :value (:ref b)})]
                [(-> (assoc state :app/message nil)
                     (open-picker purpose (if (= :bookmark-set purpose)
                                            "Set bookmark (type a new name to create)"
                                            "Diff branch")
                                  (vec items)))
                 []])))
          :diff-resolved (if-let [err (:error msg)]
                           [(error state err) []]
                           (open-diff (assoc state :app/message nil) (:target msg)))
          :preview-fetch (if (= (:key msg) (get-in state [:app/preview :key]))
                           [state [{:fx :preview :key (:key msg) :argv (first (:key msg)) :width (second (:key msg))}]]
                           [state []])
          :preview-loaded
          (let [state (update state :app/preview-cache
                              #(assoc (if (> (count %) 64) {} %) (:key msg) (:lines msg)))]
            [(if (= (:key msg) (get-in state [:app/preview :key]))
               (assoc state :app/preview {:key (:key msg) :lines (:lines msg) :offset 0})
               state)
             []])
          :error [(error state (str "Error: " (ex-message (:error msg)))) []]
          [state []])]
    (sync-preview [(scroll-into-view state) fx])))

(defn start
  "Initial [state effects] for a (re)started program."
  [state]
  (reload state))
