(ns jjtui.state
  "The app state's schema and read-only queries on it, shared by jjtui.app
   and jjtui.view.

   Top-level state keys live in the :app namespace so they can't be confused
   with the same names inside messages, effects, modes or diff targets."
  (:require
   [charm.components.text-input :as ti]
   [clojure.string :as str]
   [jjtui.log :as log]))

;; ---------------------------------------------------------------------------
;; Schema (malli, plain data: validated in the tests, not at runtime)

(def Cursor
  "Where the cursor is, by identity rather than row index, so it survives
   reloads that shift rows."
  [:map
   [:change-id :string]
   [:file [:maybe :string]]])

(def TextInput [:map [:type [:= :text-input]]])

(def PickerItem
  [:map
   [:label :string]
   [:value any?]
   [:create? {:optional true} :boolean]])

(def MenuOption
  [:map
   [:key :string]
   [:label :string]
   [:toggle {:optional true} :keyword]])

(def Mode
  "The modal interaction in progress, on top of normal mode."
  [:multi {:dispatch :type}
   [:help [:map [:type [:= :help]] [:offset :int]]]
   [:pick [:map
           [:type [:= :pick]]
           [:action [:enum :rebase :squash-into]]
           [:sources [:vector :string]]
           [:source-flag [:enum "-r" "-s" "-b"]]]]
   [:picker [:map
             [:type [:= :picker]]
             [:purpose [:enum :diff-tool :branch-diff :bookmark-op :bookmark-set]]
             [:title :string]
             [:items [:vector PickerItem]]
             [:selected :int]
             [:input TextInput]
             [:op {:optional true} [:enum :delete :rename]]]]
   [:menu [:map
           [:type [:= :menu]]
           [:purpose [:enum :new :bookmark]]
           [:title :string]
           [:options [:vector MenuOption]]
           [:flags [:set :keyword]]
           [:entry log/Entry]]]
   [:confirm [:map
              [:type [:= :confirm]]
              [:prompt [:vector :string]]
              [:on-yes [:or
                        [:tuple [:= :abandon] [:vector :string]]
                        [:tuple [:= :bookmark-delete] :string]]]]]
   [:input [:map
            [:type [:= :input]]
            [:purpose [:enum :revset :rename]]
            [:title :string]
            [:input TextInput]
            [:bookmark {:optional true} :string]]]])

(def PreviewKey
  "[argv width] of the preview command, which identifies its output."
  [:tuple [:vector :string] :int])

(def Preview
  [:map
   [:key PreviewKey]
   [:lines [:maybe [:vector :string]]]
   [:offset :int]
   [:loading? {:optional true} :boolean]])

(def Exec
  "An interactive command to run once the TUI has stopped."
  [:map
   [:argv [:vector :string]]
   [:wait? [:maybe :boolean]]
   [:reload? :boolean]])

(def Message
  [:map
   [:text :string]
   [:level [:enum :info :error]]])

(def State
  [:map {:closed true}
   [:app/dir :string]
   [:app/config [:map]]
   [:app/base-template :string]
   [:app/revset [:maybe :string]]
   [:app/good-revset {:doc "Last revset that loaded, to fall back to"} [:maybe :string]]
   [:app/log log/Log]
   [:app/log-gen {:doc "Bumped per load; stale results are dropped"} :int]
   [:app/loading? :boolean]
   [:app/cursor [:maybe Cursor]]
   [:app/cursor-row {:doc "Last cursor row index, for when its change disappears"} :int]
   [:app/offset {:doc "First visible log row"} :int]
   [:app/marks [:set :string]]
   [:app/expanded {:doc "Change ids whose files are shown"} [:set :string]]
   [:app/show-files? :boolean]
   [:app/split? :boolean]
   [:app/split-ratio number?]
   [:app/preview [:maybe Preview]]
   [:app/preview-cache [:map-of PreviewKey [:vector :string]]]
   [:app/ignore-immutable? :boolean]
   [:app/diff-tool :keyword]
   [:app/mode [:maybe Mode]]
   [:app/message [:maybe Message]]
   [:app/busy {:doc "Label of the running background jj command"} [:maybe :string]]
   [:app/width :int]
   [:app/height :int]
   [:app/dark? :boolean]
   [:app/dark-known {:optional true :doc "dark? carried across restarts"} [:maybe :boolean]]
   [:app/exec [:maybe Exec]]])

;; ---------------------------------------------------------------------------
;; Log

(defn rows [state] (get-in state [:app/log :rows]))
(defn entries [state] (get-in state [:app/log :entries]))

;; ---------------------------------------------------------------------------
;; Cursor

(defn row-cursor [state row]
  (when row
    {:change-id (:change-id (get (entries state) (:entry row)))
     :file (get-in row [:file :path])}))

(defn selectable-indices [state]
  (into [] (keep-indexed (fn [i row] (when (log/selectable? row) i))) (rows state)))

(defn- row-matches? [state row {:keys [change-id file]}]
  (and (log/selectable? row)
       (= change-id (:change-id (get (entries state) (:entry row))))
       (= file (get-in row [:file :path]))))

(defn cursor-index
  "Index of the cursor's row: the cursor's change (and file), else that
   change's first line, else the selectable row nearest to where the cursor
   was."
  [state]
  (let [rs (rows state)
        {:keys [change-id] :as cursor} (:app/cursor state)
        find-row (fn [c] (first (keep-indexed (fn [i row] (when (row-matches? state row c) i)) rs)))]
    (or (when cursor (find-row cursor))
        (when change-id (find-row {:change-id change-id :file nil}))
        (let [idxs (selectable-indices state)]
          (when (seq idxs)
            (or (first (filter #(>= % (:app/cursor-row state)) idxs))
                (peek idxs)))))))

(defn cursor-row [state]
  (when-let [i (cursor-index state)]
    (get (rows state) i)))

(defn cursor-entry [state]
  (when-let [row (cursor-row state)]
    (get (entries state) (:entry row))))

;; ---------------------------------------------------------------------------
;; Pickers

(defn fuzzy-score
  "Score of `query` against `text`, lower is better; nil when the characters
   of query don't all appear in order."
  [query text]
  (let [q (str/lower-case query)
        t (str/lower-case text)]
    (if (str/blank? q)
      0
      (if-let [idx (str/index-of t q)]
        idx
        (loop [qi 0 ti 0 gaps 0 last-match -1]
          (cond
            (= qi (count q)) (+ 1000 gaps)
            (>= ti (count t)) nil
            (= (nth q qi) (nth t ti))
            (recur (inc qi) (inc ti) (if (neg? last-match) gaps (+ gaps (- ti last-match 1))) ti)
            :else (recur qi (inc ti) gaps last-match)))))))

(defn picker-matches
  "The picker's visible items: matching items, best first. The bookmark picker
   also offers to create the typed name, after any matches."
  [{:keys [items input purpose]}]
  (let [query (str/trim (ti/value input))
        matched (->> items
                     (keep (fn [item]
                             (when-let [s (fuzzy-score query (:label item))]
                               [s item])))
                     (sort-by first)
                     (mapv second))]
    (if (and (= :bookmark-set purpose)
             (not (str/blank? query))
             (not-any? #(= query (:value %)) items))
      (conj matched {:label (str "+ create " query) :value query :create? true})
      matched)))

