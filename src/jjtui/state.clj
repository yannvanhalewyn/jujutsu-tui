(ns jjtui.state
  "The app state's schema and read-only queries on it, shared by jjtui.app
   and jjtui.view.

   Every map key is namespaced by the thing it describes (:app/ for the state
   itself, :mode/, :preview/, :change/ …), so the same name in two shapes
   can't be confused. Enum values and function options stay unqualified."
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
   [:change/id :string]
   [:file/path [:maybe :string]]])

(def TextInput
  "A charm text input; charm owns its keys."
  [:map [:type [:= :text-input]]])

(def PickerItem
  [:map
   [:item/label :string]
   [:item/value any?]
   [:item/create? {:optional true} :boolean]])

(def MenuOption
  [:map
   [:option/key :string]
   [:option/label :string]
   [:option/toggle {:optional true :doc "Flag the key toggles instead of choosing"} :keyword]])

(def Mode
  "The modal interaction in progress, on top of normal mode."
  [:multi {:dispatch :mode/type}
   [:help [:map [:mode/type [:= :help]] [:mode/offset :int]]]
   [:pick [:map
           [:mode/type [:= :pick]]
           [:mode/action [:enum :rebase :squash-into]]
           [:mode/sources {:doc "Change ids"} [:vector :string]]
           [:mode/source-flag [:enum "-r" "-s" "-b"]]]]
   [:picker [:map
             [:mode/type [:= :picker]]
             [:mode/purpose [:enum :diff-tool :branch-diff :bookmark-op :bookmark-set]]
             [:mode/title :string]
             [:mode/items [:vector PickerItem]]
             [:mode/selected :int]
             [:mode/input TextInput]
             [:mode/op {:optional true} [:enum :delete :rename]]]]
   [:menu [:map
           [:mode/type [:= :menu]]
           [:mode/purpose [:enum :new :bookmark]]
           [:mode/title :string]
           [:mode/options [:vector MenuOption]]
           [:mode/flags [:set :keyword]]
           [:mode/entry log/Entry]]]
   [:confirm [:map
              [:mode/type [:= :confirm]]
              [:mode/prompt [:vector :string]]
              [:mode/on-yes [:or
                             [:tuple [:= :abandon] [:vector :string]]
                             [:tuple [:= :bookmark-delete] :string]]]]]
   [:input [:map
            [:mode/type [:= :input]]
            [:mode/purpose [:enum :revset :rename]]
            [:mode/title :string]
            [:mode/input TextInput]
            [:mode/bookmark {:optional true} :string]]]])

(def DiffTarget
  "What a diff applies to. Tools that need commit ids get :target/from-sha
   and :target/to-sha, resolved from the revs first when they aren't known."
  [:map
   [:target/revset :string]
   [:target/files [:vector :string]]
   [:target/from-rev {:optional true} :string]
   [:target/to-rev {:optional true} :string]
   [:target/from-commit? {:optional true :doc "from-rev is the base itself, not its child"} :boolean]
   [:target/from-sha {:optional true :doc "Empty for the root commit"} :string]
   [:target/to-sha {:optional true} :string]])

(def PreviewKey
  "[argv width] of the preview command, which identifies its output."
  [:tuple [:vector :string] :int])

(def Preview
  [:map
   [:preview/key PreviewKey]
   [:preview/lines [:maybe [:vector :string]]]
   [:preview/offset :int]
   [:preview/loading? {:optional true} :boolean]])

(def Exec
  "An interactive command to run once the TUI has stopped."
  [:map
   [:exec/argv [:vector :string]]
   [:exec/wait? [:maybe :boolean]]
   [:exec/reload? :boolean]])

(def Message
  [:map
   [:message/text :string]
   [:message/level [:enum :info :error]]])

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

(defn rows [state] (get-in state [:app/log :log/rows]))
(defn entries [state] (get-in state [:app/log :log/entries]))

(defn row-entry [state row]
  (get (entries state) (:row/entry row)))

;; ---------------------------------------------------------------------------
;; Cursor

(defn row-cursor [state row]
  (when row
    {:change/id (:change/id (row-entry state row))
     :file/path (get-in row [:row/file :file/path])}))

(defn selectable-indices [state]
  (into [] (keep-indexed (fn [i row] (when (log/selectable? row) i))) (rows state)))

(defn- row-matches? [state row cursor]
  (and (log/selectable? row)
       (= cursor (row-cursor state row))))

(defn cursor-index
  "Index of the cursor's row: the cursor's change (and file), else that
   change's first line, else the selectable row nearest to where the cursor
   was."
  [state]
  (let [rs (rows state)
        {:change/keys [id] :as cursor} (:app/cursor state)
        find-row (fn [c] (first (keep-indexed (fn [i row] (when (row-matches? state row c) i)) rs)))]
    (or (when cursor (find-row cursor))
        (when id (find-row {:change/id id :file/path nil}))
        (let [idxs (selectable-indices state)]
          (when (seq idxs)
            (or (first (filter #(>= % (:app/cursor-row state)) idxs))
                (peek idxs)))))))

(defn cursor-row [state]
  (when-let [i (cursor-index state)]
    (get (rows state) i)))

(defn cursor-entry [state]
  (when-let [row (cursor-row state)]
    (row-entry state row)))

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
  [{:mode/keys [items input purpose]}]
  (let [query (str/trim (ti/value input))
        matched (->> items
                     (keep (fn [item]
                             (when-let [s (fuzzy-score query (:item/label item))]
                               [s item])))
                     (sort-by first)
                     (mapv second))]
    (if (and (= :bookmark-set purpose)
             (not (str/blank? query))
             (not-any? #(= query (:item/value %)) items))
      (conj matched {:item/label (str "+ create " query) :item/value query :item/create? true})
      matched)))
