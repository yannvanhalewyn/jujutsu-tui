(ns jjtui.state
  "The app state's schema and read-only queries on it, shared by jjtui.app
   and jjtui.view.

   Every map key is namespaced by the thing it describes (:app/ for the
   session, :repo/ for the open repo, :mode/, :preview/, :change/ …), so the
   same name in two shapes can't be confused. Enum values and function
   options stay unqualified. :app/config is the internal jjtui.config/Config,
   not the user's unqualified config.edn shape."
  (:require
   [charm.components.text-input :as ti]
   [clojure.string :as str]
   [jjtui.config :as config]
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

(def Choice
  "An option of the repo chooser."
  [:map
   [:choice/key {:doc "Hotkey"} :string]
   [:choice/label :string]
   [:choice/detail :string]
   [:choice/action {:doc "[:init git-root] or [:open dir]"} [:tuple [:enum :init :open] :string]]])

(def Mode
  "The modal interaction in progress, on top of normal mode."
  [:multi {:dispatch :mode/type}
   [:help [:map
           [:mode/type [:= :help]]
           [:mode/offset :int]
           [:mode/search {:doc "Filters the bindings; focused while typing"} TextInput]]]
   [:pick [:map
           [:mode/type [:= :pick]]
           [:mode/action [:enum :rebase :squash-into]]
           [:mode/sources {:doc "Change ids"} [:vector :string]]
           [:mode/source-flag [:enum "-r" "-s" "-b"]]]]
   [:picker [:map
             [:mode/type [:= :picker]]
             [:mode/purpose [:enum :diff-tool :branch-diff :bookmark-op :bookmark-set :workspace]]
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
            [:mode/bookmark {:optional true} :string]]]
   [:describe [:map
               [:mode/type [:= :describe]]
               [:mode/change-id :string]
               [:mode/short-id :string]
               [:mode/input {:doc "First line of the description"} TextInput]
               [:mode/body {:doc "The rest of the description, kept as is"} :string]]]
   [:search [:map
             [:mode/type [:= :search]]
             [:mode/input TextInput]
             [:mode/origin {:doc "Cursor to return to on escape"} [:maybe Cursor]]]]
   [:no-repo [:map
              [:mode/type [:= :no-repo]]
              [:mode/git-root {:doc "The git repo without jj that jjtui started in"} :string]
              [:mode/choices [:vector Choice]]
              [:mode/selected :int]]]])

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

(def Repo
  "The open jj repo: everything that's dropped and fetched again when
   switching repos."
  [:map {:closed true}
   [:repo/dir {:doc "Canonical workspace root"} :string]
   [:repo/base-template :string]
   [:repo/revset [:maybe :string]]
   [:repo/good-revset {:doc "Last revset that loaded, to fall back to"} [:maybe :string]]
   [:repo/log log/Log]
   [:repo/loading? :boolean]
   [:repo/cursor [:maybe Cursor]]
   [:repo/cursor-row {:doc "Last cursor row index, for when its change disappears"} :int]
   [:repo/offset {:doc "First visible log row"} :int]
   [:repo/marks [:set :string]]
   [:repo/expanded {:doc "Change ids whose files are shown"} [:set :string]]
   [:repo/search {:doc "Query of the active search: highlighted, n/N step through it"} [:maybe :string]]
   [:repo/preview [:maybe Preview]]
   [:repo/preview-cache [:map-of PreviewKey [:vector :string]]]
   [:repo/ignore-immutable? {:doc "Per repo, so it never carries over by surprise"} :boolean]])

(def State
  "The session (terminal, preferences, modes) and the open repo."
  [:and
   [:map {:closed true}
    [:app/config config/Config]
    [:app/repo {:doc "nil while the user picks one"} [:maybe Repo]]
    [:app/log-gen {:doc "Bumped per load, across repos; stale results are dropped"} :int]
    [:app/show-files? :boolean]
    [:app/split? :boolean]
    [:app/split-ratio number?]
    [:app/diff-tool :keyword]
    [:app/mode [:maybe Mode]]
    [:app/message [:maybe Message]]
    [:app/busy {:doc "Label of the running background command"} [:maybe :string]]
    [:app/width :int]
    [:app/height :int]
    [:app/dark? :boolean]
    [:app/dark-known {:optional true :doc "dark? carried across restarts"} [:maybe :boolean]]
    [:app/exec [:maybe Exec]]]
   [:fn {:error/message "no repo is open only while choosing one"}
    #(or (:app/repo %) (= :no-repo (get-in % [:app/mode :mode/type])))]])

;; ---------------------------------------------------------------------------
;; Log

(defn rows [state] (get-in state [:app/repo :repo/log :log/rows]))
(defn entries [state] (get-in state [:app/repo :repo/log :log/entries]))

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
        {:change/keys [id] :as cursor} (get-in state [:app/repo :repo/cursor])
        find-row (fn [c] (first (keep-indexed (fn [i row] (when (row-matches? state row c) i)) rs)))]
    (or (when cursor (find-row cursor))
        (when id (find-row {:change/id id :file/path nil}))
        (let [idxs (selectable-indices state)]
          (when (seq idxs)
            (or (first (filter #(>= % (get-in state [:app/repo :repo/cursor-row])) idxs))
                (peek idxs)))))))

(defn cursor-row [state]
  (when-let [i (cursor-index state)]
    (get (rows state) i)))

(defn cursor-entry [state]
  (when-let [row (cursor-row state)]
    (row-entry state row)))

;; ---------------------------------------------------------------------------
;; Search

(defn search-matches
  "Indices into :log/entries of the changes matching `query`, in log order: a
   change or commit id starting with it, or a description containing it,
   ignoring case."
  [state query]
  (let [q (str/lower-case (str/trim (str query)))]
    (if (str/blank? q)
      []
      (into []
            (keep (fn [{:change/keys [index id commit-id full-description]}]
                    (when (or (str/starts-with? id q)
                              (str/starts-with? commit-id q)
                              (str/includes? (str/lower-case full-description) q))
                      index)))
            (entries state)))))

(defn search-query
  "The query to highlight: the one being typed, else the active search."
  [state]
  (if (= :search (get-in state [:app/mode :mode/type]))
    (str/trim (ti/value (get-in state [:app/mode :mode/input])))
    (get-in state [:app/repo :repo/search])))

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
