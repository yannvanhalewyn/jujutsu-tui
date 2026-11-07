(ns jjtui.log
  "Builds the `jj log` template and parses its output into display rows.

   jj renders the graph. The template prepends hidden fields to every change's
   first line, between \\x1d and \\x1e, and appends one line per changed file
   starting with \\x1c for changes whose file list is shown. jj draws graph
   edges next to those lines too, so the graph stays intact."
  (:require
   [clojure.string :as str]))

(def ^:private entry-start "\u001d")
(def ^:private field-sep "\u001f")
(def ^:private file-start "\u001c")

(def ^:private marker-pattern #"\u001d([^\u001e]*)\u001e")

(def ^:private entry-fields
  "Fields of the hidden marker, in order, with their template expressions."
  [[:change/id "change_id"]
   [:change/short-id "change_id.shortest(8)"]
   [:change/commit-id "commit_id"]
   [:change/parents "parents.map(|c| c.commit_id()).join(\",\")"]
   [:change/immutable? "immutable"]
   [:change/empty? "empty"]
   [:change/conflict? "conflict"]
   [:change/divergent? "divergent"]
   [:change/working-copy? "current_working_copy"]
   [:change/bookmarks "local_bookmarks.map(|b| b.name()).join(\" \")"]
   [:change/description "description.first_line()"]])

;; ---------------------------------------------------------------------------
;; Schemas (malli, plain data: validated in the tests, not at runtime)

(def Entry
  "A change, parsed from the hidden marker."
  [:map
   [:change/index {:doc "Position in :log/entries"} :int]
   [:change/id :string]
   [:change/short-id :string]
   [:change/commit-id :string]
   [:change/parents {:doc "Commit ids"} [:vector :string]]
   [:change/immutable? :boolean]
   [:change/empty? :boolean]
   [:change/conflict? :boolean]
   [:change/divergent? :boolean]
   [:change/working-copy? :boolean]
   [:change/bookmarks [:vector :string]]
   [:change/description :string]])

(def FileChange
  [:map
   [:file/status [:enum "M" "A" "D" "R" "C" "?"]]
   [:file/path :string]
   [:file/source {:doc "Path before a rename or copy"} [:maybe :string]]])

(def Row
  "A display line. :row/entry indexes into :log/entries."
  [:multi {:dispatch :row/kind}
   [:node [:map [:row/kind [:= :node]] [:row/entry :int] [:row/line :string]]]
   [:text [:map [:row/kind [:= :text]] [:row/entry :int] [:row/line :string]]]
   [:file [:map [:row/kind [:= :file]] [:row/entry :int] [:row/prefix :string] [:row/file FileChange]]]
   [:other [:map [:row/kind [:= :other]] [:row/line :string]]]])

(def Log
  "What `parse` returns."
  [:map
   [:log/entries [:vector Entry]]
   [:log/rows [:vector Row]]])

(defn files-revset
  "Revset matching the changes whose file list is expanded."
  [change-ids]
  (when (seq change-ids)
    (str/join " | " (map #(str "change_id(" % ")") (sort change-ids)))))

(defn template
  "The full log template. `base` is the user's log template (e.g.
   builtin_log_compact). `files` is :all, nil, or a revset string."
  [{:keys [base files]}]
  (let [marker (str "stringify(\"\\x1d\" ++ "
                    (str/join " ++ \"\\x1f\" ++ " (map second entry-fields))
                    " ++ \"\\x1e\")")
        file-lines (str "self.diff().files().map(|f| \"\\x1c\" ++ f.status() ++ \"\\x1f\" ++ "
                        "f.source().path() ++ \"\\x1f\" ++ f.path() ++ \"\\n\").join(\"\")")
        files-expr (cond
                     (= :all files) file-lines
                     (string? files) (str "if(self.contained_in(" (pr-str files) "), " file-lines ")")
                     :else nil)]
    (str marker " ++ (" base ")"
         (when files-expr (str " ++ " files-expr)))))

(defn- parse-bool [s] (= "true" s))

(defn- parse-entry-fields [s]
  (let [values (str/split s (re-pattern field-sep) -1)
        m (zipmap (map first entry-fields) values)]
    (-> m
        (update :change/parents #(if (str/blank? %) [] (str/split % #",")))
        (update :change/bookmarks #(if (str/blank? %) [] (str/split % #" ")))
        (update :change/immutable? parse-bool)
        (update :change/empty? parse-bool)
        (update :change/conflict? parse-bool)
        (update :change/divergent? parse-bool)
        (update :change/working-copy? parse-bool))))

(def ^:private status-letters
  {"modified" "M" "added" "A" "removed" "D" "renamed" "R" "copied" "C"})

(defn- file-row [line entry]
  (let [idx (str/index-of line file-start)
        [status source path] (str/split (subs line (inc idx)) (re-pattern field-sep) -1)]
    {:row/kind :file
     :row/entry entry
     :row/prefix (subs line 0 idx)
     :row/file {:file/status (get status-letters status "?")
                :file/path path
                :file/source (when (not= source path) source)}}))

(defn parse
  "Parses `jj log` output (with the template above) into a Log.

   Rows have :row/kind :node (first line of a change), :text (other lines of
   a change), :file (a file of a change) or :other (anything outside a
   change), the :row/entry index, and :row/line, the text to display. File
   rows carry :row/file and :row/prefix, the graph to the left of the file."
  [output]
  (loop [[line & more :as lines] (str/split-lines (or output ""))
         entries []
         rows []]
    (if (empty? lines)
      {:log/entries entries :log/rows rows}
      (let [current (when (seq entries) (dec (count entries)))
            other {:row/kind :other :row/line line}]
        (cond
          (str/includes? line entry-start)
          (if-let [[whole fields] (re-find marker-pattern line)]
            (let [entry (assoc (parse-entry-fields fields) :change/index (count entries))]
              (recur more
                     (conj entries entry)
                     (conj rows {:row/kind :node
                                 :row/entry (:change/index entry)
                                 :row/line (str/replace-first line whole "")})))
            (recur more entries (conj rows other)))

          (and current (str/includes? line file-start))
          (recur more entries (conj rows (file-row line current)))

          (str/includes? line "(elided revisions)")
          (recur more entries (conj rows other))

          current
          (recur more entries (conj rows {:row/kind :text :row/entry current :row/line line}))

          :else
          (recur more entries (conj rows other)))))))

(defn selectable? [row]
  (contains? #{:node :file} (:row/kind row)))
