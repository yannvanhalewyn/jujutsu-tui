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
  [[:change-id "change_id"]
   [:short-id "change_id.shortest(8)"]
   [:commit-id "commit_id"]
   [:parents "parents.map(|c| c.commit_id()).join(\",\")"]
   [:immutable? "immutable"]
   [:empty? "empty"]
   [:conflict? "conflict"]
   [:divergent? "divergent"]
   [:working-copy? "current_working_copy"]
   [:bookmarks "local_bookmarks.map(|b| b.name()).join(\" \")"]
   [:description "description.first_line()"]])

;; ---------------------------------------------------------------------------
;; Schemas (malli, plain data: validated in the tests, not at runtime)

(def Entry
  "A change, parsed from the hidden marker."
  [:map
   [:index :int]
   [:change-id :string]
   [:short-id :string]
   [:commit-id :string]
   [:parents [:vector :string]]
   [:immutable? :boolean]
   [:empty? :boolean]
   [:conflict? :boolean]
   [:divergent? :boolean]
   [:working-copy? :boolean]
   [:bookmarks [:vector :string]]
   [:description :string]])

(def FileChange
  [:map
   [:status [:enum "M" "A" "D" "R" "C" "?"]]
   [:path :string]
   [:source [:maybe :string]]])

(def Row
  "A display line. :entry indexes into the log's :entries."
  [:multi {:dispatch :kind}
   [:node [:map [:kind [:= :node]] [:entry :int] [:line :string]]]
   [:text [:map [:kind [:= :text]] [:entry :int] [:line :string]]]
   [:file [:map [:kind [:= :file]] [:entry :int] [:prefix :string] [:file FileChange]]]
   [:other [:map [:kind [:= :other]] [:line :string]]]])

(def Log
  "What `parse` returns."
  [:map
   [:entries [:vector Entry]]
   [:rows [:vector Row]]])

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
        (update :parents #(if (str/blank? %) [] (str/split % #",")))
        (update :bookmarks #(if (str/blank? %) [] (str/split % #" ")))
        (update :immutable? parse-bool)
        (update :empty? parse-bool)
        (update :conflict? parse-bool)
        (update :divergent? parse-bool)
        (update :working-copy? parse-bool))))

(def ^:private status-letters
  {"modified" "M" "added" "A" "removed" "D" "renamed" "R" "copied" "C"})

(defn- parse-file-line [line]
  (let [idx (str/index-of line file-start)
        prefix (subs line 0 idx)
        [status source path] (str/split (subs line (inc idx)) (re-pattern field-sep) -1)]
    {:prefix prefix
     :file {:status (get status-letters status "?")
            :path path
            :source (when (not= source path) source)}}))

(defn parse
  "Parses `jj log` output (with the template above) into
   {:entries [entry] :rows [row]}.

   Rows have :kind :node (first line of a change), :text (other lines of a
   change), :file (a file of a change) or :other (anything outside a change),
   the :entry index, and :line, the text to display. File rows carry :file
   and :prefix, the graph to the left of the file."
  [output]
  (loop [[line & more :as lines] (str/split-lines (or output ""))
         entries []
         rows []]
    (if (empty? lines)
      {:entries entries :rows rows}
      (let [current (when (seq entries) (dec (count entries)))]
        (cond
          (str/includes? line entry-start)
          (if-let [[whole fields] (re-find marker-pattern line)]
            (let [entry (assoc (parse-entry-fields fields) :index (count entries))]
              (recur more
                     (conj entries entry)
                     (conj rows {:kind :node
                                 :entry (:index entry)
                                 :line (str/replace-first line whole "")})))
            (recur more entries (conj rows {:kind :other :line line})))

          (and current (str/includes? line file-start))
          (let [{:keys [prefix file]} (parse-file-line line)]
            (recur more entries (conj rows {:kind :file :entry current :prefix prefix :file file})))

          (str/includes? line "(elided revisions)")
          (recur more entries (conj rows {:kind :other :line line}))

          current
          (recur more entries (conj rows {:kind :text :entry current :line line}))

          :else
          (recur more entries (conj rows {:kind :other :line line})))))))

(defn selectable? [row]
  (contains? #{:node :file} (:kind row)))
