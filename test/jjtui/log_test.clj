(ns jjtui.log-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [jjtui.log :as log]))

(defn marker
  "The hidden fields jj prints for a change, as the template produces them."
  [{:keys [id commit parents wc? immutable? bookmarks description]
    :or {parents [] bookmarks []}}]
  (str "\u001d"
       (str/join "\u001f" [id (subs id 0 4) commit (str/join "," parents)
                           (str (boolean immutable?)) "false" "false" "false" (str (boolean wc?))
                           (str/join " " bookmarks) (or description "")])
       "\u001e"))

(defn file-line [prefix status path & [source]]
  (str prefix "\u001c" status "\u001f" (or source path) "\u001f" path))

(def sample
  (str/join "\n"
            [(str "@  " (marker {:id "aaaa1111" :commit "c1" :parents ["c2"] :wc? true}) "aaaa1111 me 12:00 c1")
             "│  (no description set)"
             (str "○  " (marker {:id "bbbb2222" :commit "c2" :parents ["c3"] :bookmarks ["feat" "wip"] :description "Add thing"}) "bbbb2222 me 11:00 feat c2")
             "│  Add thing"
             (file-line "│  " "modified" "src/a.clj")
             (file-line "│  " "renamed" "new.md" "old.md")
             "~  (elided revisions)"
             (str "◆  " (marker {:id "cccc3333" :commit "c3" :immutable? true}) "cccc3333 main c3")]))

(deftest parse-test
  (let [{:keys [entries rows]} (log/parse sample)]
    (testing "one entry per change, with its fields"
      (is (= ["aaaa1111" "bbbb2222" "cccc3333"] (map :change-id entries)))
      (is (:working-copy? (first entries)))
      (is (= ["feat" "wip"] (:bookmarks (second entries))))
      (is (= ["c3"] (:parents (second entries))))
      (is (= "Add thing" (:description (second entries))))
      (is (:immutable? (nth entries 2)))
      (is (= [] (:parents (nth entries 2)))))

    (testing "markers are removed from the displayed node line"
      (is (= "@  aaaa1111 me 12:00 c1" (:line (first rows))))
      (is (not-any? #(and (:line %) (re-find #"[\u001c-\u001f]" (:line %))) rows)))

    (testing "rows are classified and attached to their change"
      (is (= [:node :text :node :text :file :file :other :node] (map :kind rows)))
      (is (= [0 0 1 1 1 1 nil 2] (map :entry rows))))

    (testing "file rows keep the graph prefix and rename source"
      (let [[modified renamed] (filter #(= :file (:kind %)) rows)]
        (is (= "│  " (:prefix modified)))
        (is (= {:status "M" :path "src/a.clj" :source nil} (:file modified)))
        (is (= {:status "R" :path "new.md" :source "old.md"} (:file renamed)))))))

(deftest template-test
  (testing "file lines only for the expanded changes"
    (let [t (log/template {:base "builtin_log_compact" :files (log/files-revset #{"b" "a"})})]
      (is (str/includes? t "self.contained_in(\"change_id(a) | change_id(b)\")"))))
  (testing "no file lines when nothing is expanded"
    (is (not (str/includes? (log/template {:base "x" :files nil}) "diff()")))))
