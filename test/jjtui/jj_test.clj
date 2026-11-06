(ns jjtui.jj-test
  "Runs the templates against a real jj, so a jj release that changes the
   template language fails here rather than in the TUI."
  (:require
   [babashka.fs :as fs]
   [clojure.test :refer [deftest is testing]]
   [jjtui.demo :as demo]
   [jjtui.jj :as jj]))

(deftest real-repo-test
  (fs/with-temp-dir [tmp {}]
    (let [dir (demo/create! (str (fs/path tmp "repo")))
          base (jj/base-template dir)]
      (testing "log entries carry the hidden fields"
        (let [{:keys [entries rows error]} (jj/load-log dir {:base-template base :revset "::@ | feature/docs"})]
          (is (nil? error))
          (is (= ["" "WIP: remember me" "Validate login input" "Add login form"
                  "Document the greeting" "Initial commit" ""]
                 (map :description entries)))
          (is (:working-copy? (first entries)))
          (is (= ["feature/login"] (:bookmarks (nth entries 2))))
          (is (:immutable? (nth entries 5)) "main is pushed, so immutable")
          (is (not-any? :file rows))))

      (testing "file lines for expanded changes only"
        (let [{:keys [entries rows]} (jj/load-log dir {:base-template base :files "description(substring:\"Validate\")"})
              files (filter :file rows)]
          (is (= #{"Validate login input"} (set (map #(:description (get entries (:entry %))) files))))
          (is (= [{:status "M" :path "README.md" :source nil}
                  {:status "M" :path "src/login.clj" :source nil}]
                 (map :file files)))))

      (testing "bad revsets report jj's error"
        (is (:error (jj/load-log dir {:base-template base :revset "nope(("}))))

      (testing "bookmarks, local and remote, without @git"
        (is (= #{"main" "main@origin" "feature/login" "feature/docs"}
               (set (map :ref (:bookmarks (jj/bookmarks dir))))))))))
