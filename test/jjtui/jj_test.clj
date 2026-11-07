(ns jjtui.jj-test
  "Runs the templates against a real jj, so a jj release that changes the
   template language fails here rather than in the TUI."
  (:require
   [babashka.fs :as fs]
   [clojure.test :refer [deftest is testing]]
   [jjtui.demo :as demo]
   [jjtui.jj :as jj]
   [jjtui.log :as log]
   [malli.core :as m]
   [malli.error :as me]))

(defn- explain [schema x] (some-> (m/explain schema x) me/humanize))

(deftest real-repo-test
  (fs/with-temp-dir [tmp {}]
    (let [dir (demo/create! (str (fs/path tmp "repo")))
          base (jj/base-template dir)]
      (testing "log entries carry the hidden fields"
        (let [{:log/keys [entries rows] :as res} (jj/load-log dir {:base-template base :revset "::@ | feature/docs"})]
          (is (nil? (explain log/Log res)))
          (is (= ["" "WIP: remember me" "Validate login input" "Add login form"
                  "Document the greeting" "Initial commit" ""]
                 (map :change/description entries)))
          (is (:change/working-copy? (first entries)))
          (is (= ["feature/login"] (:change/bookmarks (nth entries 2))))
          (is (:change/immutable? (nth entries 5)) "main is pushed, so immutable")
          (is (not-any? :row/file rows))))

      (testing "file lines for expanded changes only"
        (let [{:log/keys [entries rows]} (jj/load-log dir {:base-template base :files "description(substring:\"Validate\")"})
              files (filter :row/file rows)]
          (is (= #{"Validate login input"} (set (map #(:change/description (get entries (:row/entry %))) files))))
          (is (= [{:file/status "M" :file/path "README.md" :file/source nil}
                  {:file/status "M" :file/path "src/login.clj" :file/source nil}]
                 (map :row/file files)))))

      (testing "bad revsets report jj's error"
        (let [res (jj/load-log dir {:base-template base :revset "nope(("})]
          (is (nil? (explain jj/Failure res)))
          (is (:jj/error res))))

      (testing "bookmarks, local and remote, without @git"
        (let [res (jj/bookmarks dir)]
          (is (nil? (explain jj/Bookmarks res)))
          (is (= #{"main" "main@origin" "feature/login" "feature/docs"}
                 (set (map :bookmark/ref (:jj/bookmarks res))))))))))
