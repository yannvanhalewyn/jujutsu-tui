(ns jjtui.config-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [jjtui.config :as config]))

(deftest render-command-test
  (testing "vector templates: $files expands to zero or more arguments"
    (is (= ["jj" "diff" "-r" "abc" "a.clj" "b c.clj"]
           (config/render-command ["jj" "diff" "-r" "$revset" "$files"]
                                  {"revset" "abc" "files" ["a.clj" "b c.clj"]})))
    (is (= ["jj" "diff" "-r" "abc"]
           (config/render-command ["jj" "diff" "-r" "$revset" "$files"]
                                  {"revset" "abc" "files" []}))))

  (testing "vector templates substitute inside an argument without quoting"
    (is (= ["nvim" "-c" "DiffviewOpen 111..222"]
           (config/render-command ["nvim" "-c" "DiffviewOpen $from-sha..$to-sha"]
                                  {"from-sha" "111" "to-sha" "222"}))))

  (testing "string templates run through sh with every value shell-quoted"
    (is (= ["sh" "-c" "jj diff -r 'a | b' 'it'\\''s.md' | delta"]
           (config/render-command "jj diff -r $revset $files | delta"
                                  {"revset" "a | b" "files" ["it's.md"]}))))

  (testing "unknown variables are left for the shell"
    (is (= ["sh" "-c" "echo $HOME 'x'"]
           (config/render-command "echo $HOME $revset" {"revset" "x"})))))

(deftest uses-var-test
  (is (config/uses-var? ["git" "difftool" "$from-sha" "$to-sha"] "from-sha"))
  (is (not (config/uses-var? ["jj" "diff" "-r" "$revset"] "from-sha")))
  (is (not (config/uses-var? "echo $from-shax" "from-sha"))))

(deftest merge-config-test
  (let [merged (config/merge-config {:diff-tool :mine
                                     :diff-tools {:mine ["my-tool" "$revset"]}})]
    (is (= :mine (:diff-tool merged)))
    (is (= ["my-tool" "$revset"] (get-in merged [:diff-tools :mine])))
    (is (contains? (:diff-tools merged) :difftastic) "built-in tools are kept")))

(deftest user->internal-test
  (let [cfg (config/user->internal
             (config/merge-config {:diff-tools {:str "tool $files"
                                                :vec ["tool" "$files"]
                                                :map {:cmd ["tool"] :wait? true}}
                                   :show-files :yes}))]
    (testing "every diff tool form becomes a :tool/ map"
      (is (= {:tool/cmd "tool $files" :tool/wait? false} (get-in cfg [:config/diff-tools :str])))
      (is (= {:tool/cmd ["tool" "$files"] :tool/wait? false} (get-in cfg [:config/diff-tools :vec])))
      (is (= {:tool/cmd ["tool"] :tool/wait? true} (get-in cfg [:config/diff-tools :map]))))
    (testing "flags are coerced to booleans"
      (is (true? (:config/show-files? cfg)))
      (is (false? (:config/split? cfg))))))
