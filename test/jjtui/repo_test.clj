(ns jjtui.repo-test
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]
   [clojure.test :refer [deftest is testing]]
   [jjtui.history :as history]
   [jjtui.repo :as repo]))

(deftest locate-test
  (testing "jj covers the git repo"
    (is (= [:open "/r"] (repo/locate "/r" "/r")) "colocated")
    (is (= [:open "/r"] (repo/locate "/r" nil)) "jj without git")
    (is (= [:open "/g/r"] (repo/locate "/g/r" "/g")) "a jj repo inside a git repo"))
  (testing "a git repo without jj"
    (is (= [:choose {:git-root "/g" :parent nil}] (repo/locate nil "/g")))
    (is (= [:choose {:git-root "/r/lib" :parent "/r"}] (repo/locate "/r" "/r/lib"))
        "jj would open the jj repo around it; that's offered instead")
    (is (= [:choose {:git-root "/r-lib" :parent nil}] (repo/locate nil "/r-lib"))))
  (testing "path prefixes aren't parents"
    (is (= [:open "/r"] (repo/locate "/r" "/rx"))))
  (is (= [:choose {:git-root nil :parent nil}] (repo/locate nil nil)) "neither jj nor git"))

(deftest git-root-test
  (fs/with-temp-dir [tmp {}]
    (let [root (fs/canonicalize tmp)]
      (fs/create-dirs (fs/path root ".git"))
      (fs/create-dirs (fs/path root "a" "b"))
      (is (= (str root) (repo/git-root (str (fs/path root "a" "b")))))
      (testing "a .git file (worktrees, submodules) counts"
        (fs/create-dirs (fs/path root "sub"))
        (spit (str (fs/path root "sub" ".git")) "gitdir: ../.git/modules/sub")
        (is (= (str (fs/path root "sub")) (repo/git-root (str (fs/path root "sub")))))))))

(defn- git! [dir & args]
  (p/check (p/process (into ["git" "-c" "user.name=t" "-c" "user.email=t@t" "-c" "commit.gpgsign=false"] args)
                      {:dir (str dir) :out :string :err :string})))

(deftest init-colocated-test
  (fs/with-temp-dir [tmp {}]
    (with-redefs [history/default-path (constantly (str (fs/path tmp "history.edn")))]
      (let [outer (str (fs/canonicalize tmp))
            lib (str (fs/path outer "lib"))
            sub (str (fs/path lib "src"))]
        (git! outer "init" "-q")
        (is (nil? (:jj/error (repo/init outer true nil))))
        (fs/create-dirs sub)
        (git! lib "init" "-q")
        (spit (str (fs/path lib "x")) "x")
        (git! lib "add" "x")
        (git! lib "commit" "-qm" "lib")
        (testing "started in a subdirectory of a git repo inside a jj repo"
          (is (= [:choose {:git-root lib :parent outer :dir sub}] (repo/find-start sub))))
        (testing "jj is set up in the git root, not the start dir or the outer jj repo"
          (is (= lib (:repo/dir (repo/init lib true nil))))
          (is (fs/exists? (fs/path lib ".jj")))
          (is (not (fs/exists? (fs/path sub ".jj"))))
          (is (= [:open lib] (repo/find-start sub))))))))

(deftest outside-any-repo-test
  (fs/with-temp-dir [tmp {}]
    (with-redefs [history/default-path (constantly (str (fs/path tmp "history.edn")))]
      (let [dir (str (fs/path (fs/canonicalize tmp) "code"))
            [api app plain] (map #(str (fs/path dir %)) ["api" "app" "plain"])]
        (run! fs/create-dirs [api app plain])
        (is (= [:choose {:git-root nil :parent nil :dir dir}] (repo/find-start dir)))
        (is (empty? (repo/child-repos dir)))
        (testing "jj is created in a plain dir"
          (is (= app (:repo/dir (repo/init app false nil))))
          (is (= api (:repo/dir (repo/init api false nil)))))
        (testing "the jj repos directly inside a dir, sorted"
          (is (= [api app] (repo/child-repos dir))))))))
