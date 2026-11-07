(ns jjtui.view-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [jjtui.view :as view]))

(def ^:private hl "\u001b[0m\u001b[30;43m")
(def ^:private reset "\u001b[0m")
(def ^:private green "\u001b[32m")
(def ^:private bold "\u001b[1m")

(deftest highlight-test
  (testing "every occurrence, ignoring case"
    (is (= (str "a " hl "Log" reset " b " hl "LOG" reset) (view/highlight "a Log b LOG" "log"))))
  (testing "the line's style comes back after the match"
    (is (= (str green "a" hl "b" reset green "c") (view/highlight (str green "abc") "b")))
    (is (= (str green "a" hl "b" reset green) (view/highlight (str green "ab") "b"))))
  (testing "a match spanning a style change stays highlighted, and a reset inside it isn't undone"
    (is (= (str bold "a" hl "b" reset hl "c" reset "d") (view/highlight (str bold "ab" reset "cd") "bc"))))
  (testing "escape sequences aren't searched"
    (is (= (str green "x") (view/highlight (str green "x") "32"))))
  (testing "lines without a match are untouched"
    (is (= (str green "abc") (view/highlight (str green "abc") "z")))
    (is (= "abc" (view/highlight "abc" " ")))))
