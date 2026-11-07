(ns jjtui.graph-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [jjtui.graph :as graph]))

(defn- g [& nodes]
  (graph/build (for [[id parents bms] nodes]
                 {:node/id id :node/parents parents :node/bookmarks (or bms [])})))

(deftest parent-bookmarks-test
  (testing "the nearest bookmarked ancestor, skipping unbookmarked commits"
    (is (= ["mid"] (graph/parent-bookmarks (g ["c" ["b"]] ["b" ["a"]] ["a" ["x"] ["mid"]] ["x" ["t"] ["low"]]) "c"))))
  (testing "a commit's own bookmark doesn't count"
    (is (= ["low"] (graph/parent-bookmarks (g ["a" ["x"] ["mid"]] ["x" ["t"] ["low"]]) "a"))))
  (testing "none when the walk leaves the graph first, at trunk"
    (is (= [] (graph/parent-bookmarks (g ["c" ["b"]] ["b" ["t"]]) "c"))))
  (testing "a merge stacks on the branches of each parent, in parent order"
    (is (= ["one" "two"] (graph/parent-bookmarks (g ["m" ["p" "q"]] ["p" ["t"] ["one"]] ["q" ["r"]] ["r" ["t"] ["two"]]) "m"))))
  (testing "leaving out a branch another found one already stacks on"
    (is (= ["top"] (graph/parent-bookmarks (g ["m" ["p" "q"]] ["p" ["b"] ["top"]] ["q" ["b"]] ["b" ["t"] ["base"]]) "m")))))
