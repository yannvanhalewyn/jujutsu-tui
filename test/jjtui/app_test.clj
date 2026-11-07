(ns jjtui.app-test
  (:require
   [charm.message :as msg]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [jjtui.app :as app]
   [jjtui.config :as config]
   [jjtui.effects :as effects]
   [jjtui.log :as log]
   [jjtui.log-test :refer [file-line marker]]
   [jjtui.state :as st]
   [malli.core :as m]
   [malli.error :as me]))

(def log-output
  (str/join "\n"
            [(str "@  " (marker {:id "wwww" :commit "cw" :parents ["cb"] :wc? true}) "wwww")
             "│  (no description set)"
             (str "○  " (marker {:id "bbbb" :commit "cb" :parents ["ca"] :description "B" :bookmarks ["feat"]}) "bbbb")
             "│  B"
             (file-line "│  " "modified" "b.clj")
             (str "○  " (marker {:id "aaaa" :commit "ca" :parents ["cm"] :description "A"}) "aaaa")
             "│  A"
             (str "◆  " (marker {:id "mmmm" :commit "cm" :parents [] :immutable? true :description "M"}) "mmmm")
             "│  M"]))

(defn- checked
  "Asserts that `x` matches `schema`, returning it."
  [schema x]
  (is (nil? (some-> (m/explain schema x) me/humanize)) (str "matches " (m/form schema)))
  x)

(defn- step
  "update-state, checking the message, the new state and its effects against
   their schemas. charm's own messages aren't ours to check."
  [state message]
  (when (= "msg" (namespace (:type message)))
    (checked effects/Msg message))
  (let [[state fx] (app/update-state state message)]
    [(checked st/State state) (mapv #(checked effects/Effect %) fx)]))

(defn- loaded-msg [gen output]
  {:type :msg/log-loaded :msg/gen gen :msg/result (log/parse output)})

(defn- loaded
  "A state with the sample log loaded, cursor on the working copy."
  []
  (let [s (checked st/State (assoc (app/init-state {:dir "/repo" :config config/defaults :base-template "t"})
                                   :app/width 100 :app/height 30))
        [s [fx]] (app/start s)]
    (first (step s (loaded-msg (:fx/gen fx) log-output)))))

(defn- press
  "Feeds keys through update-state. Returns [state effects-of-last-key]."
  [state & ks]
  (reduce (fn [[s _] k]
            (step s (cond
                      (map? k) k
                      (= "enter" k) (msg/key-press :enter)
                      (= "escape" k) (msg/key-press :escape)
                      :else (msg/key-press k))))
          [state []]
          ks))

(defn- cursor-id [s] (:change/id (st/cursor-entry s)))
(defn- jj-args [effects] (:fx/args (first (filter #(= :jj (:fx/type %)) effects))))

(deftest navigation-test
  (let [s (loaded)]
    (is (= "wwww" (cursor-id s)) "starts on the working copy")
    (testing "j visits file rows, which belong to their change"
      (let [[s] (press s "j" "j")]
        (is (= "bbbb" (cursor-id s)))
        (is (= "b.clj" (get-in s [:app/cursor :file/path])))))
    (testing "G and g jump to the ends"
      (is (= "mmmm" (cursor-id (first (press s "G")))))
      (is (= "wwww" (cursor-id (first (press s "G" "g"))))))))

(deftest cursor-survives-reload-test
  (let [[s] (press (loaded) "j")
        ;; A new change on top shifts every row down
        shifted (str "○  " (marker {:id "nnnn" :commit "cn" :parents ["cw"]}) "nnnn\n│  N\n" log-output)
        [s] (step (assoc s :app/log-gen 7) (loaded-msg 7 shifted))]
    (is (= "bbbb" (cursor-id s)))
    (testing "results of an older load are dropped"
      (let [[s2] (step s (loaded-msg 3 ""))]
        (is (= (:app/log s) (:app/log s2)))))))

(deftest marks-test
  (let [[s] (press (loaded) "m")]
    (is (= #{"wwww"} (:app/marks s)))
    (is (= "bbbb" (cursor-id s)) "marking moves to the next change")
    (is (nil? (get-in s [:app/cursor :file/path])) "skipping its files")
    (let [[s] (press s "m")]
      (is (= #{"wwww" "bbbb"} (:app/marks s)))
      (testing "batch actions use the marks"
        (let [[_ fx] (press s "n")]
          (is (= ["new" "wwww" "bbbb"] (jj-args fx)))))
      (testing "esc clears"
        (is (empty? (:app/marks (first (press s "escape")))))))))

(deftest rebase-pick-test
  (let [[s] (press (loaded) "r")]
    (is (= :pick (get-in s [:app/mode :mode/type])))
    (testing "s cycles the source mode, a inserts after the target"
      (let [[s fx] (press s "s" "j" "j" "j" "a")]
        (is (nil? (:app/mode s)))
        (is (= ["rebase" "-s" "wwww" "-A" "aaaa"] (jj-args fx)))))
    (testing "enter rebases onto the target"
      (let [[_ fx] (press s "G" "enter")]
        (is (= ["rebase" "-r" "wwww" "-d" "mmmm"] (jj-args fx)))))
    (testing "can't rebase onto a source"
      (let [[s fx] (press s "enter")]
        (is (empty? fx))
        (is (= :error (get-in s [:app/message :message/level])))))
    (testing "--ignore-immutable applies when toggled"
      (let [[s] (press (loaded) "!" "r" "G")
            [_ fx] (press s "enter")]
        (is (= ["rebase" "-r" "wwww" "-d" "mmmm" "--ignore-immutable"] (jj-args fx)))))))

(deftest squash-test
  (testing "into an undescribed change: background, no editor needed"
    (let [[s fx] (press (loaded) "j" "m" "g" "s")]
      (is (= "wwww" (cursor-id s)))
      (is (= ["squash" "--from" "bbbb" "--into" "wwww"] (jj-args fx)))))
  (testing "both described: suspends so jj can open the editor"
    (let [[s fx] (press (loaded) "j" "s")]
      (is (= "bbbb" (cursor-id s)))
      (is (= [{:fx/type :quit}] fx))
      (is (= ["jj" "squash" "-r" "bbbb"] (get-in s [:app/exec :exec/argv]))))))

(deftest abandon-confirm-test
  (let [[s fx] (press (loaded) "a")]
    (is (empty? fx))
    (is (= :confirm (get-in s [:app/mode :mode/type])))
    (is (empty? (second (press s "n"))) "n cancels")
    (is (= ["abandon" "wwww"] (jj-args (second (press s "y")))))))

(deftest files-toggle-test
  (testing "shift+tab shows the files of every change"
    (let [[s fx] (press (loaded) (msg/key-press :tab :shift true))]
      (is (:app/show-files? s))
      (is (= :all (get-in (first fx) [:fx/params :files])))))
  (testing "tab expands just the cursor's change"
    (let [[s fx] (press (loaded) (msg/key-press :tab))]
      (is (= #{"wwww"} (:app/expanded s)))
      (is (= "change_id(wwww)" (get-in (first fx) [:fx/params :files]))))))

(deftest diff-test
  (testing "enter on a change opens the tool with that commit"
    (let [[s fx] (press (loaded) "enter")]
      (is (= [{:fx/type :quit}] fx))
      (is (= (into (pop (pop (get-in config/defaults [:diff-tools :jj]))) ["cw"])
             (get-in s [:app/exec :exec/argv])))))
  (testing "enter on a file row limits the diff to that file"
    (let [[s] (press (loaded) "j" "j" "enter")]
      (is (= "b.clj" (peek (get-in s [:app/exec :exec/argv]))))))
  (testing "tools that need commit ids resolve them first for marks"
    (let [s (assoc (loaded) :app/diff-tool :difftool)
          [s fx] (press s "m" "m" "enter")]
      (is (nil? (:app/exec s)))
      (is (= :resolve-diff (:fx/type (first fx))))
      (is (= "heads(cw | cb)" (get-in (first fx) [:fx/target :target/to-rev]))))))

(defn- bookmark [bname remote]
  {:bookmark/name bname
   :bookmark/remote remote
   :bookmark/ref (if remote (str bname "@" remote) bname)
   :bookmark/commit "c"
   :bookmark/description "d"})

(deftest bookmark-picker-test
  (let [[s] (press (loaded) "b")
        [s] (press s {:type :msg/bookmarks-loaded
                      :msg/purpose :bookmark-set
                      :msg/result {:jj/bookmarks [(bookmark "feat" nil) (bookmark "main" "origin")]}})]
    (is (= ["feat  c  d"] (map :item/label (st/picker-matches (:app/mode s)))) "remote bookmarks can't be set")
    (testing "typing a new name offers to create it"
      (let [[s fx] (press s "n" "e" "w" "enter")]
        (is (nil? (:app/mode s)))
        (is (= ["bookmark" "create" "new" "-r" "wwww"] (jj-args fx)))))
    (testing "picking an existing one moves it"
      (let [[_ fx] (press s "f" "enter")]
        (is (= ["bookmark" "set" "feat" "-r" "wwww" "--allow-backwards"] (jj-args fx)))))))

(deftest revset-error-test
  (let [[s fx] (press (loaded) "L" "x" "(" "enter")]
    (is (= "x(" (:app/revset s)))
    (let [[s fx] (step s {:type :msg/log-loaded :msg/gen (:fx/gen (first fx)) :msg/result {:jj/error "bad revset"}})]
      (is (nil? (:app/revset s)) "falls back to the last revset that worked")
      (is (= :error (get-in s [:app/message :message/level])))
      (is (nil? (get-in (first fx) [:fx/params :revset]))))))

(deftest fuzzy-score-test
  (is (some? (st/fuzzy-score "fl" "feature/login")))
  (is (nil? (st/fuzzy-score "lf" "feature/login")))
  (is (< (st/fuzzy-score "login" "feature/login") (st/fuzzy-score "fl" "feature/login"))
      "a substring beats scattered characters"))
