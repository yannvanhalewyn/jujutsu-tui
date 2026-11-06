(ns jjtui.app-test
  (:require
   [charm.message :as msg]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [jjtui.app :as app]
   [jjtui.config :as config]
   [jjtui.log :as log]
   [jjtui.log-test :refer [file-line marker]]
   [jjtui.state :as st]))

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

(defn- loaded
  "A state with the sample log loaded, cursor on the working copy."
  []
  (let [s (assoc (app/init-state {:dir "/repo" :config config/defaults :base-template "t"})
                 :width 100 :height 30)
        [s [fx]] (app/start s)]
    (first (app/update-state s {:type :log-loaded :gen (:gen fx) :result (log/parse log-output)}))))

(defn- press
  "Feeds keys through update-state. Returns [state effects-of-last-key]."
  [state & ks]
  (reduce (fn [[s _] k]
            (app/update-state s (cond
                                  (map? k) k
                                  (= "enter" k) (msg/key-press :enter)
                                  (= "escape" k) (msg/key-press :escape)
                                  :else (msg/key-press k))))
          [state []]
          ks))

(defn- cursor-id [s] (:change-id (st/cursor-entry s)))
(defn- jj-args [effects] (:args (first (filter #(= :jj (:fx %)) effects))))

(deftest navigation-test
  (let [s (loaded)]
    (is (= "wwww" (cursor-id s)) "starts on the working copy")
    (testing "j visits file rows, which belong to their change"
      (let [[s] (press s "j" "j")]
        (is (= "bbbb" (cursor-id s)))
        (is (= "b.clj" (get-in s [:cursor :file])))))
    (testing "G and g jump to the ends"
      (is (= "mmmm" (cursor-id (first (press s "G")))))
      (is (= "wwww" (cursor-id (first (press s "G" "g"))))))))

(deftest cursor-survives-reload-test
  (let [[s] (press (loaded) "j")
        ;; A new change on top shifts every row down
        shifted (str "○  " (marker {:id "nnnn" :commit "cn" :parents ["cw"]}) "nnnn\n│  N\n" log-output)
        s (first (app/update-state (assoc s :log-gen 7)
                                   {:type :log-loaded :gen 7 :result (log/parse shifted)}))]
    (is (= "bbbb" (cursor-id s)))
    (testing "results of an older load are dropped"
      (let [s2 (first (app/update-state s {:type :log-loaded :gen 3 :result {:entries [] :rows []}}))]
        (is (= (:log s) (:log s2)))))))

(deftest marks-test
  (let [[s] (press (loaded) "m")]
    (is (= #{"wwww"} (:marks s)))
    (is (= "bbbb" (cursor-id s)) "marking moves to the next change")
    (is (nil? (get-in s [:cursor :file])) "skipping its files")
    (let [[s] (press s "m")]
      (is (= #{"wwww" "bbbb"} (:marks s)))
      (testing "batch actions use the marks"
        (let [[_ fx] (press s "n")]
          (is (= ["new" "wwww" "bbbb"] (jj-args fx)))))
      (testing "esc clears"
        (is (empty? (:marks (first (press s "escape")))))))))

(deftest rebase-pick-test
  (let [[s] (press (loaded) "r")]
    (is (= :pick (get-in s [:mode :type])))
    (testing "s cycles the source mode, a inserts after the target"
      (let [[s fx] (press s "s" "j" "j" "j" "a")]
        (is (nil? (:mode s)))
        (is (= ["rebase" "-s" "wwww" "-A" "aaaa"] (jj-args fx)))))
    (testing "enter rebases onto the target"
      (let [[_ fx] (press s "G" "enter")]
        (is (= ["rebase" "-r" "wwww" "-d" "mmmm"] (jj-args fx)))))
    (testing "can't rebase onto a source"
      (let [[s fx] (press s "enter")]
        (is (empty? fx))
        (is (= :error (get-in s [:message :level])))))
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
      (is (= [{:fx :quit}] fx))
      (is (= ["jj" "squash" "-r" "bbbb"] (get-in s [:exec :argv]))))))

(deftest abandon-confirm-test
  (let [[s fx] (press (loaded) "a")]
    (is (empty? fx))
    (is (= :confirm (get-in s [:mode :type])))
    (is (empty? (second (press s "n"))) "n cancels")
    (is (= ["abandon" "wwww"] (jj-args (second (press s "y")))))))

(deftest files-toggle-test
  (testing "shift+tab shows the files of every change"
    (let [[s fx] (press (loaded) (msg/key-press :tab :shift true))]
      (is (:show-files? s))
      (is (= :all (get-in (first fx) [:params :files])))))
  (testing "tab expands just the cursor's change"
    (let [[s fx] (press (loaded) (msg/key-press :tab))]
      (is (= #{"wwww"} (:expanded s)))
      (is (= "change_id(wwww)" (get-in (first fx) [:params :files]))))))

(deftest diff-test
  (testing "enter on a change opens the tool with that commit"
    (let [[s fx] (press (loaded) "enter")]
      (is (= [{:fx :quit}] fx))
      (is (= (into (pop (pop (get-in config/defaults [:diff-tools :jj]))) ["cw"])
             (get-in s [:exec :argv])))))
  (testing "enter on a file row limits the diff to that file"
    (let [[s] (press (loaded) "j" "j" "enter")]
      (is (= "b.clj" (peek (get-in s [:exec :argv]))))))
  (testing "tools that need commit ids resolve them first for marks"
    (let [s (assoc (loaded) :diff-tool :difftool)
          [s fx] (press s "m" "m" "enter")]
      (is (nil? (:exec s)))
      (is (= :resolve-diff (:fx (first fx))))
      (is (= "heads(cw | cb)" (get-in (first fx) [:target :to-rev]))))))

(deftest bookmark-picker-test
  (let [[s] (press (loaded) "b")
        [s] (press s {:type :bookmarks-loaded :purpose :bookmark-set
                      :result {:bookmarks [{:ref "feat" :commit "c" :description "d"}
                                           {:ref "main@origin" :remote "origin" :commit "c" :description "d"}]}})]
    (is (= ["feat  c  d"] (map :label (st/picker-matches (:mode s)))) "remote bookmarks can't be set")
    (testing "typing a new name offers to create it"
      (let [[s fx] (press s "n" "e" "w" "enter")]
        (is (nil? (:mode s)))
        (is (= ["bookmark" "create" "new" "-r" "wwww"] (jj-args fx)))))
    (testing "picking an existing one moves it"
      (let [[_ fx] (press s "f" "enter")]
        (is (= ["bookmark" "set" "feat" "-r" "wwww" "--allow-backwards"] (jj-args fx)))))))

(deftest revset-error-test
  (let [[s fx] (press (loaded) "L" "x" "(" "enter")]
    (is (= "x(" (:revset s)))
    (let [[s fx] (app/update-state s {:type :log-loaded :gen (:gen (first fx)) :result {:error "bad revset"}})]
      (is (nil? (:revset s)) "falls back to the last revset that worked")
      (is (= :error (get-in s [:message :level])))
      (is (nil? (get-in (first fx) [:params :revset]))))))

(deftest fuzzy-score-test
  (is (some? (st/fuzzy-score "fl" "feature/login")))
  (is (nil? (st/fuzzy-score "lf" "feature/login")))
  (is (< (st/fuzzy-score "login" "feature/login") (st/fuzzy-score "fl" "feature/login"))
      "a substring beats scattered characters"))
