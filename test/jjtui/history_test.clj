(ns jjtui.history-test
  (:require
   [babashka.fs :as fs]
   [clojure.test :refer [deftest is testing]]
   [jjtui.history :as history]
   [malli.core :as m]))

(def ^:private hour (* 60 60 1000))
(def ^:private day (* 24 hour))

(deftest record-test
  (let [v (-> {} (history/record "/a" 1000) (history/record "/a" 5000))]
    (is (= {"/a" #:visit{:count 2 :last 5000}} v)))
  (testing "keeps the best 100, and always the one just visited"
    (let [now (* 30 day)
          full (into {} (for [i (range 100)] [(str "/r" i) #:visit{:count 50 :last now}]))
          v (history/record full "/new" now)]
      (is (= 100 (count v)))
      (is (= #:visit{:count 1 :last now} (get v "/new"))))))

(deftest score-test
  (let [now (* 30 day)]
    (is (> (history/score #:visit{:count 2 :last (- now (* 2 hour))} now)
           (history/score #:visit{:count 7 :last (- now (* 10 day))} now))
        "a couple of visits today beat a handful a while ago")
    (is (= 4 (history/score #:visit{:count 1 :last (- now 1)} now)))
    (is (= 0.25 (history/score #:visit{:count 1 :last (- now (* 8 day))} now)))))

(deftest file-test
  (fs/with-temp-dir [tmp {}]
    (let [path (str (fs/path tmp "state" "history.edn"))
          repo (str (fs/path tmp "repo"))
          gone (str (fs/path tmp "gone"))]
      (fs/create-dirs (fs/path repo ".jj"))
      (is (= {} (history/read-visits path)) "no file yet")
      (history/record! path repo 1000)
      (history/record! path repo 2000)
      (history/record! path gone 3000)
      (let [visits (history/read-visits path)]
        (is (m/validate history/Visits visits))
        (is (= 2 (get-in visits [repo :visit/count]))))
      (testing "suggestions skip dirs that are no longer jj repos, and excluded ones"
        (is (= [repo] (map :visit/dir (history/suggestions (history/read-visits path) 4000 #{}))))
        (is (empty? (history/suggestions (history/read-visits path) 4000 #{repo}))))
      (testing "a corrupt file reads as empty"
        (spit path "{:oops")
        (is (= {} (history/read-visits path)))))))
