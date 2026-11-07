(ns jjtui.history
  "Which repos jjtui has opened, how often and how recently, to suggest where
   to go next.

   Kept in an EDN file, {dir #:visit{:count n :last epoch-ms}}, under
   $XDG_STATE_HOME. It's a best-effort cache: an unreadable file counts as
   empty, and a failed write loses one visit."
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]))

(def Visit
  [:map
   [:visit/count pos-int?]
   [:visit/last {:doc "Epoch millis"} :int]])

(def Visits [:map-of :string Visit])

(def ^:private max-entries 100)

(defn default-path []
  (str (fs/path (fs/xdg-state-home) "jjtui" "history.edn")))

(defn read-visits [path]
  (try
    (let [v (edn/read-string (slurp path))]
      (if (map? v) v {}))
    (catch Exception _ {})))

(def ^:private hour (* 60 60 1000))
(def ^:private day (* 24 hour))
(def ^:private week (* 7 day))

(defn score
  "Frecency: the visit count weighted by how recent the last visit was, with
   zoxide's weights."
  [{n :visit/count last-ms :visit/last} now]
  (let [age (- now last-ms)]
    (* n (cond
           (< age hour) 4
           (< age day) 2
           (< age week) 0.5
           :else 0.25))))

(defn- best-first [now]
  (fn [[_ v]] [(- (score v now)) (- (:visit/last v))]))

(defn record
  "`visits` with a visit to `dir` at `now`. Keeps the best `max-entries`,
   always including `dir`."
  [visits dir now]
  (let [visit {:visit/count (inc (get-in visits [dir :visit/count] 0)) :visit/last now}
        others (dissoc visits dir)]
    (assoc (if (< (count others) max-entries)
             others
             (into {} (take (dec max-entries) (sort-by (best-first now) others))))
           dir visit)))

(defn record!
  "Records a visit to `dir` in the file at `path`. Replaces the file
   atomically, so a concurrent reader never sees half of it."
  [path dir now]
  (try
    (let [visits (record (read-visits path) dir now)
          _ (fs/create-dirs (fs/parent path))
          tmp (fs/create-temp-file {:dir (fs/parent path) :prefix "history" :suffix ".edn"})]
      (spit (str tmp) (pr-str visits))
      (fs/move tmp path {:replace-existing true :atomic-move true}))
    (catch Exception _ nil)))

(defn suggestions
  "Visited dirs that are still jj repos, best first, without those in
   `exclude`. Each is a Visit with its :visit/dir."
  [visits now exclude]
  (->> visits
       (remove (fn [[dir]] (contains? exclude dir)))
       (filter (fn [[dir]] (fs/directory? (fs/path dir ".jj"))))
       (sort-by (best-first now))
       (mapv (fn [[dir v]] (assoc v :visit/dir dir)))))
