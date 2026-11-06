(ns jjtui.state
  "Read-only queries on the app state, shared by jjtui.app and jjtui.view."
  (:require
   [charm.components.text-input :as ti]
   [clojure.string :as str]
   [jjtui.log :as log]))

(defn rows [state] (get-in state [:log :rows]))
(defn entries [state] (get-in state [:log :entries]))

;; ---------------------------------------------------------------------------
;; Cursor

(defn row-cursor [state row]
  (when row
    {:change-id (:change-id (get (entries state) (:entry row)))
     :file (get-in row [:file :path])}))

(defn selectable-indices [state]
  (into [] (keep-indexed (fn [i row] (when (log/selectable? row) i))) (rows state)))

(defn- row-matches? [state row {:keys [change-id file]}]
  (and (log/selectable? row)
       (= change-id (:change-id (get (entries state) (:entry row))))
       (= file (get-in row [:file :path]))))

(defn cursor-index
  "Index of the cursor's row: the cursor's change (and file), else that
   change's first line, else the selectable row nearest to where the cursor
   was."
  [state]
  (let [rs (rows state)
        {:keys [change-id] :as cursor} (:cursor state)
        find-row (fn [c] (first (keep-indexed (fn [i row] (when (row-matches? state row c) i)) rs)))]
    (or (when cursor (find-row cursor))
        (when change-id (find-row {:change-id change-id :file nil}))
        (let [idxs (selectable-indices state)]
          (when (seq idxs)
            (or (first (filter #(>= % (:cursor-row state)) idxs))
                (peek idxs)))))))

(defn cursor-row [state]
  (when-let [i (cursor-index state)]
    (get (rows state) i)))

(defn cursor-entry [state]
  (when-let [row (cursor-row state)]
    (get (entries state) (:entry row))))

;; ---------------------------------------------------------------------------
;; Pickers

(defn fuzzy-score
  "Score of `query` against `text`, lower is better; nil when the characters
   of query don't all appear in order."
  [query text]
  (let [q (str/lower-case query)
        t (str/lower-case text)]
    (if (str/blank? q)
      0
      (if-let [idx (str/index-of t q)]
        idx
        (loop [qi 0 ti 0 gaps 0 last-match -1]
          (cond
            (= qi (count q)) (+ 1000 gaps)
            (>= ti (count t)) nil
            (= (nth q qi) (nth t ti))
            (recur (inc qi) (inc ti) (if (neg? last-match) gaps (+ gaps (- ti last-match 1))) ti)
            :else (recur qi (inc ti) gaps last-match)))))))

(defn picker-matches
  "The picker's visible items: matching items, best first. The bookmark picker
   also offers to create the typed name, after any matches."
  [{:keys [items input purpose]}]
  (let [query (str/trim (ti/value input))
        matched (->> items
                     (keep (fn [item]
                             (when-let [s (fuzzy-score query (:label item))]
                               [s item])))
                     (sort-by first)
                     (mapv second))]
    (if (and (= :bookmark-set purpose)
             (not (str/blank? query))
             (not-any? #(= query (:value %)) items))
      (conj matched {:label (str "+ create " query) :value query :create? true})
      matched)))

