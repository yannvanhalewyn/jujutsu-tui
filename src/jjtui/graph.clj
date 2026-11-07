(ns jjtui.graph
  "The commit graph between trunk and some heads, read from jj once so
   questions about it (which bookmarks does this change stack on?) are
   answered in memory. Commits are keyed by commit id; parents outside the
   graph, at or below trunk, aren't in it.")

;; ---------------------------------------------------------------------------
;; Schemas (malli, plain data: validated in the tests, not at runtime)

(def Node
  [:map
   [:node/id {:doc "Commit id"} :string]
   [:node/parents {:doc "Commit ids, first parent first"} [:vector :string]]
   [:node/bookmarks {:doc "Local bookmark names"} [:vector :string]]])

(def Graph
  [:map [:graph/nodes [:map-of :string Node]]])

;; ---------------------------------------------------------------------------
;; Construction

(defn build [nodes]
  {:graph/nodes (into {} (map (juxt :node/id identity)) nodes)})

;; ---------------------------------------------------------------------------
;; Queries

(defn node [graph id]
  (get-in graph [:graph/nodes id]))

(defn bookmark-node
  "The commit the local bookmark `bname` points at, if it's in the graph."
  [graph bname]
  (some #(when (some #{bname} (:node/bookmarks %)) %) (vals (:graph/nodes graph))))

(defn ancestor?
  "Whether commit `a` is an ancestor of commit `d`, or `d` itself."
  [graph a d]
  (loop [to-visit [d] visited #{}]
    (if-let [id (peek to-visit)]
      (cond
        (= a id) true
        (visited id) (recur (pop to-visit) visited)
        :else (recur (into (pop to-visit) (:node/parents (node graph id))) (conj visited id)))
      false)))

(defn parent-bookmarks
  "Local bookmarks the commit `id` stacks on: walking its ancestors towards
   trunk, each path stops at its first bookmarked commit. Several for a
   merge of branches, leaving out any that's an ancestor of another. Nearest
   first, first parents before later ones."
  [graph id]
  (let [found (loop [queue (into clojure.lang.PersistentQueue/EMPTY (:node/parents (node graph id)))
                     visited #{}
                     found []]
                (if-let [pid (peek queue)]
                  (let [queue (pop queue)
                        n (node graph pid)]
                    (cond
                      (or (visited pid) (nil? n)) (recur queue visited found)
                      (seq (:node/bookmarks n)) (recur queue (conj visited pid) (conj found pid))
                      :else (recur (into queue (:node/parents n)) (conj visited pid) found)))
                  found))]
    (into []
          (comp (remove (fn [a] (some #(and (not= a %) (ancestor? graph a %)) found)))
                (mapcat #(:node/bookmarks (node graph %))))
          found)))
