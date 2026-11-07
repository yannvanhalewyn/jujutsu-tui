(ns jjtui.demo-megamerge
  "Builds a small jj repo of Clojure code in the megamerge workflow: a refactor,
   a feature stacked on that refactor, a docs change and some local dev
   changes, all merged together with `jj new a b c` so they can be worked on at
   once."
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]
   [jjtui.demo :refer [jj write!]]))

(def ^:private deps
  "{:paths [\"src\"]
 :aliases {:test {:extra-paths [\"test\"]}}}
")

(def ^:private deps-dev
  "{:paths [\"src\"]
 :aliases {:test {:extra-paths [\"test\"]}
           :dev {:extra-paths [\"dev\"]}}}
")

(def ^:private readme
  "# Todo

A tiny todo list.
")

(def ^:private readme-docs
  "# Todo

A tiny todo list. See [docs/usage.md](docs/usage.md) to get started.
")

(def ^:private usage
  "# Usage

```clojure
(require '[todo.core :as todo])

(todo/add! \"Buy milk\")
(todo/complete! 0)
(todo/pending)
```
")

(def ^:private core-base
  "(ns todo.core)

(defonce todos (atom []))

(defn add! [title]
  (swap! todos conj {:title title :done? false}))

(defn complete! [idx]
  (swap! todos assoc-in [idx :done?] true))

(defn pending []
  (remove :done? @todos))
")

(def ^:private core-refactor
  "(ns todo.core
  (:require
   [todo.store :as store]))

(defonce todos (atom []))

(defn add! [title]
  (swap! todos store/add title))

(defn complete! [idx]
  (swap! todos store/complete idx))

(defn pending []
  (store/pending @todos))
")

(def ^:private core-tags
  "(ns todo.core
  (:require
   [todo.store :as store]))

(defonce todos (atom []))

(defn add!
  ([title] (add! title #{}))
  ([title tags] (swap! todos store/add title tags)))

(defn complete! [idx]
  (swap! todos store/complete idx))

(defn pending []
  (store/pending @todos))
")

(def ^:private core-filter
  "(ns todo.core
  (:require
   [todo.store :as store]))

(defonce todos (atom []))

(defn add!
  ([title] (add! title #{}))
  ([title tags] (swap! todos store/add title tags)))

(defn complete! [idx]
  (swap! todos store/complete idx))

(defn pending []
  (store/pending @todos))

(defn tagged [tag]
  (store/tagged @todos tag))
")

(def ^:private store-refactor
  "(ns todo.store)

(defn add [todos title]
  (conj todos {:title title :done? false}))

(defn complete [todos idx]
  (assoc-in todos [idx :done?] true))

(defn pending [todos]
  (remove :done? todos))
")

(def ^:private store-tags
  "(ns todo.store)

(defn add [todos title tags]
  (conj todos {:title title :done? false :tags (set tags)}))

(defn complete [todos idx]
  (assoc-in todos [idx :done?] true))

(defn pending [todos]
  (remove :done? todos))
")

(def ^:private store-filter
  "(ns todo.store)

(defn add [todos title tags]
  (conj todos {:title title :done? false :tags (set tags)}))

(defn complete [todos idx]
  (assoc-in todos [idx :done?] true))

(defn pending [todos]
  (remove :done? todos))

(defn tagged [todos tag]
  (filter #(contains? (:tags %) tag) todos))
")

(def ^:private store-test
  "(ns todo.store-test
  (:require
   [clojure.test :refer [deftest is]]
   [todo.store :as store]))

(deftest tagged-test
  (let [todos (-> []
                  (store/add \"Buy milk\" #{:errand})
                  (store/add \"Write report\" #{:work}))]
    (is (= [\"Buy milk\"] (map :title (store/tagged todos :errand))))))
")

(def ^:private user-clj
  "(ns user
  (:require
   [todo.core :as todo]))

(defn seed! []
  (reset! todo/todos [])
  (todo/add! \"Buy milk\" #{:errand})
  (todo/add! \"Write report\" #{:work}))

(comment
  (seed!)
  (todo/pending))
")

(defn create!
  "Creates the megamerge demo repo at `dir` (plus a bare origin next to it),
   replacing whatever is there. Returns dir.

   main:            Initial commit (pushed)
   refactor/store:  Extract todo store (on main)
   feature/tags:    Tag todos -> Filter todos by tag (on refactor/store)
   docs/usage:      Document usage (on main)
   dev changes:     Add :dev alias -> Add REPL helpers (on main, no bookmark)
   megamerge:       jj new feature/tags docs/usage <dev tip>
   @:               empty change on top of the megamerge"
  [dir]
  (let [origin (str (fs/normalize (fs/absolutize dir)) "-origin.git")]
    (fs/delete-tree dir)
    (fs/delete-tree origin)
    (fs/create-dirs dir)
    (p/check (p/process ["git" "init" "--bare" "-q" origin] {:out :string :err :string}))
    (jj dir "git" "init" "--colocate")
    (jj dir "git" "remote" "add" "origin" origin))
  (write! dir "deps.edn" deps)
  (write! dir "README.md" readme)
  (write! dir "src/todo/core.clj" core-base)
  (jj dir "describe" "-m" "Initial commit")
  (jj dir "bookmark" "create" "main" "-r" "@")
  (jj dir "git" "push" "-b" "main")
  ;; refactor/store: pull the pure todo logic out of the atom-wrangling ns
  (jj dir "new" "main" "-m" "Extract todo store")
  (write! dir "src/todo/store.clj" store-refactor)
  (write! dir "src/todo/core.clj" core-refactor)
  (jj dir "bookmark" "create" "refactor/store" "-r" "@")
  ;; feature/tags: builds on the extracted store
  (jj dir "new" "refactor/store" "-m" "Tag todos")
  (write! dir "src/todo/store.clj" store-tags)
  (write! dir "src/todo/core.clj" core-tags)
  (jj dir "new" "-m" "Filter todos by tag")
  (write! dir "src/todo/store.clj" store-filter)
  (write! dir "src/todo/core.clj" core-filter)
  (write! dir "test/todo/store_test.clj" store-test)
  (jj dir "bookmark" "create" "feature/tags" "-r" "@")
  ;; docs/usage: independent of the code changes
  (jj dir "new" "main" "-m" "Document usage")
  (write! dir "README.md" readme-docs)
  (write! dir "docs/usage.md" usage)
  (jj dir "bookmark" "create" "docs/usage" "-r" "@")
  ;; local dev setup, never meant to be pushed
  (jj dir "new" "main" "-m" "Add :dev alias")
  (write! dir "deps.edn" deps-dev)
  (jj dir "new" "-m" "Add REPL helpers")
  (write! dir "dev/user.clj" user-clj)
  ;; megamerge of the feature, the docs and the dev changes (@ is the dev tip)
  (jj dir "new" "feature/tags" "docs/usage" "@" "-m" "megamerge")
  (jj dir "new")
  (str dir))
