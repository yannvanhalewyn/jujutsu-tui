(ns jjtui.seed
  "Recipes for throwaway demo jj repos, for trying jjtui and for the
   integration tests. Run `bb seed` to list them, `bb seed <recipe> [dir]` to
   build one."
  (:require
   [clojure.string :as str]
   [jjtui.repo-seed :as rs]))

;; basic ----------------------------------------------------------------------

(defn- basic!
  "main:          Initial commit (pushed)
   feature/login: Add login form -> Validate login input
   feature/docs:  Document the greeting (deletes README.md)
   WIP:           remember me, on feature/login
   @:             empty change on top of the WIP"
  [dir]
  (rs/amend! dir {:message "Initial commit"
                  :files {"README.md" "# Demo\n"
                          "src/app.clj" "(ns app)\n\n(defn greet [n]\n  (str \"Hello \" n))\n"}
                  :bookmark "main"
                  :push? true})
  (rs/change! dir {:on "main"
                   :message "Add login form"
                   :files {"src/login.clj" "(ns login)\n\n(defn form [] [:form])\n"}})
  (rs/change! dir {:message "Validate login input"
                   :files {"src/login.clj" "(ns login)\n\n(defn form [] [:form])\n\n(defn valid? [u] (some? u))\n"
                           "README.md" "# Demo\n\nNow with login.\n"}
                   :bookmark "feature/login"})
  (rs/change! dir {:on "main"
                   :message "Document the greeting"
                   :files {"docs/greeting.md" "Call `greet` with a name.\n"
                           "README.md" nil}
                   :bookmark "feature/docs"})
  (rs/change! dir {:on "feature/login"
                   :message "WIP: remember me"
                   :files {"src/login.clj" "(ns login)\n\n(defn form [] [:form [:checkbox \"remember\"]])\n\n(defn valid? [u] (some? u))\n"}})
  (rs/change! dir {}))

;; conflict -------------------------------------------------------------------

(def ^:private cart-base
  "(ns shop.cart)

(defn line-total [{:keys [price qty]}]
  (* price qty))

(defn subtotal [items]
  (reduce + (map line-total items)))

(defn total [items]
  (subtotal items))
")

(def ^:private cart-tax
  "(ns shop.cart)

(def tax-rate 21/100)

(defn line-total [{:keys [price qty]}]
  (* price qty))

(defn subtotal [items]
  (reduce + (map line-total items)))

(defn total [items]
  (let [sub (subtotal items)]
    (+ sub (* sub tax-rate))))
")

(def ^:private cart-discount
  "(ns shop.cart)

(def discounts
  {\"WELCOME10\" 10/100
   \"VIP25\"     25/100})

(defn line-total [{:keys [price qty]}]
  (* price qty))

(defn subtotal [items]
  (reduce + (map line-total items)))

(defn total [items code]
  (let [sub (subtotal items)]
    (- sub (* sub (get discounts code 0)))))
")

(def ^:private cart-test
  "(ns shop.cart-test
  (:require
   [clojure.test :refer [deftest is]]
   [shop.cart :as cart]))

(def items [{:price 10 :qty 2} {:price 5 :qty 4}])

(deftest total-test
  (is (= 40 (cart/total items nil)))
  (is (= 36 (cart/total items \"WELCOME10\")))
  (is (= 30 (cart/total items \"VIP25\"))))
")

(defn- conflict!
  "main:              Initial commit -> Charge sales tax (pushed)
   feature/discounts: Apply discount codes -> Test discount codes, rebased
                      onto main, both conflicted in src/shop/cart.clj
   @:                 empty change on top of Apply discount codes, for resolving"
  [dir]
  (rs/amend! dir {:message "Initial commit"
                  :files {"deps.edn" "{:paths [\"src\"]\n :aliases {:test {:extra-paths [\"test\"]}}}\n"
                          "src/shop/cart.clj" cart-base}
                  :bookmark "main"
                  :push? true})
  (rs/change! dir {:on "main"
                   :message "Apply discount codes"
                   :files {"src/shop/cart.clj" cart-discount}})
  (rs/change! dir {:message "Test discount codes"
                   :files {"test/shop/cart_test.clj" cart-test}
                   :bookmark "feature/discounts"})
  ;; meanwhile main rewrites the same function
  (rs/change! dir {:on "main"
                   :message "Charge sales tax"
                   :files {"src/shop/cart.clj" cart-tax}
                   :bookmark "main"
                   :push? true})
  ;; rebasing the feature onto the new main conflicts in total
  (rs/jj dir "rebase" "-b" "feature/discounts" "-o" "main")
  (rs/change! dir {:on "feature/discounts-"}))

;; megamerge ------------------------------------------------------------------

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

(defn- megamerge!
  "main:            Initial commit (pushed)
   refactor/store:  Extract todo store (on main)
   feature/tags:    Tag todos -> Filter todos by tag (on refactor/store)
   docs/usage:      Document usage (on main)
   dev changes:     Add :dev alias -> Add REPL helpers (on main, no bookmark)
   megamerge:       jj new feature/tags docs/usage <dev tip>
   @:               empty change on top of the megamerge"
  [dir]
  (rs/amend! dir {:message "Initial commit"
                  :files {"deps.edn" deps
                          "README.md" readme
                          "src/todo/core.clj" core-base}
                  :bookmark "main"
                  :push? true})
  ;; pull the pure todo logic out of the atom-wrangling ns
  (rs/change! dir {:on "main"
                   :message "Extract todo store"
                   :files {"src/todo/store.clj" store-refactor
                           "src/todo/core.clj" core-refactor}
                   :bookmark "refactor/store"})
  (rs/change! dir {:on "refactor/store"
                   :message "Tag todos"
                   :files {"src/todo/store.clj" store-tags
                           "src/todo/core.clj" core-tags}})
  (rs/change! dir {:message "Filter todos by tag"
                   :files {"src/todo/store.clj" store-filter
                           "src/todo/core.clj" core-filter
                           "test/todo/store_test.clj" store-test}
                   :bookmark "feature/tags"})
  (rs/change! dir {:on "main"
                   :message "Document usage"
                   :files {"README.md" readme-docs
                           "docs/usage.md" usage}
                   :bookmark "docs/usage"})
  ;; local dev setup, never meant to be pushed
  (rs/change! dir {:on "main"
                   :message "Add :dev alias"
                   :files {"deps.edn" deps-dev}})
  (rs/change! dir {:message "Add REPL helpers"
                   :files {"dev/user.clj" user-clj}})
  ;; @ is the dev tip
  (rs/change! dir {:on ["feature/tags" "docs/usage" "@"] :message "megamerge"})
  (rs/change! dir {}))

;; registry -------------------------------------------------------------------

(def recipes
  {:basic {:doc "a few branches and a WIP change, a playground for every key"
           :build basic!}
   :conflict {:doc "a feature branch left conflicted by a rebase onto main"
              :build conflict!}
   :megamerge {:doc "a refactor, stacked feature, docs and dev changes merged into one"
               :build megamerge!}})

(defn default-dir [recipe]
  (str "/tmp/jjtui-" (name recipe)))

(defn create!
  "Builds `recipe` (a key of `recipes`) at `dir` (default `default-dir`), plus
   a bare origin next to it, replacing whatever is there. Returns dir."
  ([recipe] (create! recipe (default-dir recipe)))
  ([recipe dir]
   (let [{:keys [build]} (or (get recipes recipe)
                             (throw (ex-info (str "Unknown seed recipe: " (name recipe))
                                             {:recipe recipe})))]
     (rs/init! dir)
     (build dir)
     (str dir))))

(defn- help-text []
  (str/join "\n"
            (concat ["Usage: bb seed <recipe> [dir]" "" "Recipes:"]
                    (for [[k {:keys [doc]}] (sort-by key recipes)]
                      (format "  %-10s %s (default %s)" (name k) doc (default-dir k))))))

(defn -main [& [recipe dir]]
  (cond
    (or (nil? recipe) (#{"-h" "--help" "help"} recipe))
    (println (help-text))

    (contains? recipes (keyword recipe))
    (println "Created" (create! (keyword recipe) (or dir (default-dir recipe))))

    :else
    (binding [*out* *err*]
      (println "Unknown recipe:" recipe)
      (println (help-text))
      (System/exit 1))))
