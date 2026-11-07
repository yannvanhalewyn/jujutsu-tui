(ns jjtui.demo-conflict
  "Builds a small jj repo of Clojure code where rebasing a feature branch onto
   main left it conflicted: main started charging sales tax in `shop.cart/total`
   while `feature/discounts` rewrote the same function to apply discount codes."
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]
   [jjtui.demo :refer [jj write!]]))

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

(defn create!
  "Creates the conflict demo repo at `dir` (plus a bare origin next to it),
   replacing whatever is there. Returns dir.

   main:              Initial commit -> Charge sales tax (pushed)
   feature/discounts: Apply discount codes -> Test discount codes, rebased
                      onto main, both conflicted in src/shop/cart.clj
   @:                 empty change on top of Apply discount codes, for resolving"
  [dir]
  (let [origin (str (fs/normalize (fs/absolutize dir)) "-origin.git")]
    (fs/delete-tree dir)
    (fs/delete-tree origin)
    (fs/create-dirs dir)
    (p/check (p/process ["git" "init" "--bare" "-q" origin] {:out :string :err :string}))
    (jj dir "git" "init" "--colocate")
    (jj dir "git" "remote" "add" "origin" origin))
  (write! dir "deps.edn" "{:paths [\"src\"]\n :aliases {:test {:extra-paths [\"test\"]}}}\n")
  (write! dir "src/shop/cart.clj" cart-base)
  (jj dir "describe" "-m" "Initial commit")
  (jj dir "bookmark" "create" "main" "-r" "@")
  (jj dir "git" "push" "-b" "main")
  ;; feature/discounts branches off the initial commit
  (jj dir "new" "main" "-m" "Apply discount codes")
  (write! dir "src/shop/cart.clj" cart-discount)
  (jj dir "new" "-m" "Test discount codes")
  (write! dir "test/shop/cart_test.clj" cart-test)
  (jj dir "bookmark" "create" "feature/discounts" "-r" "@")
  ;; meanwhile main rewrites the same function
  (jj dir "new" "main" "-m" "Charge sales tax")
  (write! dir "src/shop/cart.clj" cart-tax)
  (jj dir "bookmark" "set" "main" "-r" "@")
  (jj dir "git" "push" "-b" "main")
  ;; rebasing the feature onto the new main conflicts in total
  (jj dir "rebase" "-b" "feature/discounts" "-o" "main")
  (jj dir "new" "feature/discounts-")
  (str dir))
