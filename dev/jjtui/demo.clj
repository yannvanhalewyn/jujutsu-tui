(ns jjtui.demo
  "Builds a small jj repo with a few branches, for trying jjtui and for the
   integration tests."
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]))

(defn- jj [dir & args]
  (-> (p/process (into ["jj" "--no-pager"] args)
                 {:dir (str dir)
                  :out :string
                  :err :string
                  :extra-env {"JJ_USER" "Demo User"
                              "JJ_EMAIL" "demo@example.com"
                              "JJ_EDITOR" "false"}})
      p/check
      :out))

(defn- write! [dir path content]
  (let [f (fs/file dir path)]
    (fs/create-dirs (fs/parent f))
    (spit f content)))

(defn create!
  "Creates the demo repo at `dir` (plus a bare origin next to it), replacing
   whatever is there. Returns dir."
  [dir]
  (let [origin (str dir "-origin.git")]
    (fs/delete-tree dir)
    (fs/delete-tree origin)
    (fs/create-dirs dir)
    (p/check (p/process ["git" "init" "--bare" "-q" origin] {:out :string :err :string}))
    (jj dir "git" "init" "--colocate")
    (jj dir "git" "remote" "add" "origin" origin))
  (write! dir "README.md" "# Demo\n")
  (write! dir "src/app.clj" "(ns app)\n\n(defn greet [n]\n  (str \"Hello \" n))\n")
  (jj dir "describe" "-m" "Initial commit")
  (jj dir "bookmark" "create" "main" "-r" "@")
  (jj dir "git" "push" "-b" "main")
  ;; feature/login: two stacked changes on main
  (jj dir "new" "main" "-m" "Add login form")
  (write! dir "src/login.clj" "(ns login)\n\n(defn form [] [:form])\n")
  (jj dir "new" "-m" "Validate login input")
  (write! dir "src/login.clj" "(ns login)\n\n(defn form [] [:form])\n\n(defn valid? [u] (some? u))\n")
  (write! dir "README.md" "# Demo\n\nNow with login.\n")
  (jj dir "bookmark" "create" "feature/login" "-r" "@")
  ;; feature/docs: one change on main
  (jj dir "new" "main" "-m" "Document the greeting")
  (write! dir "docs/greeting.md" "Call `greet` with a name.\n")
  (fs/delete (fs/file dir "README.md"))
  (jj dir "bookmark" "create" "feature/docs" "-r" "@")
  ;; work in progress on top of the login branch
  (jj dir "new" "feature/login" "-m" "WIP: remember me")
  (write! dir "src/login.clj" "(ns login)\n\n(defn form [] [:form [:checkbox \"remember\"]])\n\n(defn valid? [u] (some? u))\n")
  (jj dir "new")
  (str dir))
