(ns jjtui.repo
  "Finds the repo to start in, and opens repos."
  (:require
   [babashka.fs :as fs]
   [jjtui.history :as history]
   [jjtui.jj :as jj]))

(def Opened
  "What opening a repo yields: the seed of its jjtui.state/Repo."
  [:map
   [:repo/dir {:doc "Canonical workspace root"} :string]
   [:repo/base-template :string]])

(defn- canonical [path] (str (fs/canonicalize path)))

(defn git-root
  "Nearest directory at or above `dir` with a .git (a directory, or a file in
   worktrees and submodules)."
  [dir]
  (loop [d (fs/canonicalize dir)]
    (cond
      (nil? d) nil
      (fs/exists? (fs/path d ".git")) (str d)
      :else (recur (fs/parent d)))))

(defn child-repos
  "The jj repos directly inside `dir`, canonical, sorted."
  [dir]
  (->> (fs/list-dir dir)
       (filter #(fs/directory? (fs/path % ".jj")))
       (map canonical)
       sort
       vec))

(defn locate
  "Where to start, given the canonical jj workspace root and git root of the
   start directory (either may be nil):

   [:open jj-root]   jj covers the git repo, or there is no git repo
   [:choose {:git-root g :parent p}]
                     no jj repo to open. g is the git repo without jj, if
                     any. jj still finds a workspace when the git repo sits
                     inside one; that's p, else nil"
  [jj-root git-root]
  (cond
    (and git-root (or (nil? jj-root)
                      (and (not= jj-root git-root) (fs/starts-with? git-root jj-root))))
    [:choose {:git-root git-root :parent jj-root}]

    jj-root [:open jj-root]
    :else [:choose {:git-root nil :parent nil}]))

(defn find-start
  "`locate` for `dir`, with the canonical `dir` added to :choose as :dir."
  [dir]
  (let [[kind found :as start] (locate (some-> (jj/workspace-root dir) canonical) (git-root dir))]
    (if (= :choose kind)
      [kind (assoc found :dir (canonical dir))]
      start)))

(defn open
  "Opens the jj repo at or above `dir` and records the visit. Returns an
   Opened or a jj/Failure. `log-template` overrides jj's templates.log."
  [dir log-template]
  (if-let [root (some-> (jj/workspace-root dir) canonical)]
    (do (history/record! (history/default-path) root (System/currentTimeMillis))
        {:repo/dir root
         :repo/base-template (or log-template (jj/base-template root))})
    {:jj/error (str "No jj repo at " dir)}))

(defn init
  "Sets up jj at `dir`, then opens it. `colocate?` reuses the git repo there
   (jj git init --colocate); otherwise jj's git.colocate setting decides."
  [dir colocate? log-template]
  (let [res (jj/run dir (cond-> ["git" "init"] colocate? (conj "--colocate")))]
    (if (zero? (:jj/exit res))
      (open dir log-template)
      {:jj/error (jj/error-text res)})))
