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

(defn locate
  "Where to start, given the canonical jj workspace root and git root of the
   start directory (either may be nil):

   [:open jj-root]   jj covers the git repo, or there is no git repo
   [:choose {:git-root g :parent p}]
                     a git repo without jj. jj still finds a workspace when
                     the git repo sits inside one; that's :parent, else nil
   nil               neither"
  [jj-root git-root]
  (cond
    (and git-root (or (nil? jj-root)
                      (and (not= jj-root git-root) (fs/starts-with? git-root jj-root))))
    [:choose {:git-root git-root :parent jj-root}]

    jj-root [:open jj-root]
    :else nil))

(defn find-start [dir]
  (locate (some-> (jj/workspace-root dir) canonical) (git-root dir)))

(defn open
  "Opens the jj repo at or above `dir` and records the visit. Returns an
   Opened or a jj/Failure. `log-template` overrides jj's templates.log."
  [dir log-template]
  (if-let [root (some-> (jj/workspace-root dir) canonical)]
    (do (history/record! (history/default-path) root (System/currentTimeMillis))
        {:repo/dir root
         :repo/base-template (or log-template (jj/base-template root))})
    {:jj/error (str "No jj repo at " dir)}))

(defn init-colocated
  "Sets up jj in the git repo at `dir` (jj git init --colocate), then opens it."
  [dir log-template]
  (let [res (jj/run dir ["git" "init" "--colocate"])]
    (if (zero? (:jj/exit res))
      (open dir log-template)
      {:jj/error (jj/error-text res)})))
