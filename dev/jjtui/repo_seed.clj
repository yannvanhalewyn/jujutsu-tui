(ns jjtui.repo-seed
  "Helpers for building throwaway jj repos: a fresh colocated repo with a bare
   origin, plus terse change/bookmark/push steps for scripting history."
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]))

(def ^:private env
  {"JJ_USER" "Demo User"
   "JJ_EMAIL" "demo@example.com"
   "JJ_EDITOR" "false"})

(defn jj
  "Runs jj in `dir`, throwing on failure. Returns stdout."
  [dir & args]
  (-> (p/process (into ["jj" "--no-pager"] args)
                 {:dir (str dir) :out :string :err :string :extra-env env})
      p/check
      :out))

(defn origin-dir
  "The bare git origin that `init!` creates next to `dir`."
  [dir]
  (str (fs/normalize (fs/absolutize dir)) "-origin.git"))

(def gitignore
  "Written by `init!` so tool caches created while playing in a seed repo
   don't get snapshotted into its changes."
  ".cpcache/\n")

(defn init!
  "Creates a colocated jj repo at `dir` with a bare `origin` remote next to it
   (see `origin-dir`), replacing whatever is at either path. The working copy
   starts with just `gitignore` as `.gitignore`. Returns dir."
  [dir]
  (let [origin (origin-dir dir)]
    (fs/delete-tree dir)
    (fs/delete-tree origin)
    (fs/create-dirs dir)
    (p/check (p/process ["git" "init" "--bare" "-q" origin] {:out :string :err :string}))
    (jj dir "git" "init" "--colocate")
    (jj dir "git" "remote" "add" "origin" origin)
    (spit (fs/file dir ".gitignore") gitignore)
    (str dir)))

(defn write-files!
  "Writes `files`, a map of repo-relative path to content, into `dir`. A nil
   content deletes the file."
  [dir files]
  (doseq [[path content] files
          :let [f (fs/file dir path)]]
    (if (some? content)
      (do (fs/create-dirs (fs/parent f))
          (spit f content))
      (fs/delete-if-exists f))))

(defn- apply-change! [dir {:keys [message files bookmark push?]}]
  (when message (jj dir "describe" "-m" message))
  (write-files! dir files)
  (when bookmark (jj dir "bookmark" "set" bookmark "-r" "@"))
  (when push?
    (assert bookmark ":push? needs a :bookmark")
    (jj dir "git" "push" "-b" bookmark)))

(defn amend!
  "Updates the working-copy change in place: sets its `:message`, writes
   `:files` (see `write-files!`), points `:bookmark` at it and pushes that
   bookmark when `:push?`. Useful for the first commit of a fresh repo."
  [dir opts]
  (apply-change! dir opts))

(defn change!
  "Starts a new change on `:on` (a revset or a seq of them for a merge; default
   @), then applies `opts` like `amend!`."
  [dir {:keys [on] :or {on "@"} :as opts}]
  (apply jj dir "new" (if (string? on) [on] on))
  (apply-change! dir opts))
