(ns jjtui.jj
  "Runs jj. Everything here does I/O; command construction lives in
   jjtui.app and jjtui.log so it can be tested without a repo.

   Queries return their data or {:jj/error text}."
  (:require
   [babashka.process :as p]
   [clojure.string :as str]
   [jjtui.log :as log]))

;; ---------------------------------------------------------------------------
;; Schemas (malli, plain data: validated in the tests, not at runtime)

(def Result
  "A finished jj command."
  [:map
   [:jj/exit :int]
   [:jj/out :string]
   [:jj/err :string]])

(def Failure [:map [:jj/error :string]])

(def Bookmark
  [:map
   [:bookmark/name :string]
   [:bookmark/remote [:maybe :string]]
   [:bookmark/ref {:doc "name, or name@remote"} :string]
   [:bookmark/commit {:doc "Short commit id, empty when conflicted"} :string]
   [:bookmark/description :string]])

(def Bookmarks [:or [:map [:jj/bookmarks [:vector Bookmark]]] Failure])
(def CommitIds [:or [:map [:jj/ids [:vector :string]]] Failure])
(def CommitId [:or [:map [:jj/id :string]] Failure])

;; ---------------------------------------------------------------------------
;; Commands

(defn run
  "Runs `jj <args>` in `dir`, returning a Result. jj never gets a terminal
   here, so it can't page, and JJ_EDITOR=false makes a command that
   unexpectedly wants an editor fail instead of hang."
  ([dir args] (run dir args {}))
  ([dir args {:keys [env]}]
   (try
     (let [{:keys [exit out err]} @(p/process (into ["jj" "--no-pager"] args)
                                              {:dir dir
                                               :in ""
                                               :out :string
                                               :err :string
                                               :extra-env (merge {"JJ_EDITOR" "false"} env)})]
       {:jj/exit exit :jj/out out :jj/err err})
     (catch Exception e
       {:jj/exit -1 :jj/out "" :jj/err (ex-message e)}))))

(defn- ok? [res] (zero? (:jj/exit res)))

(defn error-text
  "The useful part of a failed command's stderr."
  [{:jj/keys [err out exit]}]
  (let [text (str/trim (str err))]
    (cond
      (not (str/blank? text)) text
      (not (str/blank? out)) (str/trim out)
      :else (str "jj exited with " exit))))

(defn- error [res] {:jj/error (error-text res)})

(defn workspace-root [dir]
  (let [res (run dir ["workspace" "root"])]
    (when (ok? res)
      (str/trim (:jj/out res)))))

(defn base-template
  "The user's log template, as configured in jj."
  [dir]
  (let [res (run dir ["config" "get" "templates.log"])]
    (if (and (ok? res) (not (str/blank? (:jj/out res))))
      (str/trim (:jj/out res))
      "builtin_log_compact")))

(defn load-log
  "Runs jj log and parses it. Returns a jjtui.log/Log or a Failure."
  [dir {:keys [revset base-template files]}]
  (let [args (cond-> ["log" "--color=always" "-T" (log/template {:base base-template :files files})]
               revset (conj "-r" revset))
        res (run dir args)]
    (if (ok? res)
      (log/parse (:jj/out res))
      (error res))))

(def ^:private bookmark-template
  (str "if(self.remote() != \"git\", "
       "self.name() ++ \"\\x1f\" ++ coalesce(self.remote(), \"\") ++ \"\\x1f\" ++ "
       "if(self.normal_target(), self.normal_target().commit_id().short(8), \"\") ++ \"\\x1f\" ++ "
       "if(self.normal_target(), self.normal_target().description().first_line(), \"\") ++ \"\\n\")"))

(defn parse-bookmarks [out]
  (->> (str/split-lines out)
       (remove str/blank?)
       (map (fn [line]
              (let [[bname remote sha desc] (str/split line #"\u001f" -1)]
                {:bookmark/name bname
                 :bookmark/remote (not-empty remote)
                 :bookmark/ref (if (str/blank? remote) bname (str bname "@" remote))
                 :bookmark/commit sha
                 :bookmark/description desc})))
       distinct
       vec))

(defn bookmarks
  "All bookmarks, local and remote (except the internal @git ones)."
  [dir]
  (let [res (run dir ["bookmark" "list" "--all-remotes" "-T" bookmark-template])]
    (if (ok? res)
      {:jj/bookmarks (parse-bookmarks (:jj/out res))}
      (error res))))

(defn commit-ids
  "Commit ids of the commits in `revset`, newest first."
  [dir revset]
  (let [res (run dir ["log" "--no-graph" "-r" revset "-T" "commit_id ++ \"\\n\""])]
    (if (ok? res)
      {:jj/ids (vec (remove str/blank? (str/split-lines (:jj/out res))))}
      (error res))))

(defn first-parent
  "Commit id of the first parent of the single commit in `revset`."
  [dir revset]
  (let [res (run dir ["log" "--no-graph" "-r" revset "-T" "parents.map(|c| c.commit_id()).join(\",\") ++ \"\\n\""])]
    (if (ok? res)
      (let [lines (remove str/blank? (str/split-lines (:jj/out res)))]
        (if (not= 1 (count lines))
          {:jj/error (str "Expected one root commit, found " (count lines))}
          {:jj/id (first (str/split (first lines) #","))}))
      (error res))))
