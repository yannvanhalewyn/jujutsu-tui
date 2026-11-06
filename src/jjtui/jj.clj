(ns jjtui.jj
  "Runs jj. Everything here does I/O; command construction lives in
   jjtui.app and jjtui.log so it can be tested without a repo."
  (:require
   [babashka.process :as p]
   [clojure.string :as str]
   [jjtui.log :as log]))

(defn run
  "Runs `jj <args>` in `dir`, returning {:exit :out :err}. jj never gets a
   terminal here, so it can't page, and JJ_EDITOR=false makes a command that
   unexpectedly wants an editor fail instead of hang."
  ([dir args] (run dir args {}))
  ([dir args {:keys [env]}]
   (try
     (-> (p/process (into ["jj" "--no-pager"] args)
                    {:dir dir
                     :in ""
                     :out :string
                     :err :string
                     :extra-env (merge {"JJ_EDITOR" "false"} env)})
         deref
         (select-keys [:exit :out :err]))
     (catch Exception e
       {:exit -1 :out "" :err (ex-message e)}))))

(defn error-text
  "The useful part of a failed command's stderr."
  [{:keys [err out exit]}]
  (let [text (str/trim (str err))]
    (cond
      (not (str/blank? text)) text
      (not (str/blank? out)) (str/trim out)
      :else (str "jj exited with " exit))))

(defn workspace-root [dir]
  (let [res (run dir ["workspace" "root"])]
    (when (zero? (:exit res))
      (str/trim (:out res)))))

(defn base-template
  "The user's log template, as configured in jj."
  [dir]
  (let [res (run dir ["config" "get" "templates.log"])]
    (if (and (zero? (:exit res)) (not (str/blank? (:out res))))
      (str/trim (:out res))
      "builtin_log_compact")))

(defn load-log
  "Runs jj log and parses it. Returns {:entries :rows} or {:error text}."
  [dir {:keys [revset base-template files]}]
  (let [args (cond-> ["log" "--color=always" "-T" (log/template {:base base-template :files files})]
               revset (conj "-r" revset))
        res (run dir args)]
    (if (zero? (:exit res))
      (log/parse (:out res))
      {:error (error-text res)})))

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
                {:name bname
                 :remote (not-empty remote)
                 :ref (if (str/blank? remote) bname (str bname "@" remote))
                 :commit sha
                 :description desc})))
       distinct
       vec))

(defn bookmarks
  "All bookmarks, local and remote (except the internal @git ones)."
  [dir]
  (let [res (run dir ["bookmark" "list" "--all-remotes" "-T" bookmark-template])]
    (if (zero? (:exit res))
      {:bookmarks (parse-bookmarks (:out res))}
      {:error (error-text res)})))

(defn commit-ids
  "Commit ids of the commits in `revset`, newest first."
  [dir revset]
  (let [res (run dir ["log" "--no-graph" "-r" revset "-T" "commit_id ++ \"\\n\""])]
    (if (zero? (:exit res))
      {:ids (vec (remove str/blank? (str/split-lines (:out res))))}
      {:error (error-text res)})))

(defn first-parent
  "Commit id of the first parent of the single commit in `revset`."
  [dir revset]
  (let [res (run dir ["log" "--no-graph" "-r" revset "-T" "parents.map(|c| c.commit_id()).join(\",\") ++ \"\\n\""])]
    (if (zero? (:exit res))
      (let [lines (remove str/blank? (str/split-lines (:out res)))]
        (cond
          (not= 1 (count lines)) {:error (str "Expected one root commit, found " (count lines))}
          :else {:id (first (str/split (first lines) #","))}))
      {:error (error-text res)})))
