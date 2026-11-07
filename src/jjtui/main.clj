(ns jjtui.main
  (:require
   [babashka.process :as p]
   [charm.program :as program]
   [clojure.string :as str]
   [jjtui.app :as app]
   [jjtui.config :as config]
   [jjtui.effects :as effects]
   [jjtui.history :as history]
   [jjtui.repo :as repo]
   [jjtui.view :as view])
  (:import
   [org.jline.terminal TerminalBuilder]))

(def usage
  "Usage: jjtui [options]

Interactive TUI for jujutsu repositories. Run it inside a jj repo; in a git
repo without jj it offers to set jj up, or to open another repo.

Options:
  -r, --revset REVSET   Log revset (default: jj's revsets.log)
      --config PATH     Config file (default: ~/.config/jjtui/config.edn)
  -h, --help            Show this help

Press ? inside the TUI for keybindings.")

(defn parse-args [args]
  (loop [[a & more] args
         opts {}]
    (cond
      (nil? a) opts
      (#{"-h" "--help"} a) (recur more (assoc opts :help true))
      (#{"-r" "--revset"} a) (recur (rest more) (assoc opts :revset (first more)))
      (= "--config" a) (recur (rest more) (assoc opts :config (first more)))
      :else (recur more (update opts :errors (fnil conj []) (str "Unknown argument: " a))))))

(defn- wait-for-key []
  (with-open [terminal (-> (TerminalBuilder/builder) (.system true) (.build))]
    (let [attrs (.enterRawMode terminal)]
      (try
        (.read (.reader terminal))
        (finally
          (.setAttributes terminal attrs))))))

(defn- run-interactive
  "Runs a command on the real terminal while the TUI is stopped."
  [dir {:exec/keys [argv wait?]}]
  (println (str "\u001b[2m$ " (str/join " " argv) "\u001b[0m"))
  (let [exit (try
               (:exit @(p/process argv {:dir dir :inherit true}))
               (catch Exception e
                 (binding [*out* *err*]
                   (println (ex-message e)))
                 127))]
    (when (or wait? (not (zero? exit)))
      (print "\n\u001b[2mPress any key to return to jjtui\u001b[0m")
      (flush)
      (wait-for-key))
    exit))

(defn- run-program
  "Runs the charm program until the user quits or an interactive command needs
   the terminal. Returns the final state."
  [[state effects]]
  (program/run {:init (fn [] [state (effects/->cmd state effects)])
                :update (fn [state msg]
                          (let [[state effects] (app/update-state state msg)]
                            [state (effects/->cmd state effects)]))
                :view view/render
                :alt-screen true
                :overflow :bottom
                ;; Asked once; restarts reuse the answer instead of re-querying
                :dark-background? (:app/dark-known state)}))

(defn- fail! [message]
  (binding [*out* *err*]
    (println (str "jjtui: " message)))
  (System/exit 1))

(defn- start-state
  "The initial state for `cwd`: its jj repo, or the repo chooser when it's in
   a git repo without jj."
  [cwd config config-error]
  (let [[kind found] (repo/find-start cwd)
        init #(app/init-state (merge {:config config :config-error config-error} %))]
    (case kind
      :open (let [opened (repo/open found (:log-template config))]
              (when-let [err (:jj/error opened)] (fail! err))
              (init {:repo opened}))
      :choose (let [{:keys [git-root parent]} found
                    now (System/currentTimeMillis)
                    recent (history/suggestions (history/read-visits (history/default-path))
                                                now
                                                (set (remove nil? [git-root parent])))]
                (init {:mode (app/chooser {:git-root git-root :parent parent :recent recent :now now})}))
      (fail! "not inside a jj or git repository"))))

(defn -main [& args]
  (let [opts (parse-args args)]
    (cond
      (:help opts) (println usage)

      (:errors opts)
      (do (binding [*out* *err*]
            (run! println (:errors opts))
            (println)
            (println usage))
          (System/exit 2))

      :else
      (let [{:keys [config error]} (config/load-config (:config opts))
            config (cond-> config (:revset opts) (assoc :revset (:revset opts)))]
        (loop [step (app/start (start-state (System/getProperty "user.dir") config error))]
          (let [state (run-program step)]
            (when-let [exec (:app/exec state)]
              (let [exit (run-interactive (get-in state [:app/repo :repo/dir]) exec)]
                (recur (app/after-exec (assoc state :app/dark-known (:app/dark? state)) {:exit exit}))))))))))
