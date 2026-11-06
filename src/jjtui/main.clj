(ns jjtui.main
  (:require
   [babashka.process :as p]
   [charm.program :as program]
   [clojure.string :as str]
   [jjtui.app :as app]
   [jjtui.config :as config]
   [jjtui.effects :as effects]
   [jjtui.jj :as jj]
   [jjtui.view :as view])
  (:import
   [org.jline.terminal TerminalBuilder]))

(def usage
  "Usage: jjtui [options]

Interactive TUI for jujutsu repositories. Run it inside a jj repo.

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
  [dir {:keys [argv wait?]}]
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
                :dark-background? (:dark-known state)}))

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
      (let [cwd (System/getProperty "user.dir")
            root (jj/workspace-root cwd)]
        (when-not root
          (binding [*out* *err*]
            (println "jjtui: not inside a jj repository (jj workspace root failed)"))
          (System/exit 1))
        (let [{:keys [config error]} (config/load-config (:config opts))]
          (loop [step (app/start (app/init-state {:dir root
                                                  :config config
                                                  :config-error error
                                                  :revset (:revset opts)
                                                  :base-template (or (:log-template config)
                                                                     (jj/base-template root))}))]
            (let [state (run-program step)]
              (when-let [exec (:exec state)]
                (let [exit (run-interactive root exec)]
                  (recur (app/after-exec (assoc state :dark-known (:dark? state)) {:exit exit})))))))))))
