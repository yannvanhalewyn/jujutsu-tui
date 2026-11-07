(ns jjtui.config
  "Defaults, config file loading and command templating.

   Config comes in two shapes:

   - User config: what config.edn holds and the docs describe. Keys are
     unqualified (:diff-tool, :split …) to keep the file short, and values
     allow sugar: a diff tool is a bare string or vector, or a map
     {:cmd … :wait? true} when it needs options; flags are any truthy value.
     `defaults` and `merge-config` work in this shape, so the defaults read
     like a config file.
   - Internal config (`Config`): what the app keeps under :app/config. Keys
     are qualified (:config/, :tool/) like the rest of the app state, and the
     sugar is expanded so there is one form per value: every diff tool is
     {:tool/cmd … :tool/wait? …}, flags are booleans.

   `user->internal` is the only bridge; nothing past `load-config` sees user
   config."
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(def ^:private paged-jj
  "jj invocation that always pages the diff and never quits on a short one, so
   output doesn't flash by when the TUI resumes."
  ["jj" "--config" "ui.paginate=auto" "--config" "ui.pager.command=[\"less\",\"-R\"]"])

;; ---------------------------------------------------------------------------
;; Internal config

(def Command
  "A command template: a string run through `sh -c`, or an argv vector."
  [:or :string [:sequential :string]])

(def Tool
  [:map {:closed true}
   [:tool/cmd Command]
   [:tool/wait? {:doc "Wait for a key after the tool exits"} :boolean]])

(def Config
  [:map {:closed true}
   [:config/diff-tool {:doc "Initially active key of :config/diff-tools"} :keyword]
   [:config/diff-tools [:map-of :keyword Tool]]
   [:config/preview-command Command]
   [:config/revset {:doc "nil = jj's revsets.log"} [:maybe :string]]
   [:config/log-template {:doc "nil = jj's templates.log"} [:maybe :string]]
   [:config/show-files? :boolean]
   [:config/split? :boolean]
   [:config/split-ratio number?]])

;; ---------------------------------------------------------------------------
;; User config

(def defaults
  "The user config used for keys config.edn leaves out. `presets` add to
   :diff-tools when their program is installed."
  {:diff-tool :jj
   :diff-tools {:jj (into paged-jj ["diff" "-r" "$revset" "$files"])
                :difftool ["git" "difftool" "--dir-diff" "$from-sha" "$to-sha" "--" "$files"]}
   :preview-command ["jj" "diff" "--no-pager" "--color=always" "-r" "$revset" "$files"]
   :revset nil
   :log-template nil
   :show-files false
   :split false
   :split-ratio 0.5})

(def presets
  "Diff tools offered when `:program` is on PATH. A key config.edn also defines
   keeps the user's tool. The GUI tools run through jj's built-in
   merge-tools.<name> configs, so no colocated git repo is needed."
  {:difftastic {:program "difft"
                :tool (into paged-jj ["diff" "--tool" "difft" "-r" "$revset" "$files"])}
   :delta {:program "delta"
           :tool "jj diff --git -r $revset $files | delta --paging=always"}
   :diff-so-fancy {:program "diff-so-fancy"
                   :tool "jj diff --git --color=always -r $revset $files | diff-so-fancy | less -R"}
   :meld {:program "meld"
          :tool ["jj" "diff" "--tool" "meld" "-r" "$revset" "$files"]}
   :kdiff3 {:program "kdiff3"
            :tool ["jj" "diff" "--tool" "kdiff3" "-r" "$revset" "$files"]}
   :vscode {:program "code"
            :tool ["jj" "diff" "--tool" "vscode" "-r" "$revset" "$files"]}})

(defn detected-tools
  "The `presets` whose program `installed?` accepts, as user-config tools."
  [installed?]
  (into {} (for [[k {:keys [program tool]}] presets
                 :when (installed? program)]
             [k tool])))

(defn- on-path? [program]
  (some? (fs/which program)))

(defn config-path
  "The config file to read: `override`, else $JJTUI_CONFIG, else
   $XDG_CONFIG_HOME/jjtui/config.edn."
  [override]
  (str (or override
           (System/getenv "JJTUI_CONFIG")
           (fs/path (fs/xdg-config-home) "jjtui" "config.edn"))))

(defn- deep-merge [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn merge-config
  "User config over the defaults plus the presets `installed?` finds (default:
   programs on PATH). Nested maps merge, so adding one diff tool keeps the
   built-in ones, and a user tool replaces a preset of the same key."
  ([user] (merge-config user on-path?))
  ([user installed?]
   (deep-merge (update defaults :diff-tools merge (detected-tools installed?))
               (or user {}))))

(defn- user->tool
  "A diff tool is a command template, or {:cmd template :wait? bool}."
  [tool]
  (if (map? tool)
    {:tool/cmd (:cmd tool) :tool/wait? (boolean (:wait? tool))}
    {:tool/cmd tool :tool/wait? false}))

(defn user->internal
  "Translates a complete user config (see `merge-config`) into a `Config`,
   expanding its sugar."
  [user]
  {:config/diff-tool (:diff-tool user)
   :config/diff-tools (update-vals (:diff-tools user) user->tool)
   :config/preview-command (:preview-command user)
   :config/revset (:revset user)
   :config/log-template (:log-template user)
   :config/show-files? (boolean (:show-files user))
   :config/split? (boolean (:split user))
   :config/split-ratio (:split-ratio user)})

(defn load-config
  "Reads the user config at `override` (see `config-path`). Returns
   {:config Config :error message-or-nil :path path}; on error the config is
   the defaults."
  [override]
  (let [path (config-path override)]
    (if-not (fs/exists? path)
      {:config (user->internal (merge-config nil)) :path path}
      (try
        {:config (user->internal
                  (merge-config (edn/read-string {:readers {'env #(System/getenv (str %))}}
                                                 (slurp path))))
         :path path}
        (catch Exception e
          {:config (user->internal (merge-config nil))
           :path path
           :error (str "Could not read " path ": " (ex-message e))})))))

;; ---------------------------------------------------------------------------
;; Command templates

(defn shell-quote [s]
  (str "'" (str/replace s "'" "'\\''") "'"))

(def ^:private var-pattern #"\$([a-z][a-z0-9-]*[a-z0-9])")

(defn uses-var?
  "True when the command template references $`var-name`."
  [cmd var-name]
  (let [s (if (string? cmd) cmd (str/join " " cmd))]
    (boolean (some #(= var-name (second %)) (re-seq var-pattern s)))))

(defn- substitute [s vars quote-fn]
  (str/replace s var-pattern
               (fn [[whole k]]
                 (let [v (get vars k ::missing)]
                   (cond
                     (= ::missing v) whole
                     (sequential? v) (str/join " " (map quote-fn v))
                     :else (quote-fn (str v)))))))

(defn render-command
  "Turns a command template into an argv vector.

   `vars` maps variable names (without $) to strings, or to a seq of strings
   for $files. A string template runs through `sh -c`, with every substituted
   value shell-quoted. In a vector template an element that is exactly
   \"$files\" expands to zero or more arguments."
  [cmd vars]
  (if (string? cmd)
    ["sh" "-c" (substitute cmd vars shell-quote)]
    (into []
          (mapcat (fn [arg]
                    (let [whole (re-matches var-pattern arg)
                          v (when whole (get vars (second whole)))]
                      (if (sequential? v)
                        v
                        [(substitute arg vars identity)]))))
          cmd)))
