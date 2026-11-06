(ns jjtui.config
  "Defaults, config file loading and command templating."
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(def ^:private paged-jj
  "jj invocation that always pages the diff and never quits on a short one, so
   output doesn't flash by when the TUI resumes."
  ["jj" "--config" "ui.paginate=auto" "--config" "ui.pager.command=[\"less\",\"-R\"]"])

(def defaults
  {:diff-tool :jj
   :diff-tools {:jj (into paged-jj ["diff" "-r" "$revset" "$files"])
                :difftastic (into paged-jj ["diff" "--tool" "difft" "-r" "$revset" "$files"])
                :delta "jj diff --git -r $revset $files | delta --paging=always"
                :diffview ["nvim" "-c" "DiffviewOpen $from-sha..$to-sha"]
                :difftool ["git" "difftool" "--dir-diff" "$from-sha" "$to-sha" "--" "$files"]}
   :preview-command ["jj" "diff" "--no-pager" "--color=always" "-r" "$revset" "$files"]
   :revset nil
   :log-template nil
   :show-files false
   :split false
   :split-ratio 0.5})

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
  "User config over the defaults. Nested maps merge, so adding one diff tool
   keeps the built-in ones."
  [user]
  (deep-merge defaults (or user {})))

(defn load-config
  "Returns {:config merged-config :error message-or-nil :path path}."
  [override]
  (let [path (config-path override)]
    (if-not (fs/exists? path)
      {:config defaults :path path}
      (try
        {:config (merge-config (edn/read-string {:readers {'env #(System/getenv (str %))}}
                                                (slurp path)))
         :path path}
        (catch Exception e
          {:config defaults
           :path path
           :error (str "Could not read " path ": " (ex-message e))})))))

;; ---------------------------------------------------------------------------
;; Command templates

(defn tool-spec
  "Normalizes a diff tool entry to {:cmd string-or-vector :wait? bool}. A tool
   is a string, a vector, or a map with :cmd and an optional :wait?, which asks
   for a keypress before returning to the TUI (for tools that print and exit)."
  [tool]
  (if (map? tool)
    {:cmd (:cmd tool) :wait? (boolean (:wait? tool))}
    {:cmd tool :wait? false}))

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
