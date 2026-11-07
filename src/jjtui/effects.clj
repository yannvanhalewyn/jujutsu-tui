(ns jjtui.effects
  "Turns the effect maps returned by jjtui.app into charm commands. This is
   the only place the running program does I/O.

   Effects dispatch on :fx/type. Their results come back as messages whose
   charm :type is a :msg/ keyword, which keeps them apart from charm's own
   messages (:key-press, :window-size …)."
  (:require
   [babashka.process :as p]
   [charm.program :as program]
   [clojure.string :as str]
   [jjtui.history :as history]
   [jjtui.jj :as jj]
   [jjtui.log :as log]
   [jjtui.repo :as repo]
   [jjtui.state :as st]))

;; ---------------------------------------------------------------------------
;; Schemas (malli, plain data: validated in the tests, not at runtime)

(def JjEffect
  "Runs jj in the background. Its result message carries the effect back."
  [:map
   [:fx/type [:= :jj]]
   [:fx/args [:vector :string]]
   [:fx/then {:optional true :doc "More jj commands, each run once the previous one succeeded"} [:vector [:vector :string]]]
   [:fx/success {:doc "Message on success, else jj's output"} [:maybe :string]]
   [:fx/clear-marks? [:maybe :boolean]]])

(def BookmarkPurpose [:enum :branch-diff :bookmark-set :push :pull])

(def Msg
  "The messages effects send back to jjtui.app/update-state."
  [:multi {:dispatch :type}
   [:msg/log-loaded [:map
                     [:type [:= :msg/log-loaded]]
                     [:msg/gen :int]
                     [:msg/result [:or log/Log jj/Failure]]]]
   [:msg/jj-done [:map
                  [:type [:= :msg/jj-done]]
                  [:msg/effect JjEffect]
                  [:msg/result jj/Result]]]
   [:msg/bookmarks-loaded [:map
                           [:type [:= :msg/bookmarks-loaded]]
                           [:msg/purpose BookmarkPurpose]
                           [:msg/result jj/Bookmarks]]]
   [:msg/description-loaded [:map
                             [:type [:= :msg/description-loaded]]
                             [:msg/change-id :string]
                             [:msg/result jj/Description]]]
   [:msg/recent-repos-loaded [:map
                              [:type [:= :msg/recent-repos-loaded]]
                              [:msg/repos {:doc "Best first"} [:vector history/Suggestion]]
                              [:msg/now {:doc "Epoch millis the repos were ranked at"} :int]]]
   [:msg/diff-resolved [:map
                        [:type [:= :msg/diff-resolved]]
                        [:msg/result [:or st/DiffTarget jj/Failure]]]]
   [:msg/repo-opened [:map
                      [:type [:= :msg/repo-opened]]
                      [:msg/result [:or repo/Opened jj/Failure]]]]
   [:msg/preview-fetch [:map
                        [:type [:= :msg/preview-fetch]]
                        [:msg/key st/PreviewKey]]]
   [:msg/preview-loaded [:map
                         [:type [:= :msg/preview-loaded]]
                         [:msg/key st/PreviewKey]
                         [:msg/lines [:vector :string]]]]])

(def Effect
  [:multi {:dispatch :fx/type}
   [:quit [:map [:fx/type [:= :quit]]]]
   [:load-log [:map
               [:fx/type [:= :load-log]]
               [:fx/gen :int]
               [:fx/params {:doc "Options of jjtui.jj/load-log"}
                [:map
                 [:revset [:maybe :string]]
                 [:base-template :string]
                 [:files [:or [:= :all] :string :nil]]]]]]
   [:jj JjEffect]
   [:bookmarks [:map [:fx/type [:= :bookmarks]] [:fx/purpose BookmarkPurpose]]]
   [:description [:map [:fx/type [:= :description]] [:fx/change-id :string]]]
   [:recent-repos [:map [:fx/type [:= :recent-repos]] [:fx/exclude {:doc "Dirs to leave out"} [:set :string]]]]
   [:resolve-diff [:map [:fx/type [:= :resolve-diff]] [:fx/target st/DiffTarget]]]
   [:debounce [:map [:fx/type [:= :debounce]] [:fx/ms :int] [:fx/msg Msg]]]
   [:preview [:map [:fx/type [:= :preview]] [:fx/key st/PreviewKey]]]
   [:open-repo [:map [:fx/type [:= :open-repo]] [:fx/dir :string]]]
   [:init-repo {:doc "jj git init --colocate, then open"} [:map [:fx/type [:= :init-repo]] [:fx/dir :string]]]])

;; ---------------------------------------------------------------------------
;; Running effects

(def ^:private max-preview-lines 20000)

(defn- preview-lines [out]
  (->> (str/split-lines (str/replace out "\r" ""))
       (take max-preview-lines)
       (mapv #(str/replace % "\t" "    "))))

(defn- run-preview [dir argv width]
  (try
    (let [{:keys [out err exit]} @(p/process argv {:dir dir
                                                    :in ""
                                                    :out :string
                                                    :err :string
                                                    :extra-env {"COLUMNS" (str width)
                                                                "JJ_EDITOR" "false"}})]
      (preview-lines (if (and (zero? exit) (seq out)) out (str out err))))
    (catch Exception e
      [(str "Preview command failed: " (ex-message e))])))

(defn- resolve-diff
  "Fills :target/from-sha and :target/to-sha of a diff target from its
   :target/from-rev and :target/to-rev. Returns the target or a jj/Failure."
  [dir {:target/keys [from-rev to-rev from-commit?] :as target}]
  (let [to (jj/commit-ids dir to-rev)
        from (if from-commit?
               (let [r (jj/commit-ids dir from-rev)]
                 (if (:jj/error r) r {:jj/id (first (:jj/ids r))}))
               (jj/first-parent dir from-rev))]
    (cond
      (:jj/error to) to
      (not= 1 (count (:jj/ids to))) {:jj/error (str "The selection has " (count (:jj/ids to)) " heads; the diff tool needs exactly one")}
      (:jj/error from) from
      (str/blank? (:jj/id from)) {:jj/error "The selection starts at the root commit, which has no parent"}
      :else (assoc target :target/from-sha (:jj/id from) :target/to-sha (first (:jj/ids to))))))

(defn- effect->cmd [state effect]
  (let [dir (get-in state [:app/repo :repo/dir])
        log-template (get-in state [:app/config :config/log-template])]
    (case (:fx/type effect)
      :quit program/quit-cmd

      :load-log
      (program/cmd #(hash-map :type :msg/log-loaded
                              :msg/gen (:fx/gen effect)
                              :msg/result (jj/load-log dir (:fx/params effect))))

      :jj
      (program/cmd #(hash-map :type :msg/jj-done
                              :msg/effect effect
                              :msg/result (reduce (fn [res args]
                                                    (if (zero? (:jj/exit res)) (jj/run dir args) (reduced res)))
                                                  (jj/run dir (:fx/args effect))
                                                  (:fx/then effect))))

      :bookmarks
      (program/cmd #(hash-map :type :msg/bookmarks-loaded
                              :msg/purpose (:fx/purpose effect)
                              :msg/result (jj/bookmarks dir)))

      :description
      (program/cmd #(hash-map :type :msg/description-loaded
                              :msg/change-id (:fx/change-id effect)
                              :msg/result (jj/description dir (:fx/change-id effect))))

      :resolve-diff
      (program/cmd #(hash-map :type :msg/diff-resolved
                              :msg/result (resolve-diff dir (:fx/target effect))))

      :recent-repos
      (program/cmd #(let [now (System/currentTimeMillis)]
                      {:type :msg/recent-repos-loaded
                       :msg/repos (history/suggestions (history/read-visits (history/default-path))
                                                       now
                                                       (:fx/exclude effect))
                       :msg/now now}))

      :debounce
      (program/cmd (fn [] (Thread/sleep (long (:fx/ms effect))) (:fx/msg effect)))

      :preview
      (program/cmd #(let [[argv width :as k] (:fx/key effect)]
                      {:type :msg/preview-loaded
                       :msg/key k
                       :msg/lines (run-preview dir argv width)}))

      :open-repo
      (program/cmd #(hash-map :type :msg/repo-opened
                              :msg/result (repo/open (:fx/dir effect) log-template)))

      :init-repo
      (program/cmd #(hash-map :type :msg/repo-opened
                              :msg/result (repo/init-colocated (:fx/dir effect) log-template))))))

(defn ->cmd
  "One charm command for a list of effects. Quitting wins over everything
   else, since the program is about to stop."
  [state effects]
  (cond
    (empty? effects) nil
    (some #(= :quit (:fx/type %)) effects) program/quit-cmd
    (= 1 (count effects)) (effect->cmd state (first effects))
    :else (apply program/batch (map #(effect->cmd state %) effects))))
