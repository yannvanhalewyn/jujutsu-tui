(ns jjtui.effects
  "Turns the effect maps returned by jjtui.app into charm commands. This is
   the only place the running program does I/O."
  (:require
   [babashka.process :as p]
   [charm.program :as program]
   [clojure.string :as str]
   [jjtui.jj :as jj]))

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
  "Fills :from-sha and :to-sha of a diff target from its :from-rev/:to-rev."
  [dir {:keys [from-rev to-rev from-is-commit?] :as target}]
  (let [to (jj/commit-ids dir to-rev)
        from (if from-is-commit?
               (let [r (jj/commit-ids dir from-rev)]
                 (if (:error r) r {:id (first (:ids r))}))
               (jj/first-parent dir from-rev))]
    (cond
      (:error to) {:error (:error to)}
      (not= 1 (count (:ids to))) {:error (str "The selection has " (count (:ids to)) " heads; the diff tool needs exactly one")}
      (:error from) {:error (:error from)}
      (str/blank? (:id from)) {:error "The selection starts at the root commit, which has no parent"}
      :else {:target (assoc target :from-sha (:id from) :to-sha (first (:ids to)))})))

(defn- effect->cmd [state {:keys [fx] :as effect}]
  (let [dir (:dir state)]
    (case fx
      :quit program/quit-cmd

      :load-log
      (program/cmd #(hash-map :type :log-loaded
                              :gen (:gen effect)
                              :result (jj/load-log dir (:params effect))))

      :jj
      (program/cmd #(hash-map :type :jj-done
                              :fx effect
                              :result (jj/run dir (:args effect))))

      :bookmarks
      (program/cmd #(hash-map :type :bookmarks-loaded
                              :purpose (:purpose effect)
                              :result (jj/bookmarks dir)))

      :resolve-diff
      (program/cmd #(merge {:type :diff-resolved} (resolve-diff dir (:target effect))))

      :debounce
      (program/cmd (fn [] (Thread/sleep (long (:ms effect))) (:msg effect)))

      :preview
      (program/cmd #(hash-map :type :preview-loaded
                              :key (:key effect)
                              :lines (run-preview dir (:argv effect) (:width effect)))))))

(defn ->cmd
  "One charm command for a list of effects. Quitting wins over everything
   else, since the program is about to stop."
  [state effects]
  (cond
    (empty? effects) nil
    (some #(= :quit (:fx %)) effects) program/quit-cmd
    (= 1 (count effects)) (effect->cmd state (first effects))
    :else (apply program/batch (map #(effect->cmd state %) effects))))
