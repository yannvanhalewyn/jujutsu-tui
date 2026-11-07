(ns jjtui.app-test
  (:require
   [charm.components.text-input :as ti]
   [charm.message :as msg]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [jjtui.app :as app]
   [jjtui.config :as config]
   [jjtui.effects :as effects]
   [jjtui.log :as log]
   [jjtui.log-test :refer [file-line marker]]
   [jjtui.state :as st]
   [jjtui.view :as view]
   [malli.core :as m]
   [malli.error :as me]))

(def log-output
  (str/join "\n"
            [(str "@  " (marker {:id "wwww" :commit "cw" :parents ["cb"] :wc? true}) "wwww")
             "│  (no description set)"
             (str "○  " (marker {:id "bbbb" :commit "cb" :parents ["ca"] :description "B" :bookmarks ["feat"] :conflict? true}) "bbbb")
             "│  B"
             (file-line "│  " "modified" "b.clj")
             (str "○  " (marker {:id "aaaa" :commit "ca" :parents ["cm"] :description "A" :full-description "A  Fixes the Login bug"}) "aaaa")
             "│  A"
             (str "◆  " (marker {:id "mmmm" :commit "cm" :parents [] :immutable? true :description "M"}) "mmmm")
             "│  M"]))

(defn- checked
  "Asserts that `x` matches `schema`, returning it."
  [schema x]
  (is (nil? (some-> (m/explain schema x) me/humanize)) (str "matches " (m/form schema)))
  x)

(defn- step
  "update-state, checking the message, the new state and its effects against
   their schemas. charm's own messages aren't ours to check."
  [state message]
  (when (= "msg" (namespace (:type message)))
    (checked effects/Msg message))
  (let [[state fx] (app/update-state state message)]
    [(checked st/State state) (mapv #(checked effects/Effect %) fx)]))

(defn- loaded-msg [gen output]
  {:type :msg/log-loaded :msg/gen gen :msg/result (log/parse output)})

(defn- loaded
  "A state with the sample log loaded, cursor on the working copy."
  []
  (let [s (checked st/State (assoc (app/init-state {:config (config/user->internal config/defaults)
                                                    :repo {:repo/dir "/repo" :repo/base-template "t"}})
                                   :app/width 100 :app/height 30))
        [s [fx]] (app/start s)]
    (first (step s (loaded-msg (:fx/gen fx) log-output)))))

(defn- press
  "Feeds keys through update-state. Returns [state effects-of-last-key]."
  [state & ks]
  (reduce (fn [[s _] k]
            (step s (cond
                      (map? k) k
                      (= "enter" k) (msg/key-press :enter)
                      (= "escape" k) (msg/key-press :escape)
                      :else (msg/key-press k))))
          [state []]
          ks))

(defn- cursor-id [s] (:change/id (st/cursor-entry s)))
(defn- jj-args [effects] (:fx/args (first (filter #(= :jj (:fx/type %)) effects))))

(deftest navigation-test
  (let [s (loaded)]
    (is (= "wwww" (cursor-id s)) "starts on the working copy")
    (testing "j visits file rows, which belong to their change"
      (let [[s] (press s "j" "j")]
        (is (= "bbbb" (cursor-id s)))
        (is (= "b.clj" (get-in s [:app/repo :repo/cursor :file/path])))))
    (testing "G and g jump to the ends"
      (is (= "mmmm" (cursor-id (first (press s "G")))))
      (is (= "wwww" (cursor-id (first (press s "G" "g"))))))))

(deftest search-test
  (let [s (loaded)]
    (testing "matches change id and commit id prefixes, and any description line ignoring case"
      (is (= "aaaa" (cursor-id (first (press s "/" "a" "a")))))
      (is (= "mmmm" (cursor-id (first (press s "/" "c" "m")))))
      (is (= "aaaa" (cursor-id (first (press s "/" "L" "O" "G" "I" "N"))))))
    (testing "typing jumps to the first match from where the search started, wrapping"
      (is (= "bbbb" (cursor-id (first (press s "/" "b")))))
      (is (= "aaaa" (cursor-id (first (press s "j" "j" "j" "/" "b")))) "the cursor's own change counts")
      (is (= "bbbb" (cursor-id (first (press s "G" "/" "b"))))))
    (testing "down and up step through the matches, wrapping"
      (is (= ["aaaa" "bbbb" "aaaa"]
             (map #(cursor-id (first (apply press s "/" "b" %))) [["down"] ["down" "down"] ["up"]]))))
    (testing "a query matching nothing goes back to where the search started"
      (is (= "wwww" (cursor-id (first (press s "/" "a" "z"))))))
    (testing "enter keeps the cursor on the match and activates the search, escape goes back"
      (let [[s'] (press s "/" "b" "down" "enter")]
        (is (nil? (:app/mode s')))
        (is (= "aaaa" (cursor-id s')))
        (is (= "b" (get-in s' [:app/repo :repo/search]))))
      (let [[s'] (press s "/" "b" "escape")]
        (is (nil? (:app/mode s')))
        (is (= "wwww" (cursor-id s')))
        (is (nil? (get-in s' [:app/repo :repo/search])))))
    (testing "enter without a match says so and leaves no active search"
      (let [[s'] (press s "/" "z" "enter")]
        (is (= :error (get-in s' [:app/message :message/level])))
        (is (nil? (get-in s' [:app/repo :repo/search])))))))

(deftest active-search-test
  (let [[s] (press (loaded) "/" "b" "enter")]
    (is (= "bbbb" (cursor-id s)))
    (testing "n and N step through the matches, wrapping, instead of creating changes"
      (let [[s' fx] (press s "n")]
        (is (= "aaaa" (cursor-id s')))
        (is (empty? fx)))
      (is (= "bbbb" (cursor-id (first (press s "n" "n")))))
      (is (= "aaaa" (cursor-id (first (press s "N"))))))
    (testing "n and N also move while picking a target"
      (let [[s'] (press s "r")
            [s''] (press s' "n")]
        (is (= :pick (get-in s'' [:app/mode :mode/type])))
        (is (= "aaaa" (cursor-id s')) "r steps past the source")
        (is (= "bbbb" (cursor-id s'')) "n wraps to the first match")))
    (testing "/ starts from the active query; escape keeps it"
      (is (= "b" (ti/value (get-in (first (press s "/")) [:app/mode :mode/input]))))
      (is (= "b" (get-in (first (press s "/" "x" "escape")) [:app/repo :repo/search])))
      (is (nil? (get-in (first (press s "/" "x" "enter")) [:app/repo :repo/search])) "bx matches nothing"))
    (testing "escape clears the search before the marks, then n creates a change again"
      (let [[s'] (press s "m" "escape")]
        (is (nil? (get-in s' [:app/repo :repo/search])))
        (is (= #{"bbbb"} (get-in s' [:app/repo :repo/marks])))
        (is (empty? (get-in (first (press s' "escape")) [:app/repo :repo/marks])))
        (is (= "new" (first (jj-args (second (press s' "n"))))))))))

(deftest cursor-survives-reload-test
  (let [[s] (press (loaded) "j")
        ;; A new change on top shifts every row down
        shifted (str "○  " (marker {:id "nnnn" :commit "cn" :parents ["cw"]}) "nnnn\n│  N\n" log-output)
        [s] (step (assoc s :app/log-gen 7) (loaded-msg 7 shifted))]
    (is (= "bbbb" (cursor-id s)))
    (testing "results of an older load are dropped"
      (let [[s2] (step s (loaded-msg 3 ""))]
        (is (= (get-in s [:app/repo :repo/log]) (get-in s2 [:app/repo :repo/log])))))))

(deftest marks-test
  (let [[s] (press (loaded) "m")]
    (is (= #{"wwww"} (get-in s [:app/repo :repo/marks])))
    (is (= "bbbb" (cursor-id s)) "marking moves to the next change")
    (is (nil? (get-in s [:app/repo :repo/cursor :file/path])) "skipping its files")
    (let [[s] (press s "m")]
      (is (= #{"wwww" "bbbb"} (get-in s [:app/repo :repo/marks])))
      (testing "batch actions use the marks"
        (let [[_ fx] (press s "n")]
          (is (= ["new" "wwww" "bbbb"] (jj-args fx)))))
      (testing "esc clears"
        (is (empty? (get-in (first (press s "escape")) [:app/repo :repo/marks])))))))

(deftest rebase-pick-test
  (let [[s] (press (loaded) "r")
        cursor-id #(:change/id (st/cursor-entry %))]
    (is (= :pick (get-in s [:app/mode :mode/type])))
    (testing "r makes the cursor's change the source and steps to the next change"
      (is (= ["wwww"] (get-in s [:app/mode :mode/sources])))
      (is (= "bbbb" (cursor-id s))))
    (testing "with marks the cursor isn't a source, so it stays"
      (let [[s] (press (loaded) "j" "j" "j" "m" "g" "r")]
        (is (= ["aaaa"] (get-in s [:app/mode :mode/sources])))
        (is (= "wwww" (cursor-id s)))))
    (testing "s cycles the source mode, a inserts after the target"
      (let [[s fx] (press s "s" "j" "j" "a")]
        (is (nil? (:app/mode s)))
        (is (= ["rebase" "-s" "wwww" "-A" "aaaa"] (jj-args fx)))))
    (testing "enter rebases onto the target"
      (let [[_ fx] (press s "G" "enter")]
        (is (= ["rebase" "-r" "wwww" "-d" "mmmm"] (jj-args fx)))))
    (testing "can't rebase onto a source"
      (let [[s fx] (press s "g" "enter")]
        (is (empty? fx))
        (is (= :error (get-in s [:app/message :message/level])))))
    (testing "chained r adds the cursor's change to the sources and steps past its files to the next change"
      (let [[s] (press s "r")]
        (is (= ["wwww" "bbbb"] (get-in s [:app/mode :mode/sources])))
        (is (= "aaaa" (cursor-id s)))
        (testing "the source flag applies to each"
          (let [[_ fx] (press s "s" "s" "G" "enter")]
            (is (= ["rebase" "-b" "wwww" "-b" "bbbb" "-d" "mmmm"] (jj-args fx)))))
        (testing "r on a source removes it, and steps on too"
          (let [[s] (press s "k" "r")]
            (is (= ["wwww"] (get-in s [:app/mode :mode/sources])))
            (is (= "aaaa" (cursor-id s)))))
        (testing "removing the last one cancels the rebase, leaving the cursor"
          (let [[s] (press s "k" "r" "g" "r")]
            (is (nil? (:app/mode s)))
            (is (= "wwww" (cursor-id s)))))))
    (testing "--ignore-immutable applies when toggled"
      (let [[s] (press (loaded) "!" "r" "G")
            [_ fx] (press s "enter")]
        (is (= ["rebase" "-r" "wwww" "-d" "mmmm" "--ignore-immutable"] (jj-args fx)))))))

(deftest squash-test
  (testing "into an undescribed change: background, no editor needed"
    (let [[s fx] (press (loaded) "j" "m" "g" "s")]
      (is (= "wwww" (cursor-id s)))
      (is (= ["squash" "--from" "bbbb" "--into" "wwww"] (jj-args fx)))))
  (testing "both described: suspends so jj can open the editor"
    (let [[s fx] (press (loaded) "j" "s")]
      (is (= "bbbb" (cursor-id s)))
      (is (= [{:fx/type :quit}] fx))
      (is (= ["jj" "squash" "-r" "bbbb"] (get-in s [:app/exec :exec/argv]))))))

(deftest abandon-confirm-test
  (let [[s fx] (press (loaded) "a")]
    (is (empty? fx))
    (is (= :confirm (get-in s [:app/mode :mode/type])))
    (is (empty? (second (press s "n"))) "n cancels")
    (is (= ["abandon" "wwww"] (jj-args (second (press s "y")))))))

(deftest describe-popup-test
  (let [[s fx] (press (loaded) "j" "d")
        _ (is (= [{:fx/type :description :fx/change-id "bbbb"}] fx))
        [s] (press s {:type :msg/description-loaded
                      :msg/change-id "bbbb"
                      :msg/result {:jj/description "B\n\nWhy B.\n"}})
        [s] (press s "x")]
    (is (= :describe (get-in s [:app/mode :mode/type])))
    (testing "enter replaces the first line, keeping the rest"
      (let [[s fx] (press s "enter")]
        (is (nil? (:app/mode s)))
        (is (= ["describe" "-r" "bbbb" "-m" "Bx\n\nWhy B."] (jj-args fx)))))
    (testing "ctrl+e hands the edited text to jj's editor"
      (let [[s fx] (press s (msg/key-press "e" :ctrl true))]
        (is (= [{:fx/type :quit}] fx))
        (is (= ["jj" "describe" "-r" "bbbb" "--editor" "-m" "Bx\n\nWhy B."] (get-in s [:app/exec :exec/argv])))))
    (testing "escape cancels"
      (let [[s fx] (press s "escape")]
        (is (nil? (:app/mode s)))
        (is (empty? fx)))))
  (testing "a jj failure is shown instead of the popup"
    (let [[s] (press (loaded) "d" {:type :msg/description-loaded :msg/change-id "wwww" :msg/result {:jj/error "boom"}})]
      (is (nil? (:app/mode s)))
      (is (= "boom" (get-in s [:app/message :message/text]))))))

(deftest files-toggle-test
  (testing "shift+tab shows the files of every change"
    (let [[s fx] (press (loaded) (msg/key-press :tab :shift true))]
      (is (:app/show-files? s))
      (is (= :all (get-in (first fx) [:fx/params :files])))))
  (testing "tab expands just the cursor's change"
    (let [[s fx] (press (loaded) (msg/key-press :tab))]
      (is (= #{"wwww"} (get-in s [:app/repo :repo/expanded])))
      (is (= "change_id(wwww)" (get-in (first fx) [:fx/params :files]))))))

(deftest diff-test
  (testing "enter on a change opens the tool with that commit"
    (let [[s fx] (press (loaded) "enter")]
      (is (= [{:fx/type :quit}] fx))
      (is (= (into (pop (pop (get-in config/defaults [:diff-tools :jj]))) ["cw"])
             (get-in s [:app/exec :exec/argv])))))
  (testing "enter on a file row limits the diff to that file"
    (let [[s] (press (loaded) "j" "j" "enter")]
      (is (= "b.clj" (peek (get-in s [:app/exec :exec/argv]))))))
  (testing "tools that need commit ids resolve them first for marks"
    (let [s (assoc (loaded) :app/diff-tool :difftool)
          [s fx] (press s "m" "m" "enter")]
      (is (nil? (:app/exec s)))
      (is (= :resolve-diff (:fx/type (first fx))))
      (is (= "heads(cw | cb)" (get-in (first fx) [:fx/target :target/to-rev]))))))

(deftest resolve-test
  (testing "a change without conflicts never suspends the TUI"
    (let [[s fx] (press (loaded) "M")]
      (is (empty? fx))
      (is (nil? (:app/exec s)))
      (is (= :error (get-in s [:app/message :message/level])))))
  (testing "a conflicted change runs jj resolve on all its conflicts"
    (let [[s fx] (press (loaded) "j" "M")]
      (is (= [{:fx/type :quit}] fx))
      (is (= ["jj" "resolve" "-r" "bbbb"] (get-in s [:app/exec :exec/argv])))))
  (testing "on a file row only that file, honoring --ignore-immutable"
    (let [[s] (press (loaded) "!" "j" "j" "M")]
      (is (= ["jj" "resolve" "-r" "bbbb" "b.clj" "--ignore-immutable"] (get-in s [:app/exec :exec/argv]))))))

(defn- help-text [s] (str/join "\n" (map #(str/replace % #"\u001b\[[0-9;]*m" "") (str/split-lines (view/render s)))))

(deftest help-search-test
  (let [[s] (press (loaded) "?" "/" "p" "u" "s" "h")]
    (testing "typing filters the bindings, matching group titles too"
      (is (str/includes? (help-text s) "Push bookmarks of change"))
      (is (not (str/includes? (help-text s) "Rebase"))))
    (testing "letters go to the search, not to the help keys"
      (is (= :help (get-in (first (press s "q")) [:app/mode :mode/type]))))
    (testing "enter keeps the filter; then esc clears it before closing"
      (let [[s] (press s "enter")]
        (is (str/includes? (help-text s) "Push bookmarks of change"))
        (is (= :help (get-in (first (press s "j")) [:app/mode :mode/type])) "j scrolls again")
        (let [[s] (press s "escape")]
          (is (= :help (get-in s [:app/mode :mode/type])))
          (is (str/includes? (help-text s) "Rebase"))
          (is (nil? (:app/mode (first (press s "escape"))))))))
    (testing "a query matching nothing says so"
      (is (str/includes? (help-text (first (press s "z" "z" "z"))) "no matching keys")))))

(defn- bookmark [bname remote & {:keys [tracked? synced?] :or {tracked? true synced? true}}]
  {:bookmark/name bname
   :bookmark/remote remote
   :bookmark/ref (if remote (str bname "@" remote) bname)
   :bookmark/commit "c"
   :bookmark/tracked? (boolean (and remote tracked?))
   :bookmark/synced? synced?
   :bookmark/description "d"})

(deftest branch-diff-test
  (let [[s] (press (loaded) "D" {:type :msg/bookmarks-loaded
                                 :msg/purpose :branch-diff
                                 :msg/result {:jj/bookmarks [(bookmark "feat" nil) (bookmark "main" nil) (bookmark "main" "origin")]}})
        [s] (press s "f" "e" "a" "t" "enter")
        values #(map :item/value (st/picker-matches (:app/mode (first (apply press s %)))))
        revset #(some (fn [arg] (when (str/starts-with? arg "fork_point") arg))
                      (get-in (first (apply press s %)) [:app/exec :exec/argv]))]
    (testing "picking the branch offers its base: trunk, the other bookmarks, then the loaded changes"
      (is (= ["trunk()" "main" "main@origin" "cw" "cb" "ca" "cm"] (values []))))
    (testing "enter right away diffs from the fork point with trunk"
      (is (= "fork_point(trunk() | feat)..feat" (revset ["enter"]))))
    (testing "another bookmark can be the base"
      (is (= "fork_point(main | feat)..feat" (revset ["m" "a" "i" "n" "enter"]))))
    (testing "the query greps changes like / does: change id, commit id or description"
      (is (= ["ca"] (values ["a" "a"])))
      (is (= ["cb"] (values ["c" "b"])))
      (is (= ["ca"] (values ["l" "o" "g" "i" "n"])))
      (is (= "fork_point(ca | feat)..feat" (revset ["c" "a" "enter"]))))))

(deftest bookmark-picker-test
  (let [[s] (press (loaded) "b")
        [s] (press s {:type :msg/bookmarks-loaded
                      :msg/purpose :bookmark-set
                      :msg/result {:jj/bookmarks [(bookmark "feat" nil) (bookmark "main" "origin")]}})]
    (is (= ["feat  c  d"] (map :item/label (st/picker-matches (:app/mode s)))) "remote bookmarks can't be set")
    (testing "typing a new name offers to create it"
      (let [[s fx] (press s "n" "e" "w" "enter")]
        (is (nil? (:app/mode s)))
        (is (= ["bookmark" "create" "new" "-r" "wwww"] (jj-args fx)))))
    (testing "picking an existing one moves it"
      (let [[_ fx] (press s "f" "enter")]
        (is (= ["bookmark" "set" "feat" "-r" "wwww" "--allow-backwards"] (jj-args fx)))))))

(deftest push-bookmarks-test
  (testing "pushes the bookmarks on the change"
    (is (= ["git" "push" "-b" "feat"] (jj-args (second (press (loaded) "j" "P"))))))
  (testing "without any, offers the local bookmarks the remote lacks or has elsewhere"
    (let [[s fx] (press (loaded) "P")
          _ (is (= [{:fx/type :bookmarks :fx/purpose :push}] fx))
          [s] (press s {:type :msg/bookmarks-loaded
                        :msg/purpose :push
                        :msg/result {:jj/bookmarks [(bookmark "main" nil)
                                                    (bookmark "main" "origin")
                                                    (bookmark "ahead" nil :synced? false)
                                                    (bookmark "ahead" "origin" :synced? false)
                                                    (bookmark "new" nil)
                                                    (bookmark "gone" "origin" :tracked? false :synced? false)]}})]
      (is (= [["ahead" "out of sync"] ["new" "not on remote"]]
             (map (juxt :item/value :item/detail) (st/picker-matches (:app/mode s)))))
      (is (= ["git" "push" "-b" "new"] (jj-args (second (press s "n" "e" "w" "enter")))))))
  (testing "says so when everything is pushed"
    (let [[s] (press (loaded) "P" {:type :msg/bookmarks-loaded
                                   :msg/purpose :push
                                   :msg/result {:jj/bookmarks [(bookmark "main" nil) (bookmark "main" "origin")]}})]
      (is (nil? (:app/mode s)))
      (is (= "Every bookmark is pushed" (get-in s [:app/message :message/text]))))))

(deftest pull-test
  (testing "fetches the bookmarks on the marked changes"
    (let [[s fx] (press (loaded) "m" "m" "F")]
      (is (= #{"wwww" "bbbb"} (get-in s [:app/repo :repo/marks])))
      (is (= ["git" "fetch" "-b" "feat"] (jj-args fx)))))
  (let [[s fx] (press (loaded) "F")
        _ (is (= [{:fx/type :bookmarks :fx/purpose :pull}] fx))
        [s] (press s {:type :msg/bookmarks-loaded
                      :msg/purpose :pull
                      :msg/result {:jj/bookmarks [(bookmark "main" nil)
                                                  (bookmark "main" "origin")
                                                  (bookmark "theirs" "upstream" :tracked? false :synced? false)]}})]
    (testing "offers the remote bookmarks"
      (is (= ["main@origin  c  d" "theirs@upstream  c  d"] (map :item/label (st/picker-matches (:app/mode s))))))
    (testing "a tracked one is fetched from its remote"
      (let [[_ [fx]] (press s "m" "a" "i" "n" "enter")]
        (is (= ["git" "fetch" "--remote" "origin" "-b" "main"] (:fx/args fx)))
        (is (nil? (:fx/then fx)))))
    (testing "an untracked one is tracked after fetching, so a local bookmark follows it"
      (let [[_ [fx]] (press s "t" "h" "enter")]
        (is (= ["git" "fetch" "--remote" "upstream" "-b" "theirs"] (:fx/args fx)))
        (is (= [["bookmark" "track" "theirs@upstream"]] (:fx/then fx)))))))

(deftest repo-switch-test
  (let [[s fx] (press (loaded) "w")
        now 1000000000
        visit (fn [dir n] {:visit/dir dir :visit/count n :visit/last (- now 7200000)})]
    (is (= [{:fx/type :recent-repos :fx/exclude #{"/repo"}}] fx) "the open repo isn't offered")
    (let [[s] (press s {:type :msg/recent-repos-loaded
                        :msg/now now
                        :msg/repos [(visit "/code/app" 9) (visit "/code/api" 4) (visit "/work/boat" 1)]})
          labels #(map :item/label (st/picker-matches (:app/mode (first (apply press s %)))))]
      (testing "best first, with the visits as detail"
        (is (= ["/code/app" "/code/api" "/work/boat"] (labels [])))
        (is (= "9 visits · 2h ago" (:item/detail (first (st/picker-matches (:app/mode s)))))))
      (testing "equally good matches keep frecency order"
        (is (= ["/code/app" "/code/api"] (labels ["a" "p"]))))
      (testing "a match in the repo's own folder beats one elsewhere in its path"
        (is (= ["/work/boat" "/code/app" "/code/api"] (labels ["o"]))))
      (testing "scattered matches come last, however frequent"
        (is (= ["/work/boat" "/code/app" "/code/api"] (labels ["o" "a"])))
        (is (= ["/code/api"] (labels ["c" "d" "i"])) "matching scattered still counts"))
      (testing "picking one opens it, and its log replaces this repo's"
        (let [[s fx] (press s "a" "p" "i" "enter")]
          (is (nil? (:app/mode s)))
          (is (= [{:fx/type :open-repo :fx/dir "/code/api"}] fx))
          (let [[s fx] (press s {:type :msg/repo-opened
                                 :msg/result {:repo/dir "/code/api" :repo/base-template "t"}})]
            (is (= "/code/api" (get-in s [:app/repo :repo/dir])))
            (is (nil? (:app/busy s)))
            (is (= [:load-log] (map :fx/type fx)))))))
    (testing "without other visited repos there's nothing to pick"
      (let [[s] (press s {:type :msg/recent-repos-loaded :msg/now now :msg/repos []})]
        (is (nil? (:app/mode s)))
        (is (= "No other repos visited yet" (get-in s [:app/message :message/text])))))))

(deftest revset-error-test
  (let [[s fx] (press (loaded) "L" "x" "(" "enter")]
    (is (= "x(" (get-in s [:app/repo :repo/revset])))
    (let [[s fx] (step s {:type :msg/log-loaded :msg/gen (:fx/gen (first fx)) :msg/result {:jj/error "bad revset"}})]
      (is (nil? (get-in s [:app/repo :repo/revset])) "falls back to the last revset that worked")
      (is (= :error (get-in s [:app/message :message/level])))
      (is (nil? (get-in (first fx) [:fx/params :revset]))))))

(defn- choosing
  "The chooser for /work/lib, a git repo inside the jj repo /work, with one
   recent repo."
  []
  (let [now 1000000000]
    (checked st/State
             (app/init-state {:config (config/user->internal config/defaults)
                              :mode (app/chooser {:git-root "/work/lib"
                                                  :parent "/work"
                                                  :recent [{:visit/dir "/code/other" :visit/count 3 :visit/last (- now 7200000)}]
                                                  :now now})}))))

(deftest chooser-test
  (let [s (choosing)]
    (is (= [["i" [:init "/work/lib"]] ["p" [:open "/work"]] ["1" [:open "/code/other"]]]
           (map (juxt :choice/key :choice/action) (get-in s [:app/mode :mode/choices]))))
    (is (= "3 visits · 2h ago" (get-in s [:app/mode :mode/choices 2 :choice/detail])))
    (testing "a hotkey or j + enter picks a choice"
      (is (= [{:fx/type :init-repo :fx/dir "/work/lib"}] (second (press s "i"))))
      (is (= [{:fx/type :open-repo :fx/dir "/work"}] (second (press s "j" "enter"))))
      (is (= [{:fx/type :open-repo :fx/dir "/code/other"}] (second (press s "1")))))
    (testing "one choice at a time"
      (is (empty? (second (press s "i" "p")))))
    (testing "q quits"
      (is (= [{:fx/type :quit}] (second (press s "q")))))
    (testing "a repo that fails to open leaves the chooser up with the error"
      (let [[s] (press s "p" {:type :msg/repo-opened :msg/result {:jj/error "No jj repo at /work"}})]
        (is (= :no-repo (get-in s [:app/mode :mode/type])))
        (is (nil? (:app/busy s)))
        (is (= "No jj repo at /work" (get-in s [:app/message :message/text])))))
    (testing "an opened repo replaces the chooser and loads its log"
      (let [[s fx] (press s "p" {:type :msg/repo-opened
                                 :msg/result {:repo/dir "/work" :repo/base-template "t"}})]
        (is (nil? (:app/mode s)))
        (is (= "/work" (get-in s [:app/repo :repo/dir])))
        (is (= [:load-log] (map :fx/type fx)))
        (let [[s] (step s (loaded-msg (:fx/gen (first fx)) log-output))]
          (is (= "wwww" (cursor-id s))))))))

(deftest fuzzy-score-test
  (is (some? (st/fuzzy-score "fl" "feature/login")))
  (is (nil? (st/fuzzy-score "lf" "feature/login")))
  (is (< (st/fuzzy-score "login" "feature/login") (st/fuzzy-score "fl" "feature/login"))
      "a substring beats scattered characters"))
