(ns jjtui.view
  "Renders the state to a string. The layout helpers (log-width, panel-lines)
   are also used by jjtui.app to size scrolling, so the two can't disagree."
  (:require
   [charm.components.text-input :as ti]
   [charm.style.core :as style]
   [clojure.string :as str]
   [jjtui.keys :as keys]
   [jjtui.state :as st]))

;; ---------------------------------------------------------------------------
;; Styling helpers

(def ^:private esc "\u001b[")

(defn- sgr [& codes] (str esc (str/join ";" codes) "m"))
(def ^:private reset (sgr 0))

(defn- paint [s & codes] (str (apply sgr codes) s reset))

(defn- dim [s] (paint s 2))
(defn- bold [s] (paint s 1))
(defn- fg [n s] (paint s "38;5" n))

(defn- palette [state]
  (if (:app/dark? state)
    {:block 235 :cursor 237 :bar-fg 252 :bar-bg 236 :badge 5 :selected 238}
    {:block 254 :cursor 252 :bar-fg 236 :bar-bg 253 :badge 5 :selected 252}))

(defn- width-of [s] (style/string-width s))

(defn- fit
  "Truncates `s` to `width` columns, styles intact."
  [s width]
  (if (<= (width-of s) width)
    s
    (style/truncate s width :tail "…")))

(defn- pad [s width]
  (let [w (width-of s)]
    (if (< w width) (str s (apply str (repeat (- width w) " "))) s)))

(defn- with-bg
  "`s` on background color `bg`, filled to `width`. jj's output resets styles
   mid-line, so the background is re-applied after every reset."
  [s bg width]
  (let [on (sgr "48;5" bg)
        s (str/replace s #"\u001b\[(?:0|49)?m" (fn [m] (str m on)))
        s (fit (str on s) width)]
    (str s on (apply str (repeat (max 0 (- width (width-of s))) " ")) reset)))

;; ---------------------------------------------------------------------------
;; Layout

(defn log-width [state]
  (if (:app/split? state)
    (max 20 (min (- (:app/width state) 20) (int (* (:app/width state) (:app/split-ratio state)))))
    (:app/width state)))

(defn- message-lines [state]
  (let [lines (str/split-lines (str (get-in state [:app/message :message/text])))]
    (when (> (count lines) 1) lines)))

(defn- separator [state title]
  (let [w (:app/width state)
        t (if title (str "── " title " ") "")]
    (dim (str t (apply str (repeat (max 0 (- w (width-of t))) "─"))))))

(defn- picker-max-items [state]
  (max 3 (min 12 (quot (:app/height state) 3))))

(defn panel-lines
  "Lines of the bottom panel (menus, pickers, prompts, multi-line errors)."
  [state]
  (let [{:mode/keys [type title input] :as mode} (:app/mode state)]
    (case type
      :picker
      (let [matches (st/picker-matches mode)
            n (picker-max-items state)
            sel (max 0 (min (:mode/selected mode) (dec (count matches))))
            start (max 0 (min (- sel (quot n 2)) (- (count matches) n)))
            shown (take n (drop start (map-indexed vector matches)))]
        (concat
         [(separator state title)
          (str (ti/text-input-view input) "  " (dim (str (count matches) "/" (count (:mode/items mode)))))]
         (if (empty? shown)
           [(dim "  no matches")]
           (for [[i item] shown]
             (if (= i sel)
               (with-bg (str (fg 5 "❯ ") (bold (:item/label item))) (:selected (palette state)) (:app/width state))
               (str "  " (:item/label item)))))))

      :menu
      (concat
       [(separator state title)]
       (for [{:option/keys [key label toggle]} (:mode/options mode)]
         (str "  " (bold (fg 3 key)) "  "
              (if toggle
                (str (if (contains? (:mode/flags mode) toggle) (fg 2 "[x] ") (dim "[ ] ")) label)
                label)))
       [(dim "  esc cancel")])

      :confirm
      (concat
       [(separator state "Confirm")]
       (map #(str "  " %) (:mode/prompt mode))
       [(str "  " (bold (fg 2 "y")) "/enter confirm   " (bold (fg 1 "n")) "/esc cancel")])

      :input
      [(separator state title)
       (str "  " (ti/text-input-view input))]

      (when-let [lines (message-lines state)]
        (cons (separator state "Message")
              (map #(fg 1 %) (take 10 lines)))))))

;; ---------------------------------------------------------------------------
;; Log pane

(def ^:private status-colors
  {"M" 3 "A" 2 "D" 1 "R" 6 "C" 6})

(defn- file-line [{:row/keys [prefix file]}]
  (let [{:file/keys [status path source]} file]
    (str prefix
         (fg (get status-colors status 7) status) " "
         path
         (when source (dim (str " ← " source))))))

(defn- block-of
  "Row indices belonging to the change at row index `idx` (its node, text and
   file lines)."
  [rows idx]
  (let [entry (:row/entry (get rows idx))]
    (set (when entry
           (keep-indexed (fn [i r] (when (and (= entry (:row/entry r)) (not= :other (:row/kind r))) i)) rows)))))

(defn- log-lines [state cursor-idx height width]
  (let [rows (st/rows state)
        mode (:app/mode state)
        pal (palette state)
        block (if cursor-idx (block-of rows cursor-idx) #{})
        {:repo/keys [marks offset loading?]} (:app/repo state)
        sources (set (when (= :pick (:mode/type mode)) (:mode/sources mode)))
        visible (take height (drop offset (map-indexed vector rows)))]
    (cond
      (and (empty? rows) loading?) [(dim "  Loading…")]
      (empty? rows) [(dim "  No changes in this revset (press L to change it)")]
      :else
      (for [[i row] visible]
        (let [id (:change/id (st/row-entry state row))
              node? (= :node (:row/kind row))
              cursor? (= i cursor-idx)
              in-block? (contains? block i)
              gutter (str (cond cursor? (fg 6 "▌") in-block? (fg 6 "▏") :else " ")
                          (cond
                            (and node? (contains? sources id)) (fg 3 "◆")
                            (and node? (contains? marks id)) (fg 5 "●")
                            :else " "))
              content (if (= :file (:row/kind row)) (file-line row) (:row/line row))
              line (str gutter content)]
          (cond
            cursor? (with-bg line (:cursor pal) width)
            in-block? (with-bg line (:block pal) width)
            :else (fit line width)))))))

;; ---------------------------------------------------------------------------
;; Diff pane

(defn- preview-lines [state height width]
  (let [{:preview/keys [lines offset loading?]} (get-in state [:app/repo :repo/preview])
        entry (st/cursor-entry state)
        title (str " " (if entry (str "diff " (:change/short-id entry)) "diff")
                   (when-let [f (get-in state [:app/repo :repo/cursor :file/path])] (str "  " f))
                   (when loading? "  …")
                   (when (seq lines)
                     (str "  " (inc (or offset 0)) "/" (count lines))))
        body (if (and (empty? lines) (not loading?))
               [(dim " (empty diff)")]
               (map #(fit (str " " %) width) (take (dec height) (drop (or offset 0) lines))))]
    (cons (fit (dim title) width) body)))

;; ---------------------------------------------------------------------------
;; Status bar

(defn- status-line [state]
  (let [pal (palette state)
        {:mode/keys [type action] :as mode} (:app/mode state)
        badge (case type
                :pick (if (= :rebase action)
                        (str " REBASE " (:mode/source-flag mode) " ")
                        " SQUASH INTO ")
                :picker " PICK "
                :menu " MENU "
                :confirm " CONFIRM "
                :input " INPUT "
                :help " HELP "
                " jjtui ")
        badge (paint badge 1 "38;5;15" (str "48;5;" (if (= :pick type) 3 (:badge pal))))
        {:message/keys [text level] :as msg} (:app/message state)
        left (cond
               (:app/busy state) (fg 3 (str "⟳ " (:app/busy state) "…"))
               (= :pick type)
               (str "Pick target for " (count (:mode/sources mode)) " change(s): "
                    (dim (get keys/pick-help action)))
               (= :picker type) (dim "type to filter · ↑/↓ move · enter select · esc cancel")
               (= :input type) (dim "enter submit · esc cancel")
               (= :help type) (dim (cond
                                     (ti/focused? (:mode/search mode)) "type to filter · enter done · esc clear"
                                     (seq (ti/value (:mode/search mode))) "/ edit search · j/k scroll · esc clear · q close"
                                     :else "/ search · j/k scroll · esc/q/? close"))
               (message-lines state) (dim "(see message above)")
               msg (if (= :error level) (fg 1 text) text)
               (= :no-repo type) (dim "j/k move · enter or key select · q quit")
               (get-in state [:app/repo :repo/loading?]) (dim "loading…")
               :else (dim "? help"))
        {:repo/keys [marks ignore-immutable? revset]} (:app/repo state)
        right (str/join "  "
                        (remove nil?
                                [(when (seq marks) (fg 5 (str "● " (count marks) " marked")))
                                 (when ignore-immutable? (fg 1 "!immutable"))
                                 (when revset (str (dim "revset ") revset))
                                 (str (dim "diff ") (name (:app/diff-tool state)))]))
        width (:app/width state)
        right-w (width-of right)
        left-space (max 0 (- width (width-of badge) 1 right-w 1))
        left (fit left left-space)]
    (str badge " " (pad left left-space) " " right)))

;; ---------------------------------------------------------------------------
;; Help

(defn- help-groups
  "[title [[keys description] ...]] of the help screen, keeping the bindings
   whose group title, keys and description contain every word of `query`."
  [query]
  (let [terms (str/split (str/lower-case (str/trim query)) #"\s+")
        groups (concat
                (for [[title bindings] keys/groups]
                  [title (for [[ks _ desc] bindings]
                           [(str/join " " (map keys/display-key ks)) desc])])
                [["Modes" [["help" "/ search, enter done, esc clear"]
                           ["pick" "j/k move, enter select, esc cancel"]
                           ["rebase pick" "a after, b before, s source mode"]
                           ["picker" "type to filter, ↑/↓, enter, esc"]
                           ["menu" "press the option key, esc cancel"]]]])
        match? (fn [title [ks desc]]
                 (let [haystack (str/lower-case (str title " " ks " " desc))]
                   (every? #(str/includes? haystack %) terms)))]
    (keep (fn [[title rows]]
            (when-let [rows (seq (filter #(match? title %) rows))]
              [title rows]))
          groups)))

(defn- help-columns [state groups]
  (let [groups (for [[title rows] groups]
                 (concat [(bold (fg 6 title))]
                         (for [[ks desc] rows]
                           (str "  " (pad (fg 3 ks) 14) desc))
                         [""]))
        col-w 48
        ncols (max 1 (quot (:app/width state) col-w))
        total (reduce + (map count groups))
        target (int (Math/ceil (/ total ncols)))
        columns (reduce (fn [cols g]
                          (let [cur (peek cols)]
                            (if (and (seq cur) (> (+ (count cur) (count g)) target) (< (count cols) ncols))
                              (conj cols (vec g))
                              (conj (pop cols) (into cur g)))))
                        [[]]
                        groups)
        height (apply max (map count columns))]
    (for [i (range height)]
      (str/join (for [col columns] (pad (get col i "") col-w))))))

(defn- help-screen [state height]
  (let [search (get-in state [:app/mode :mode/search])
        query (ti/value search)
        groups (help-groups query)
        header (str (bold " jjtui") (dim "  keys")
                    (when (or (ti/focused? search) (seq query))
                      (str "  " (ti/text-input-view search))))
        lines (concat [header ""]
                      (if (seq groups)
                        (map #(str " " %) (help-columns state groups))
                        [(dim "  no matching keys")]))
        offset (max 0 (min (get-in state [:app/mode :mode/offset] 0) (- (count lines) height)))]
    (map #(fit % (:app/width state)) (take height (drop offset lines)))))

;; ---------------------------------------------------------------------------
;; Repo chooser

(defn- chooser-screen [state]
  (let [{:mode/keys [git-root choices selected]} (:app/mode state)
        width (:app/width state)
        recent? #(re-matches #"\d" (:choice/key %))
        line (fn [i {:choice/keys [key label detail]}]
               (let [s (str (bold (fg 3 key)) "  " label "  " (dim detail))]
                 (if (= i selected)
                   (with-bg (str (fg 5 " ❯ ") s) (:selected (palette state)) width)
                   (fit (str "   " s) width))))]
    (concat [""
             (str " " (bold git-root) " is a git repo, but jj isn't set up in it.")
             (dim " Choose how to continue:")
             ""]
            (map-indexed line (remove recent? choices))
            [""
             (bold (fg 6 " Recent repos"))]
            (if (some recent? choices)
              (keep-indexed (fn [i c] (when (recent? c) (line i c))) choices)
              [(dim "   none yet: jjtui remembers the repos you open")]))))

;; ---------------------------------------------------------------------------
;; Root

(defn render [state]
  (let [{:app/keys [width height]} state
        panel (panel-lines state)
        body-h (max 1 (- height 1 (count panel)))
        lw (log-width state)
        cursor-idx (st/cursor-index state)
        body (case (get-in state [:app/mode :mode/type])
               :help (help-screen state body-h)
               :no-repo (chooser-screen state)
               (let [left (log-lines state cursor-idx body-h lw)]
                 (if (:app/split? state)
                   (let [pw (- width lw 1)
                         right (vec (preview-lines state body-h pw))
                         left (vec left)]
                     (for [i (range body-h)]
                       (str (pad (get left i "") lw) (dim "│") (get right i ""))))
                   left)))
        body (take body-h (concat body (repeat "")))]
    (str/join "\n" (concat body panel [(status-line state)]))))
