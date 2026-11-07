(ns jjtui.view
  "Renders the state to a string. The layout helpers (log-width, panel-lines)
   are also used by jjtui.app to size scrolling, so the two can't disagree."
  (:require
   [charm.components.text-input :as ti]
   [charm.style.core :as style]
   [charm.style.overlay :as overlay]
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

(def ^:private match-on (str reset (sgr 30 43)))

(defn highlight
  "Paints every case-insensitive occurrence of `query` in the visible text of
   `line`, restoring the line's own styles after each one."
  [line query]
  (let [q (str/lower-case query)
        tokens (re-seq #"\u001b\[[0-9;]*m|(?s:.)" line)
        style? #(str/starts-with? % "\u001b")
        chars (mapv str/lower-case (remove style? tokens))
        text (apply str chars)
        starts (vec (reductions + 0 (map count chars)))
        hits (if (str/blank? q)
               #{}
               (loop [from 0 hits #{}]
                 (if-let [at (str/index-of text q from)]
                   (recur (+ at (count q))
                          (into hits (filter #(<= at (starts %) (+ at (count q) -1))) (range (count chars))))
                   hits)))]
    (if (empty? hits)
      line
      (let [{:keys [out active on?]}
            (reduce (fn [{:keys [out active on? i] :as acc} t]
                      (if (style? t)
                        (assoc acc
                               :out (cond-> (conj out t) on? (conj match-on))
                               :active (if (re-matches #"\u001b\[0?m" t) [] (conj active t)))
                        (let [hit? (contains? hits i)]
                          (assoc acc
                                 :out (conj (cond-> out
                                              (and hit? (not on?)) (conj match-on)
                                              (and on? (not hit?)) (into (cons reset active)))
                                            t)
                                 :on? hit?
                                 :i (inc i)))))
                    {:out [] :active [] :on? false :i 0}
                    tokens)]
        (apply str (cond-> out on? (into (cons reset active))))))))

(defn- match-count
  "\"2/5\": where the cursor's change is among the `matches` (entry indices)."
  [state matches]
  (let [current (:change/index (st/cursor-entry state))]
    (str (or (first (keep-indexed (fn [i m] (when (= m current) (inc i))) matches)) "-")
         "/" (count matches))))

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
           (for [[i item] shown
                 :let [detail (some->> (:item/detail item) (str "  ") dim)]]
             (if (= i sel)
               (with-bg (str (fg 5 "❯ ") (bold (:item/label item)) detail) (:selected (palette state)) (:app/width state))
               (str "  " (:item/label item) detail))))))

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

      :search
      (let [query (ti/value input)
            matches (st/search-matches state query)]
        [(separator state "Search changes")
         (str "  " (ti/text-input-view input) "  "
              (dim (cond
                     (str/blank? query) ""
                     (empty? matches) "no matches"
                     :else (match-count state matches))))])

      (when-let [lines (message-lines state)]
        (cons (separator state "Message")
              (map #(fg 1 %) (take 10 lines)))))))

;; ---------------------------------------------------------------------------
;; Describe popup

(defn- input-window
  "The text input scrolled so its cursor stays within `width` columns."
  [input width]
  (let [{:keys [value pos]} input
        start (max 0 (- pos (dec width)))]
    (fit (ti/text-input-view (assoc input :value (subvec value start) :pos (- pos start)))
         width)))

(defn- describe-popup
  "Lines of the floating describe box, `width` columns wide."
  [state width]
  (let [{:mode/keys [short-id input body]} (:app/mode state)
        inner (- width 4)
        rule #(apply str (repeat % "─"))
        row #(str (dim "│ ") (pad (fit % inner) inner) (dim " │"))
        title (str " Describe " short-id " ")
        hint (str " " (bold (fg 3 "ctrl+e")) " open in editor ")
        ;; ╭─ title ─…─ hint ─╮, dropping the hint when the box is too narrow
        fill (- width 4 (width-of title) (width-of hint))
        top (if (pos? fill)
              (str (dim "╭─") (bold title) (dim (rule fill)) hint (dim "─╮"))
              (str (dim "╭─") (bold title) (dim (str (rule (max 0 (- width 3 (width-of title)))) "╮"))))
        more (if (str/blank? body) 0 (count (str/split-lines (str/trim body))))]
    (concat
     [top (row (input-window input inner))]
     (when (pos? more)
       [(row (dim (str "+ " more " more line" (when (> more 1) "s"))))])
     [(dim (str "╰" (rule (- width 2)) "╯"))])))

(defn- with-popup
  "Floats the describe box over the body lines."
  [state body]
  (let [width (:app/width state)
        w (max 20 (min 72 (- width 4)))
        popup (describe-popup state w)
        ;; Padded so no line is empty: place-overlay drops trailing empty lines
        base (str/join "\n" (map #(pad % width) body))]
    (str/split-lines (overlay/place-overlay base (str/join "\n" popup)
                                            (max 0 (quot (- width w) 2))
                                            (max 0 (quot (- (count body) (count popup)) 2))))))

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
        query (st/search-query state)
        matched (set (st/search-matches state query))
        sources (set (when (= :pick (:mode/type mode)) (:mode/sources mode)))
        visible (take height (drop offset (map-indexed vector rows)))]
    (cond
      (and (empty? rows) loading?) [(dim "  Loading…")]
      (empty? rows) [(dim "  No changes in this revset (press L to change it)")]
      :else
      (for [[i row] visible]
        (let [id (:change/id (st/row-entry state row))
              match? (contains? matched (:row/entry row))
              node? (= :node (:row/kind row))
              cursor? (= i cursor-idx)
              in-block? (contains? block i)
              gutter (str (cond cursor? (fg 6 "▌") in-block? (fg 6 "▏") :else " ")
                          (cond
                            (and node? (contains? sources id)) (fg 3 "◆")
                            (and node? (contains? marks id)) (fg 5 "●")
                            (and node? match?) (fg 3 "›")
                            :else " "))
              content (case (:row/kind row)
                        :file (file-line row)
                        (:node :text) (cond-> (:row/line row) match? (highlight query))
                        (:row/line row))
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
                :describe " DESCRIBE "
                :search " SEARCH "
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
               (= :describe type) (dim "enter save · ctrl+e edit in $EDITOR · esc cancel")
               (= :search type) (dim "type to search · ↑/↓ next/prev match · enter keep · esc back")
               (= :help type) (dim (cond
                                     (ti/focused? (:mode/search mode)) "type to filter · enter done · esc clear"
                                     (seq (ti/value (:mode/search mode))) "/ edit search · j/k scroll · esc clear · q close"
                                     :else "/ search · j/k scroll · esc/q/? close"))
               (message-lines state) (dim "(see message above)")
               msg (if (= :error level) (fg 1 text) text)
               (= :no-repo type) (dim "j/k move · enter or key select · q quit")
               (get-in state [:app/repo :repo/loading?]) (dim "loading…")
               (get-in state [:app/repo :repo/search]) (dim "n/N next/prev match · esc clear search · ? help")
               :else (dim "? help"))
        {:repo/keys [marks ignore-immutable? revset search]} (:app/repo state)
        right (str/join "  "
                        (remove nil?
                                [(when (and search (not= :search type))
                                   (str (fg 3 (str "/" search)) " " (dim (match-count state (st/search-matches state search)))))
                                 (when (seq marks) (fg 5 (str "● " (count marks) " marked")))
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
                           ["search" "type to jump, ↑/↓ next/prev match, enter keep, esc back"]
                           ["search active" "n/N next/prev match, / edit query, esc clear"]
                           ["pick" "j/k move, enter select, esc cancel"]
                           ["rebase pick" "a after, b before, r add/remove change, s source mode"]
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

(defn- chooser-screen
  "The chooser's lines, scrolled so the selected choice stays within
   `height`."
  [state height]
  (let [{:mode/keys [dir git-root choices selected]} (:app/mode state)
        width (:app/width state)
        line (fn [i {:choice/keys [key label detail]}]
               (let [s (str (bold (fg 3 (or key " "))) "  " label "  " (dim detail))]
                 (if (= i selected)
                   (with-bg (str (fg 5 " ❯ ") s) (:selected (palette state)) width)
                   (fit (str "   " s) width))))
        group (fn [g] (keep-indexed (fn [i c] (when (= g (:choice/group c)) [i (line i c)])) choices))
        section (fn [title rows] (when (seq rows) (concat [[nil ""] [nil (bold (fg 6 (str " " title)))]] rows)))
        recent (group :recent)
        rows (concat [[nil ""]
                      [nil (if git-root
                             (str " " (bold git-root) " is a git repo, but jj isn't set up in it.")
                             (str " " (bold dir) " isn't in a jj repo."))]
                      [nil (dim " Choose how to continue:")]
                      [nil ""]]
                     (group :setup)
                     (section (str "Repos in " dir) (group :nearby))
                     (or (section "Recent repos" recent)
                         [[nil ""] [nil (bold (fg 6 " Recent repos"))]
                          [nil (dim "   none yet: jjtui remembers the repos you open")]]))
        at (count (take-while #(not= selected (first %)) rows))
        offset (max 0 (- (inc at) height))]
    (map second (take height (drop offset rows)))))

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
               :no-repo (chooser-screen state body-h)
               (let [left (log-lines state cursor-idx body-h lw)]
                 (if (:app/split? state)
                   (let [pw (- width lw 1)
                         right (vec (preview-lines state body-h pw))
                         left (vec left)]
                     (for [i (range body-h)]
                       (str (pad (get left i "") lw) (dim "│") (get right i ""))))
                   left)))
        body (take body-h (concat body (repeat "")))
        body (if (= :describe (get-in state [:app/mode :mode/type])) (with-popup state body) body)]
    (str/join "\n" (concat body panel [(status-line state)]))))
