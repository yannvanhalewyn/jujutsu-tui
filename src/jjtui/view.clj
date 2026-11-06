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
  (if (:dark? state)
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
  (if (:split? state)
    (max 20 (min (- (:width state) 20) (int (* (:width state) (:split-ratio state)))))
    (:width state)))

(defn- message-lines [state]
  (let [lines (str/split-lines (str (get-in state [:message :text])))]
    (when (> (count lines) 1) lines)))

(defn- separator [state title]
  (let [w (:width state)
        t (if title (str "── " title " ") "")]
    (dim (str t (apply str (repeat (max 0 (- w (width-of t))) "─"))))))

(defn- picker-max-items [state]
  (max 3 (min 12 (quot (:height state) 3))))

(defn panel-lines
  "Lines of the bottom panel (menus, pickers, prompts, multi-line errors)."
  [state]
  (let [{:keys [type] :as mode} (:mode state)]
    (case type
      :picker
      (let [matches (st/picker-matches mode)
            n (picker-max-items state)
            sel (max 0 (min (:selected mode) (dec (count matches))))
            start (max 0 (min (- sel (quot n 2)) (- (count matches) n)))
            shown (take n (drop start (map-indexed vector matches)))]
        (concat
         [(separator state (:title mode))
          (str (ti/text-input-view (:input mode)) "  " (dim (str (count matches) "/" (count (:items mode)))))]
         (if (empty? shown)
           [(dim "  no matches")]
           (for [[i item] shown]
             (if (= i sel)
               (with-bg (str (fg 5 "❯ ") (bold (:label item))) (:selected (palette state)) (:width state))
               (str "  " (:label item)))))))

      :menu
      (concat
       [(separator state (:title mode))]
       (for [{:keys [key label toggle]} (:options mode)]
         (str "  " (bold (fg 3 key)) "  "
              (if toggle
                (str (if (contains? (:flags mode) toggle) (fg 2 "[x] ") (dim "[ ] ")) label)
                label)))
       [(dim "  esc cancel")])

      :confirm
      (concat
       [(separator state "Confirm")]
       (map #(str "  " %) (:prompt mode))
       [(str "  " (bold (fg 2 "y")) "/enter confirm   " (bold (fg 1 "n")) "/esc cancel")])

      :input
      [(separator state (:title mode))
       (str "  " (ti/text-input-view (:input mode)))]

      (when-let [lines (message-lines state)]
        (cons (separator state "Message")
              (map #(fg 1 %) (take 10 lines)))))))

;; ---------------------------------------------------------------------------
;; Log pane

(def ^:private status-colors
  {"M" 3 "A" 2 "D" 1 "R" 6 "C" 6})

(defn- file-line [{:keys [prefix file]}]
  (str prefix
       (fg (get status-colors (:status file) 7) (:status file)) " "
       (:path file)
       (when (:source file) (dim (str " ← " (:source file))))))

(defn- block-of
  "Row indices belonging to the change at row index `idx` (its node, text and
   file lines)."
  [rows idx]
  (let [entry (:entry (get rows idx))]
    (set (when entry
           (keep-indexed (fn [i r] (when (and (= entry (:entry r)) (not= :other (:kind r))) i)) rows)))))

(defn- log-lines [state cursor-idx height width]
  (let [rows (get-in state [:log :rows])
        entries (get-in state [:log :entries])
        pal (palette state)
        block (if cursor-idx (block-of rows cursor-idx) #{})
        marks (:marks state)
        sources (set (when (= :pick (get-in state [:mode :type])) (get-in state [:mode :sources])))
        visible (take height (drop (:offset state) (map-indexed vector rows)))]
    (cond
      (and (empty? rows) (:loading? state)) [(dim "  Loading…")]
      (empty? rows) [(dim "  No changes in this revset (press L to change it)")]
      :else
      (for [[i row] visible]
        (let [entry (get entries (:entry row))
              id (:change-id entry)
              node? (= :node (:kind row))
              cursor? (= i cursor-idx)
              in-block? (contains? block i)
              gutter (str (cond cursor? (fg 6 "▌") in-block? (fg 6 "▏") :else " ")
                          (cond
                            (and node? (contains? sources id)) (fg 3 "◆")
                            (and node? (contains? marks id)) (fg 5 "●")
                            :else " "))
              content (if (= :file (:kind row)) (file-line row) (:line row))
              line (str gutter content)]
          (cond
            cursor? (with-bg line (:cursor pal) width)
            in-block? (with-bg line (:block pal) width)
            :else (fit line width)))))))

;; ---------------------------------------------------------------------------
;; Diff pane

(defn- preview-lines [state height width]
  (let [{:keys [lines offset loading?]} (:preview state)
        entry (st/cursor-entry state)
        title (str " " (if entry (str "diff " (:short-id entry)) "diff")
                   (when-let [f (get-in state [:cursor :file])] (str "  " f))
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
        mode (:mode state)
        badge (case (:type mode)
                :pick (if (= :rebase (:action mode))
                        (str " REBASE " (:source-flag mode) " ")
                        " SQUASH INTO ")
                :picker " PICK "
                :menu " MENU "
                :confirm " CONFIRM "
                :input " INPUT "
                :help " HELP "
                " jjtui ")
        badge (paint badge 1 "38;5;15" (str "48;5;" (if (= :pick (:type mode)) 3 (:badge pal))))
        msg (:message state)
        left (cond
               (:busy state) (fg 3 (str "⟳ " (:busy state) "…"))
               (= :pick (:type mode))
               (str "Pick target for " (count (:sources mode)) " change(s): "
                    (dim (get keys/pick-help (:action mode))))
               (= :picker (:type mode)) (dim "type to filter · ↑/↓ move · enter select · esc cancel")
               (= :input (:type mode)) (dim "enter submit · esc cancel")
               (message-lines state) (dim "(see message above)")
               msg (if (= :error (:level msg)) (fg 1 (:text msg)) (:text msg))
               (:loading? state) (dim "loading…")
               :else (dim "? help"))
        right (str/join "  "
                        (remove nil?
                                [(when (seq (:marks state)) (fg 5 (str "● " (count (:marks state)) " marked")))
                                 (when (:ignore-immutable? state) (fg 1 "!immutable"))
                                 (when (:revset state) (str (dim "revset ") (:revset state)))
                                 (str (dim "diff ") (name (:diff-tool state)))]))
        width (:width state)
        right-w (width-of right)
        left-space (max 0 (- width (width-of badge) 1 right-w 1))
        left (fit left left-space)]
    (str badge " " (pad left left-space) " " right)))

;; ---------------------------------------------------------------------------
;; Help

(defn- help-columns [state]
  (let [entries (for [[title bindings] keys/groups]
                  (concat [(bold (fg 6 title))]
                          (for [[ks _ desc] bindings]
                            (str "  " (pad (fg 3 (str/join " " (map keys/display-key ks))) 14) desc))
                          [""]))
        extra [(bold (fg 6 "Modes"))
               (str "  " (pad (fg 3 "pick") 14) "j/k move, enter select, esc cancel")
               (str "  " (pad (fg 3 "rebase pick") 14) "a after, b before, s source mode")
               (str "  " (pad (fg 3 "picker") 14) "type to filter, ↑/↓, enter, esc")
               (str "  " (pad (fg 3 "menu") 14) "press the option key, esc cancel")
               ""]
        groups (concat entries [extra])
        col-w 48
        ncols (max 1 (quot (:width state) col-w))
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
  (let [lines (concat [(str (bold " jjtui") (dim "  keys  ·  esc/q/? close  ·  j/k scroll")) ""]
                      (map #(str " " %) (help-columns state)))
        offset (max 0 (min (get-in state [:mode :offset] 0) (- (count lines) height)))]
    (map #(fit % (:width state)) (take height (drop offset lines)))))

;; ---------------------------------------------------------------------------
;; Root

(defn render [state]
  (let [{:keys [width height]} state
        panel (panel-lines state)
        body-h (max 1 (- height 1 (count panel)))
        lw (log-width state)
        cursor-idx (st/cursor-index state)
        body (if (= :help (get-in state [:mode :type]))
               (help-screen state body-h)
               (let [left (log-lines state cursor-idx body-h lw)]
                 (if (:split? state)
                   (let [pw (- width lw 1)
                         right (vec (preview-lines state body-h pw))
                         left (vec left)]
                     (for [i (range body-h)]
                       (str (pad (get left i "") lw) (dim "│") (get right i ""))))
                   left)))
        body (take body-h (concat body (repeat "")))]
    (str/join "\n" (concat body panel [(status-line state)]))))
