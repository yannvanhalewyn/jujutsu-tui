(ns jjtui.keys
  "Key names and the normal-mode keymap. The keymap is also the source of the
   help screen.")

(defn key-str
  "Canonical name of a charm key-press message: \"j\", \"G\", \"enter\",
   \"space\", \"ctrl+d\", \"alt+x\", \"shift+tab\". Shift only shows up for
   named keys; uppercase letters arrive as themselves."
  [{:keys [key ctrl alt shift]}]
  (let [k (cond
            (keyword? key) (name key)
            (= " " key) "space"
            :else key)]
    (str (when ctrl "ctrl+") (when alt "alt+") (when (and shift (keyword? key)) "shift+") k)))

(def groups
  "[group-title [[keys action description] ...]] in display order."
  [["Navigation"
    [[["j" "down"] :cursor/down "Next row"]
     [["k" "up"] :cursor/up "Previous row"]
     [["g"] :cursor/top "First change"]
     [["G"] :cursor/bottom "Last change"]
     [["ctrl+d"] :cursor/half-down "Half page down"]
     [["ctrl+u"] :cursor/half-up "Half page up"]
     [["@"] :cursor/working-copy "Jump to working copy"]
     [["/"] :search "Search change ids, commit ids, descriptions"]
     [["w"] :repo/switch "Switch repo / workspace (recent, by frecency)"]
     [["R" "ctrl+r"] :refresh "Refresh"]
     [["?"] :help "Help"]
     [["q"] :quit "Quit"]]]
   ["View"
    [[["tab"] :files/toggle "Toggle files of change"]
     [["shift+tab" "i"] :files/toggle-all "Toggle files of all changes"]
     [["|"] :split/toggle "Toggle split diff pane"]
     [["J"] :preview/down "Scroll diff pane down"]
     [["K"] :preview/up "Scroll diff pane up"]
     [["ctrl+f"] :preview/page-down "Page diff pane down"]
     [["ctrl+b"] :preview/page-up "Page diff pane up"]
     [["<"] :split/shrink "Shrink log pane"]
     [[">"] :split/grow "Grow log pane"]
     [["L"] :revset/set "Set log revset"]]]
   ["Diff"
    [[["enter"] :diff/open "Diff change / marks / file"]
     [["D"] :diff/branch "Diff a branch (picker)"]
     [["v"] :diff/pick-tool "Pick diff tool"]]]
   ["Marks"
    [[["m" "space"] :mark/toggle "Toggle mark"]
     [["c" "escape"] :mark/clear "Clear marks"]]]
   ["Changes"
    [[["d"] :describe "Describe"]
     [["n"] :new "New change (merge if marked)"]
     [["N"] :new-menu "New… (after / before)"]
     [["e"] :edit "Edit change"]
     [["a"] :abandon "Abandon"]
     [["A"] :absorb "Absorb into ancestors"]
     [["s"] :squash "Squash (marks into cursor)"]
     [["S"] :squash-into "Squash into… (pick)"]
     [["r"] :rebase "Rebase (pick destination)"]
     [["X"] :split-change "Split (interactive)"]
     [["M"] :resolve "Resolve conflicts (merge tool)"]
     [["u"] :undo "Undo"]
     [["U"] :redo "Redo"]
     [["!"] :toggle-ignore-immutable "Toggle --ignore-immutable"]]]
   ["Bookmarks & remotes"
    [[["b"] :bookmark/set "Set / create bookmark"]
     [["B"] :bookmark/menu "Bookmark… (delete / rename)"]
     [["p"] :push "Push"]
     [["P"] :push-new "Push new (-c)"]
     [["f"] :fetch "Fetch"]]]])

(def keymap
  "key name -> action"
  (into {}
        (for [[_ bindings] groups
              [ks action] bindings
              k ks]
          [k action])))

(def search-keymap
  "Keys acting on the active search, ahead of the keymap: without one, n and
   N create changes and escape clears the marks."
  {"n" :search/next "N" :search/prev "escape" :search/clear})

(def pick-help
  {:rebase "enter/o onto · a after · b before · s source mode · esc cancel"
   :squash-into "enter squash into · esc cancel"})

(defn display-key [k]
  (case k
    "down" "↓"
    "up" "↑"
    "escape" "esc"
    k))
