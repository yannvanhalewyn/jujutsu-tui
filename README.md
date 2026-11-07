# jjtui

A fast, keyboard-driven terminal UI for [jujutsu](https://github.com/jj-vcs/jj).
Vim bindings, single-key mnemonic actions, and marks for batch operations on top
of jj's own colored graph log. Built with [babashka](https://babashka.org) and
[charm.clj](https://github.com/TimoKramer/charm.clj).

**[Documentation](https://yannvanhalewyn.github.io/jujutsu-tui/)** ·
[Keybindings](https://yannvanhalewyn.github.io/jujutsu-tui/#keys) ·
[Configuration](https://yannvanhalewyn.github.io/jujutsu-tui/#config) ·
[Interactive demo](https://yannvanhalewyn.github.io/jujutsu-tui/#demo)

<!--
  Media shot list. Every recording starts from a seed repo, so it can be redone:
  `bb seed <recipe>` then `cd /tmp/jjtui-<recipe> && jjtui`.
  GIFs, because GitHub only renders relative-path videos as download links.

  megamerge.png  megamerge  log with split diff pane open (`|`), cursor on the megamerge
  diff.gif       megamerge  j/k with the pane following, tab/i for file lists, enter on a
                            file, v to switch tool, D to diff feature/tags against trunk
  describe.gif   basic      d, edit subject, enter; then d + ctrl+e for the full editor
  squash.gif     basic      m m on two changes, s on a third; then S and pick a target
  rebase.gif     megamerge  r on docs/usage, r r to add sources, s to cycle -r/-s/-b,
                            a / b / enter to land after / before / onto
  split.gif      basic      X on the WIP change, pick hunks, back in the log
  resolve.gif    conflict   tab to open the conflicted change, M on cart.clj, merge, return
  switch.gif     any        w for the recent repos / workspaces picker, / to search
-->

<p align="center">
  <img src="docs/media/megamerge.png" alt="jjtui showing a megamerge: a refactor, a stacked feature, docs and dev changes merged into one working change, with the diff pane open" width="900">
</p>

## Why

- **jj's log, untouched.** jjtui renders `jj log` with your `templates.log` and
  `revsets.log`, so the graph looks exactly like it does in your shell.
- **One key per operation.** `d`escribe, `n`ew, `s`quash, `r`ebase, `a`bandon,
  `u`ndo. No command palette, no modals stacked three deep.
- **Marks for batch work.** Mark changes with `m` / `space`, then squash,
  rebase, abandon, push or diff all of them at once. `n` on marks creates a
  merge, which makes megamerge workflows a few keystrokes.
- **Your tools, not ours.** Diffs, editors, merge tools and `jj split` run in
  the real terminal; jjtui suspends, waits, and refreshes when you come back.

## Features

### Diffing

A live diff pane follows the cursor (`|`), file lists expand inline under each
change (`tab`, `i`), and `enter` opens the change, the marked set, or a single
file in your diff tool. `D` diffs a whole branch against its fork point.
Difftastic, delta, diff-so-fancy, meld, kdiff3 and VS Code are picked up
automatically when installed; switch between them with `v`.

<img src="docs/media/diff.gif" alt="Browsing changes with the diff pane, expanding files and opening difftastic" width="800">

### Describing

`d` floats a small popup over the log with the subject line ready to edit.
`enter` saves; `ctrl+e` hands the full message to your `$EDITOR`.

<img src="docs/media/describe.gif" alt="Editing a change description in the describe popup" width="800">

### Squashing and absorbing

`s` squashes the marked changes into the one under the cursor, or the cursor
into its parent when nothing is marked. `S` lets you pick the destination in
the log. `A` absorbs a change into the ancestors that last touched its lines.

<img src="docs/media/squash.gif" alt="Marking two changes and squashing them into a third" width="800">

### Rebasing

`r` enters pick mode: move the cursor to the destination and press `enter`
(onto), `a` (after) or `b` (before). Press `r` again on other changes to rebase
several at once, and `s` to cycle between moving the revision, its descendants
(`-s`) or the whole branch (`-b`).

<img src="docs/media/rebase.gif" alt="Rebasing several changes after another in pick mode" width="800">

### Splitting

`X` runs `jj split` on the change under the cursor with your configured diff
editor, then drops you back in the log with both halves visible.

<img src="docs/media/split.gif" alt="Splitting a change interactively" width="800">

### Resolving conflicts

Conflicted changes are flagged in the log. `M` opens `jj resolve` for the
change, or for just the file under the cursor when its file list is expanded.

<img src="docs/media/resolve.gif" alt="Resolving a conflicted file with the merge tool" width="800">

### And the rest

- `/` searches change ids, commit ids and descriptions; `n` / `N` step through matches.
- `w` switches between recent repos and workspaces, ranked by how often and how
  recently you visited them.
- `b` / `B` set, rename and delete bookmarks; `p` / `P` push, `f` / `F` fetch and pull.
- `L` changes the log revset on the fly; `!` toggles `--ignore-immutable`.
- `u` / `U` undo and redo through the operation log.
- In a git repo without jj, jjtui offers to run `jj git init --colocate` for you.

<img src="docs/media/switch.gif" alt="Switching between recent repositories and workspaces" width="800">

## Install

Requires [babashka](https://github.com/babashka/babashka#installation) ≥ 1.12.215
and [jj](https://jj-vcs.github.io/jj/latest/install-and-setup/).

```sh
git clone https://github.com/yannvanhalewyn/jujutsu-tui.git
ln -s "$(pwd)/jujutsu-tui/bin/jjtui" ~/bin/jjtui   # anywhere on your PATH
```

Then, inside any jj (or git) repository:

```sh
jjtui                 # jj's default log revset
jjtui -r 'trunk()..@' # or any revset
```

Press `?` for the full keymap.

## Keys at a glance

| Navigate | | Change | | Remote | |
|---|---|---|---|---|---|
| `j` `k` | move | `d` | describe | `b` | set bookmark |
| `g` `G` | first / last | `n` `N` | new / new after, before | `p` `P` | push |
| `@` | working copy | `e` | edit | `f` `F` | fetch / pull |
| `/` | search | `s` `S` | squash / squash into | `w` | switch repo |
| `tab` `i` | file lists | `r` | rebase | `L` | set revset |
| `\|` | diff pane | `X` | split | `u` `U` | undo / redo |
| `enter` | open diff | `M` | resolve | `m` | mark |

All bindings, with filtering: [docs → Keybindings](https://yannvanhalewyn.github.io/jujutsu-tui/#keys).

## Configuration

Optional EDN at `~/.config/jjtui/config.edn` (or `$JJTUI_CONFIG`, or
`--config PATH`), deep-merged over the defaults:

```clojure
{:diff-tool   :difftastic     ; preset, when difft is installed; switch with v
 :split       true            ; start with the diff pane open
 :split-ratio 0.45
 :show-files  true            ; expand file lists under every change
 :revset      "trunk()..@"

 ;; strings run through sh -c, vectors are argv, $files expands to the selection
 :diff-tools {:bat {:cmd "jj diff --git -r $revset $files | bat -l diff" :wait? true}}

 ;; what the diff pane renders
 :preview-command "jj diff --git --color=always -r $revset $files | delta --width $width"}
```

Template variables, tool presets and every key are covered in
[docs → Configuration](https://yannvanhalewyn.github.io/jujutsu-tui/#config).

## Develop

```sh
bb start [args]        # run from the checkout
bb test                # unit and jj integration tests
bb seed                # list demo repos
bb seed megamerge      # build one in /tmp/jjtui-megamerge
```

The architecture overview and roadmap live in the
[docs](https://yannvanhalewyn.github.io/jujutsu-tui/#architecture).
