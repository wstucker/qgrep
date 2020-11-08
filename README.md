qgrep
=====

qgrep is a wrapper around Emacs grep-mode enabling quick successive searches.

The canonical example is searching the current thing-at-point. The recommended setup is to bind qgrep-no-confirm to `C-c g`.

![Canonical Qgrep Example](/doc/canonical-qgrep-at-point.gif)


Setup
=====
Clone to `~/.emacs.d/qgrep`

Add to your `~/.emacs`
```elisp
(let ((default-directory "~/.emacs.d/qgrep/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(autoload 'qgrep "qgrep" "Quick grep" t)
(autoload 'qgrep-no-confirm "qgrep" "Quick grep" t)
(autoload 'qgrep-confirm "qgrep" "Quick grep" t)
(global-set-key (kbd "\C-c g") 'qgrep-no-confirm)
(global-set-key (kbd "\C-c G") 'qgrep-confirm)
;; Stricter filters
(setq qgrep-default-find "find . \\(  -wholename '*/.git' \\) -prune -o -type f \\( '!' -name '*.drawio' -a \\( '!' -name '*~' \\) -a \\( '!' -name '#*#' \\) -a \\( -name '*' \\) \\) -type f -print0")
(setq qgrep-default-grep "grep -iI -nH -e \"%s\"")
```

Examples
========

If text is currently selected, then that will be searched instead.

![Text Selected Qgrep](/doc/text-selected-qgrep.gif)

Using `C-c G` (note uppercase `g`) will prompt for the command in three parts:
* directory to search
* the find command to filter files
* the grep command to execute

![Interactive Qgrep](/doc/interactive-qgrep.gif)

Once in the grep window, there are a few extensions to the standard grep-mode. Example, hitting `u` will jump _up_ a directory and repeat the search.

![Qgrep Up Directory](/doc/qgrep-repeat-up.gif)

Hitting `r` in the grep window will prompt to refine the search.

![Qgrep Refine](/doc/qgrep-refine.gif)

Other options include:
* `c` flush comments (limited filetypes supported)
* `k` keep-lines  (allows pruning results without rerunning search) (not shown)
* `f` flush-lines (allows pruning results without rerunning search)
* `g` repeat search (native to grep-mode)
* `Q` kill all qgrep buffers (each qgrep is run in a separate buffer to make back-tracking easier)

![Qgrep Refine](/doc/qgrep-other-options.gif)
