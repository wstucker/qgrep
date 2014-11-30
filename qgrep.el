;;; qgrep.el --- Quick access to grep

;; Copyright (C) 2013-2014
;; Free Software Foundation, Inc.

;; Author: William Stucker <wstucker@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary
;;
;; This package provides the ability to "grep" at point; allowing for
;; multiple successive greps to quickly navigate through project
;; trees. It was designed as a cheap replacement to ctags in a mixed
;; language environment where several languages do not currently have
;; ctag parsers.
;;
;; The find and the grep have been separated into two histories with
;; different defaults. You can easily customize the default find
;; command and the default grep. See configuration variables.
;;
;; Features
;; 1. Grep for symbol at point (or currently selected region):
;;    (quick-grep)
;; 2. Interactive grep similar to lgrep/rgrep in grep-mode
;;    (quick-grep-confirm)
;; 3. Additional filtering of results
;;    r - refine (rerun grep with different arguments
;;    u - run one directory level higher
;;    f - flush lines
;;    k - keep lines
;; 4. Refinement: confirm the arguments of the previous command
;;    interactively.
;; 5. Unique buffer naming: keep previous searches around for
;;    reference. Kill all qgrep buffers with 'Q'.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Setup
;;
;; ;; qgrep setup
;; (autoload 'qgrep "qgrep" "Quick grep" t)
;; (autoload 'qgrep-no-confirm "qgrep" "Quick grep" t)
;; (autoload 'qgrep-confirm "qgrep" "Quick grep" t)
;; (global-set-key (kbd "\C-c g") 'qgrep-no-confirm)
;; (global-set-key (kbd "\C-c G") 'qgrep-confirm)
;; ;; Stricter filters
;; (setq qgrep-default-find "find . \\( -wholename '*/.svn' -o -wholename '*/obj' -o -wholename '*/.git' -o -wholename '*/sim' -o -wholename '*/VCOMP' \\) -prune -o -type f \\( '!' -name '*atdesignerSave.ses' -a \\( -name '*' \\) \\) -type f -print0")
;; (setq qgrep-default-grep "grep -iI -nH -e \"%s\"")
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dependencies

(require 'grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration variables

(defvar qgrep-default-grep
  "grep -nHe \"%s\""
  "The default grep command.")

(defvar qgrep-default-find
  "find . -name \"*\" -type f -print0"
  "The default find command.")

(defvar qgrep-default-find-grep-link
  "%s | xargs -0 %s"
  "Format string to link the find command to the grep command.
Should contain two %-sequences which will be substituted for
'qgrep-default-find' and 'gqrep-default-find'.")

(defvar qgrep-name-uniquely t
  "If t, name each buffer created by qgrep uniquely by search
  path and pattern.")

(defvar qgrep-comment-list
  '((".el" ";")
    (".c" "//")
    (".cc" "//")
    (".h" "//")
    (".py" "#")
    (".pl" "#"))
  "List of regexp-regexp pairs of file extension to comment
  character.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State variables

(defvar qgrep-grep-history nil
  "History list for previous greps.")

(defvar qgrep-find-history nil
  "History list for previous finds.")

(defvar qgrep-grep-command nil
  "The grep command associated with this qgrep buffer. This
variable becomes buffer-local when qgrep-mode is started. Its
value is assigned by peeking at the top of the qgrep-grep-history
list.")

(defvar qgrep-find-command nil
  "The find command associated with this qgrep buffer. This
variable becomes buffer-local when qgrep-mode is started. Its
value is assigned by peeking at the top of the qgrep-grep-history
list.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User accessible functions

(defun qgrep (confirm)
  "Grep on current symbol or active region region. If CONFIRM,
require user to confirm arguments."
  (interactive "P")
  (let ((original-dd default-directory))
    (qgrep-parse-args confirm)
    (qgrep-launch-compile qgrep-find-command qgrep-grep-command)
    (setq default-directory original-dd)))

(defun qgrep-no-confirm ()
  "Call qgrep without confirming arguments"
  (interactive)
  (qgrep nil))

(defun qgrep-confirm ()
  "Call qgrep and require user to confirm arguments."
  (interactive)
  (qgrep t))

(defun qgrep-kill-all-qgrep-buffers ()
  "Kill all existing qgrep buffers. Buffers can easily accumulate
when using unique names, this provides an easy way to get rid of
them all quickly."
  (interactive)
  (mapc (lambda (x)
          (with-current-buffer x
            (when (eq major-mode 'qgrep-mode)
              (kill-buffer x))))
        (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter/refinement functions
;;
;; Use these functions to filter and refine search results.

(defun qgrep-refine ()
  "Rerun the current grep but allow the user to confirm/change
arguments."
  (interactive)
  (qgrep-parse-args t)
  (let* ((new-buf-name (qgrep-name-buffer 'qgrep-mode))
         (new-buf (get-buffer new-buf-name)))
    (when new-buf
      (unless (string= new-buf-name (buffer-name))
        (kill-buffer new-buf)))
    (rename-buffer new-buf-name))
  (qgrep-launch-compile qgrep-find-command qgrep-grep-command))

(defun qgrep-flush-lines (regexp)
  "Filter out results using flush-lines."
  (interactive "sFlush lines containing match for regexp:Flush lines containing match for regexp: ")
  (qgrep-filter 'flush-lines regexp))

(defun qgrep-keep-lines (regexp)
  "Filter out results using keep-lines."
  (interactive "sKeep lines containing match for regexp: ")
  (qgrep-filter 'keep-lines regexp))

(defun qgrep-keep-comments ()
  "Keep only lines that are commented out."
  (interactive)
  (qgrep-keep-lines (qgrep-build-comment-regexp)))

(defun qgrep-flush-comments ()
  "Flush lines that are commented out."
  (interactive)
  (qgrep-flush-lines (qgrep-build-comment-regexp)))

(defun qgrep-up-directory (levels)
  "Run the current find and grep command at LEVELS directory levels higher."
  (interactive "p")
  (while (> levels 0)
    (setq default-directory (concat default-directory "../"))
    (setq levels (- levels 1)))
  (setq default-directory (expand-file-name default-directory))
  (let* ((new-buf-name (qgrep-name-buffer 'qgrep-mode))
         (new-buf (get-buffer new-buf-name)))
    (when new-buf
      (unless (string= new-buf-name (buffer-name))
        (kill-buffer new-buf)))
    (rename-buffer new-buf-name))
  (qgrep-launch-compile qgrep-find-command qgrep-grep-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal functions

(defun qgrep-build-comment-regexp ()
  "Build a regexp to search for commented-out lines by file
extension based on the 'qgrep-comment-list'."
  (let ((regexp (mapconcat (lambda (x)
                             (format "\\(^[^ \t\n]+\\(%s\\):[0-9]+:[ \t]*\\(%s\\)\\)" (car x) (car (cdr x))))
                           qgrep-comment-list
                           "\\|")))
    (message "%s" regexp)
    regexp))

(defun qgrep-parse-args (confirm)
  "Grab all the arguments necessary to run a grep command."
  (setq default-directory (qgrep-determine-dired confirm))
  (setq qgrep-find-command (qgrep-determine-find confirm))
  (setq qgrep-grep-command (qgrep-determine-grep confirm)))

(defun qgrep-grab-text ()
  "If the region is active, return it as a string, otherwise
return thing at point."
  (if (use-region-p)
      (buffer-substring-no-properties (point) (mark))
    (let ((symb (thing-at-point 'symbol)))
      (if symb
          (substring-no-properties symb)
        ""))))

(defun qgrep-determine-grep (confirm)
  "Determine the grep command based on default value, point, previous search, or
user input."
  (let ((text (qgrep-grab-text)))
    (when (string= text "") ;; Don't allow for empty search
      (setq confirm t))
    (let* ((default-grep (format qgrep-default-grep text)))
      (when (eq major-mode 'qgrep-mode) ;; Doing a refine
        (setq default-grep qgrep-grep-command))
      (when confirm
        (setq default-grep (read-string "Grep command: "
                                        default-grep
                                        'qgrep-grep-history
                                        nil
                                        nil)))
      default-grep)))

(defun qgrep-determine-find (confirm)
  "Determine the find command based on default value, previous
find, or user input."
  (let ((default-find qgrep-default-find))
    (when (eq major-mode 'qgrep-mode) ;; Doing a refine
      (setq default-find qgrep-find-command))
    (when confirm
      (setq default-find (read-string "Find command: "
                                      default-find
                                      'qgrep-find-history
                                      nil
                                      nil)))
    default-find))

(defun qgrep-determine-dired (confirm)
  "Determine which directory to run in. Default to
default-directory."
  (if (not confirm)
      default-directory
    (read-directory-name "Directory to search: "
                         nil
                         nil
                         t
                         nil)))

(defun qgrep-name-buffer (mode)
  "Name qgrep buffers uniquely based on grep and directory if
'qgrep-name-uniquely' is t, otherwise just call it \"*qgrep*\"."
  (if qgrep-name-uniquely
      (format "*qgrep* %s @ %s" qgrep-grep-command default-directory)
    "*qgrep*"))

(defun qgrep-launch-compile (find-command grep-command)
  "Launch the actual compilation command."
  ;; Push find and grep in to history lists. When qgrep mode is
  ;; initiated, peek at the top item in the list and make it a
  ;; buffer-local variable for reference. Not sure of an easier
  ;; approach to get the find a grep information stored.n
  (add-to-history 'qgrep-find-history find-command)
  (add-to-history 'qgrep-grep-history grep-command)
  (let ((command (format qgrep-default-find-grep-link find-command grep-command)))
    (compilation-start command
                       'qgrep-mode
                       'qgrep-name-buffer)))

(defun qgrep-results-start ()
  "Return the starting position of the results section."
  (save-excursion
    (goto-line 5)
    (beginning-of-line)
    (point)))

(defun qgrep-results-end ()
  "Return the ending position of the results section."
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (previous-line 2)
    (point)))

(defun qgrep-filter (filter-function &optional regexp)
  "Filter results using flush-lines or keep-lines."
  (save-excursion
    (setq inhibit-read-only t)
    (narrow-to-region (qgrep-results-start) (qgrep-results-end))
    (funcall filter-function regexp)
    (widen)
    (setq inhibit-read-only t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definition and key bindings

(define-derived-mode qgrep-mode grep-mode "Grep"
  "qgrep-mode allows for quick access to repeated greps."
  (make-local-variable 'qgrep-find-command)
  (make-local-variable 'qgrep-grep-command)
  (setq qgrep-find-command (car qgrep-find-history))
  (setq qgrep-grep-command (car qgrep-grep-history)))

(defun qgrep-bindkeys ()
  "Bind keys for additional functionality in qgrep-mode"
  (local-set-key (kbd "c") 'qgrep-flush-comments)
  (local-set-key (kbd "C") 'qgrep-keep-comments)
  (local-set-key (kbd "f") 'qgrep-flush-lines)
  (local-set-key (kbd "k") 'qgrep-keep-lines)
  (local-set-key (kbd "u") 'qgrep-up-directory)
  (local-set-key (kbd "Q") 'qgrep-kill-all-qgrep-buffers)
  (local-set-key (kbd "r") 'qgrep-refine))

(add-hook 'qgrep-mode-hook 'qgrep-bindkeys)

(provide 'qgrep)
