;;; helm-gtags2.el --- GNU GLOBAL helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags2
;; Version: 1.5.6
;; Package-Requires: ((emacs "27.1") (helm "3.6.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `helm-gtags2.el' is a `helm' interface of GNU Global.
;; `helm-gtags2.el' is not compatible `anything-gtags.el', but `helm-gtags2.el'
;; is designed for fast search.

;;
;; To use this package, add these lines to your init.el or .emacs file:
;;
;;     ;; Enable helm-gtags2-mode
;;     (add-hook 'c-mode-hook 'helm-gtags2-mode)
;;     (add-hook 'c++-mode-hook 'helm-gtags2-mode)
;;     (add-hook 'asm-mode-hook 'helm-gtags2-mode)
;;
;;     ;; Set key bindings
;;     (eval-after-load "helm-gtags2"
;;       '(progn
;;          (define-key helm-gtags2-mode-map (kbd "M-t") 'helm-gtags2-find-tag)
;;          (define-key helm-gtags2-mode-map (kbd "M-r") 'helm-gtags2-find-rtag)
;;          (define-key helm-gtags2-mode-map (kbd "M-s") 'helm-gtags2-find-symbol)
;;          (define-key helm-gtags2-mode-map (kbd "C-c <") 'helm-gtags2-previous-history)
;;          (define-key helm-gtags2-mode-map (kbd "C-c >") 'helm-gtags2-next-history)
;;          (define-key helm-gtags2-mode-map (kbd "M-,") 'helm-gtags2-pop-stack)))
;;

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-files)
(require 'which-func)
(require 'pulse)
(require 'subr-x)

(declare-function helm-comp-read "helm-mode")

(defgroup helm-gtags2 nil
  "GNU GLOBAL for helm."
  :group 'helm)

(defcustom helm-gtags2-pulse-at-cursor t
  "If non-nil, pulse at point after jumping"
  :type 'boolean)

(defcustom helm-gtags2-highlight-candidate t
  "Highlight candidate or not"
  :type 'boolean)

(defcustom helm-gtags2-maximum-candidates 9999
  "Maximum number of helm candidates"
  :type 'integer)

(defface helm-gtags2-file
  '((t :inherit font-lock-keyword-face))
  "Face for line numbers in the error list.")

(defface helm-gtags2-lineno
  '((t :inherit font-lock-doc-face))
  "Face for line numbers in the error list.")

(defface helm-gtags2-match
  '((t :inherit helm-match))
  "Face for word matched against tagname")

(defvar helm-gtags2--tag-location nil)
(defvar helm-gtags2--last-update-time 0)
(defvar helm-gtags2--completing-history nil)
(defvar helm-gtags2--context-stack (make-hash-table :test 'equal))
(defvar helm-gtags2--result-cache (make-hash-table :test 'equal))
(defvar helm-gtags2--saved-context nil)
(defvar helm-gtags2--use-otherwin nil)
(defvar helm-gtags2--local-directory nil)
(defvar helm-gtags2--current-position nil)
(defvar helm-gtags2--real-tag-location nil)
(defvar helm-gtags2--last-input nil)
(defvar helm-gtags2--query nil)
(defvar helm-gtags2--last-default-directory nil)

(defconst helm-gtags2--buffer "*helm gtags*")

(defconst helm-gtags2--include-regexp
  "\\`\\s-*#\\(?:include\\|import\\)\\s-*[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]")

;; completsion function for completing-read.
(defun helm-gtags2--completing-gtags (string predicate code)
  (helm-gtags2--complete 'tag string predicate code))
(defun helm-gtags2--completing-pattern (string predicate code)
  (helm-gtags2--complete 'pattern string predicate code))
(defun helm-gtags2--completing-grtags (string predicate code)
  (helm-gtags2--complete 'rtag string predicate code))
(defun helm-gtags2--completing-gsyms (string predicate code)
  (helm-gtags2--complete 'symbol string predicate code))
(defun helm-gtags2--completing-files (string predicate code)
  (helm-gtags2--complete 'find-file string predicate code))

(defconst helm-gtags2-comp-func-alist
  '((tag       . helm-gtags2--completing-gtags)
    (pattern   . helm-gtags2--completing-pattern)
    (rtag      . helm-gtags2--completing-grtags)
    (symbol    . helm-gtags2--completing-gsyms)
    (find-file . helm-gtags2--completing-files)))

(defconst helm-gtags2--search-option-alist
  '((pattern   . "-g")
    (rtag      . "-r")
    (symbol    . "-s")
    (find-file . "-Poa")))

(defsubst helm-gtags2--windows-p ()
  (memq system-type '(windows-nt ms-dos)))

(defun helm-gtags2--remove-carrige-returns ()
  (when (helm-gtags2--windows-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\xd" nil t)
        (replace-match "")))))

;; Work around for GNU global Windows issue
(defsubst helm-gtags2--use-abs-path-p (gtagslibpath)
  (and (helm-gtags2--windows-p) gtagslibpath))

(defun helm-gtags2--construct-options (type completion)
  (let ((find-file-p (eq type 'find-file))
        (gtagslibpath (getenv "GTAGSLIBPATH"))
        options)
    (unless find-file-p
      (push "--result=grep" options))
    (when completion
      (push "-c" options))
    (helm-aif (assoc-default type helm-gtags2--search-option-alist)
        (push it options))
    (when (helm-gtags2--use-abs-path-p gtagslibpath)
      (push "-a" options))
    (when (and current-prefix-arg (not find-file-p))
      (push "-l" options))
    (when gtagslibpath
      (push "-T" options))
    options))

(defun helm-gtags2--complete (type string predicate code)
  (let* ((options (helm-gtags2--construct-options type t))
         (args (reverse (cons string options)))
         candidates)
    (with-temp-buffer
      (apply #'process-file "global" nil t nil args)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (match-string-no-properties 1) candidates)))
    (if (not code)
        (try-completion string candidates predicate)
      (all-completions string candidates predicate))))

(defun helm-gtags2--token-at-point (type)
  (if (not (eq type 'find-file))
      (thing-at-point 'symbol)
    (let ((line (helm-current-line-contents)))
      (when (string-match helm-gtags2--include-regexp line)
        (match-string-no-properties 1 line)))))

(defconst helm-gtags2--prompt-alist
  '((tag       . "Find Definition: ")
    (pattern   . "Find Pattern: ")
    (rtag      . "Find Reference: ")
    (symbol    . "Find Symbol: ")
    (find-file . "Find File: ")))

(defun helm-gtags2--read-tagname (type &optional default-tagname)
  (let ((tagname (helm-gtags2--token-at-point type))
        (prompt (assoc-default type helm-gtags2--prompt-alist))
        (comp-func (assoc-default type helm-gtags2-comp-func-alist)))
    (when (and (not tagname) default-tagname)
      (setq tagname default-tagname))
    (when tagname
      (setq prompt (format "%s(default \"%s\") " prompt tagname)))
    (let ((completing-read-function 'completing-read-default))
      (completing-read prompt comp-func nil nil nil
                       'helm-gtags2--completing-history tagname))))

(defun helm-gtags2--path-libpath-p (tagroot)
  (helm-aif (getenv "GTAGSLIBPATH")
      (cl-loop for path in (parse-colon-path it)
               for libpath = (file-name-as-directory (expand-file-name path))
               thereis (string= tagroot libpath))))

(defun helm-gtags2--tag-directory ()
  (with-temp-buffer
    (helm-aif (getenv "GTAGSROOT")
        it
      (unless (zerop (process-file "global" nil t nil "-p"))
        (error "GTAGS not found"))
      (goto-char (point-min))
      (when (looking-at "^\\([^\r\n]+\\)")
        (let ((tag-path (match-string-no-properties 1)))
          (file-name-as-directory tag-path))))))

(defun helm-gtags2--find-tag-directory ()
  (setq helm-gtags2--real-tag-location nil)
  (let ((tagroot (helm-gtags2--tag-directory)))
    (if (and (helm-gtags2--path-libpath-p tagroot) helm-gtags2--tag-location)
        (progn
          (setq helm-gtags2--real-tag-location tagroot)
          helm-gtags2--tag-location)
      (setq helm-gtags2--tag-location tagroot))))

(defun helm-gtags2--base-directory ()
  (let ((dir (or helm-gtags2--last-default-directory
                 helm-gtags2--local-directory
                 helm-gtags2--real-tag-location
                 helm-gtags2--tag-location))
        (remote (file-remote-p default-directory)))
    (if (and remote (not (file-remote-p dir)))
        (concat remote dir)
      dir)))

(defsubst helm-gtags2--new-context-info (index stack)
  (list :index index :stack stack))

(defun helm-gtags2--put-context-stack (tag-location index stack)
  (puthash tag-location (helm-gtags2--new-context-info index stack)
           helm-gtags2--context-stack))

(defsubst helm-gtags2--current-context ()
  (let ((file (buffer-file-name (current-buffer))))
    (list :file file :position (point) :readonly buffer-file-read-only)))

(defsubst helm-gtags2--save-current-context ()
  (setq helm-gtags2--saved-context (helm-gtags2--current-context)))

(defun helm-gtags2--get-context-info ()
  (let* ((tag-location (helm-gtags2--find-tag-directory))
         (context-info (gethash tag-location helm-gtags2--context-stack))
         (context-stack (plist-get context-info :stack)))
    (if (null context-stack)
        (error "Context stack is empty(TAG at %s)" tag-location)
      context-info)))

(defun helm-gtags2--get-or-create-context-info ()
  (or (gethash helm-gtags2--tag-location helm-gtags2--context-stack)
      (helm-gtags2--new-context-info -1 nil)))

(defun helm-gtags2--move-to-context (context)
  (let ((file (plist-get context :file))
        (curpoint (plist-get context :position))
        (readonly (plist-get context :readonly)))
    (if readonly
        (find-file-read-only file)
      (find-file file))
    (goto-char curpoint)
    (recenter)))

(defun helm-gtags2-next-history ()
  "Jump to next position on context stack"
  (interactive)
  (let* ((context-info (helm-gtags2--get-context-info))
         (current-index (plist-get context-info :index))
         (context-stack (plist-get context-info :stack))
         context)
    (when (<= current-index -1)
      (error "This context is latest in context stack"))
    (setf (nth current-index context-stack) (helm-gtags2--current-context))
    (cl-decf current-index)
    (if (= current-index -1)
        (setq context helm-gtags2--current-position
              helm-gtags2--current-position nil)
      (setq context (nth current-index context-stack)))
    (helm-gtags2--put-context-stack helm-gtags2--tag-location
                                    current-index context-stack)
    (helm-gtags2--move-to-context context)))

(defun helm-gtags2-previous-history ()
  "Jump to previous position on context stack"
  (interactive)
  (let* ((context-info (helm-gtags2--get-context-info))
         (current-index (plist-get context-info :index))
         (context-stack (plist-get context-info :stack))
         (context-length (length context-stack)))
    (cl-incf current-index)
    (when (>= current-index context-length)
      (error "This context is last in context stack"))
    (if (= current-index 0)
        (setq helm-gtags2--current-position (helm-gtags2--current-context))
      (setf (nth (- current-index 1) context-stack) (helm-gtags2--current-context)))
    (let ((prev-context (nth current-index context-stack)))
      (helm-gtags2--move-to-context prev-context))
    (helm-gtags2--put-context-stack helm-gtags2--tag-location
                                    current-index context-stack)))

(defun helm-gtags2--referer-function (file ref-line)
  (let ((is-opened (cl-loop with path = (concat default-directory file)
                            for buf in (buffer-list)
                            when (string= (buffer-file-name buf) path)
                            return it))
        retval)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (forward-line (1- ref-line))
      (unless (zerop (current-indentation))
        (setq retval (which-function)))
      (unless is-opened
        (kill-buffer (current-buffer)))
      retval)))

(defun helm-gtags2--print-path-in-gtagslibpath (args)
  (let ((libpath (getenv "GTAGSLIBPATH")))
    (when libpath
      (dolist (path (parse-colon-path libpath))
        (let ((default-directory (file-name-as-directory path)))
          (apply #'process-file "global" nil t nil "-Poa" args))))))

(defun helm-gtags2--exec-global-command (type input)
  (let ((args (helm-gtags2--construct-command type input)))
    (helm-gtags2--find-tag-directory)
    (helm-gtags2--save-current-context)
    (let ((buf-coding buffer-file-coding-system))
      (with-current-buffer (helm-candidate-buffer 'global)
        (let ((default-directory (helm-gtags2--base-directory))
              (input (car (last args)))
              (coding-system-for-read buf-coding)
              (coding-system-for-write buf-coding))
          (unless (zerop (apply #'process-file "global" nil '(t nil) nil args))
            (error (format "%s: not found" input)))
          ;; --path options does not support searching under GTAGSLIBPATH
          (when (eq type 'find-file)
            (helm-gtags2--print-path-in-gtagslibpath args))
          (helm-gtags2--remove-carrige-returns))))))

(defun helm-gtags2--construct-command (type &optional in)
  (setq helm-gtags2--local-directory nil)
  (let ((dir (helm-get-attr 'helm-gtags2-base-directory (helm-get-current-source))))
    (when (and dir (not (eq type 'find-file)))
      (setq helm-gtags2--local-directory dir)))
  (let ((input (or in helm-gtags2--query))
        (options (helm-gtags2--construct-options type nil)))
    (when (string-empty-p input)
      (error "Input is empty!!"))
    (setq helm-gtags2--last-input input)
    (reverse (cons input options))))

(defun helm-gtags2--tags-init (&optional input)
  (helm-gtags2--exec-global-command 'tag input))

(defun helm-gtags2--pattern-init (&optional input)
  (helm-gtags2--exec-global-command 'pattern input))

(defun helm-gtags2--rtags-init (&optional input)
  (helm-gtags2--exec-global-command 'rtag input))

(defun helm-gtags2--gsyms-init ()
  (helm-gtags2--exec-global-command 'symbol nil))

(defun helm-gtags2--files-init ()
  (helm-gtags2--exec-global-command 'find-file nil))

(defun helm-gtags2--real-file-name ()
  (let ((buffile (buffer-file-name)))
    (unless buffile
      (error "This buffer is not related to file."))
    (file-truename buffile)))

(defun helm-gtags2--find-tag-from-here-init ()
  (helm-gtags2--find-tag-directory)
  (helm-gtags2--save-current-context)
  (let ((token (helm-gtags2--token-at-point 'from-here)))
    (unless token
      (error "Cursor is not on symbol."))
    (let* ((filename (helm-gtags2--real-file-name))
           (from-here-opt (format "--from-here=%d:%s" (line-number-at-pos) filename)))
      (setq helm-gtags2--last-input token)
      (with-current-buffer (helm-candidate-buffer 'global)
        (let* ((default-directory (helm-gtags2--base-directory))
               (status (process-file "global" nil '(t nil) nil
                                     "--result=grep" from-here-opt token)))
          (helm-gtags2--remove-carrige-returns)
          (unless (zerop status)
            (cond ((= status 1)
                   (error "Error: %s%s" (buffer-string) filename))
                  ((= status 3)
                   (error "Error: %s" (buffer-string)))
                  (t (error "%s: not found" token)))))))))

(defun helm-gtags2--push-context (context)
  (let* ((context-info (helm-gtags2--get-or-create-context-info))
         (current-index (plist-get context-info :index))
         (context-stack (plist-get context-info :stack)))
    (unless (= current-index -1)
      (setq context-stack (nthcdr (1+ current-index) context-stack)))
    (setq helm-gtags2--current-position nil)
    (push context context-stack)
    (helm-gtags2--put-context-stack helm-gtags2--tag-location -1 context-stack)))

(defun helm-gtags2--do-open-file (open-func file line)
  (funcall open-func file)
  (goto-char (point-min))
  (forward-line (1- line))
  (back-to-indentation)
  (recenter)
  (helm-gtags2--push-context helm-gtags2--saved-context)
  (when helm-gtags2-pulse-at-cursor
    (pulse-momentary-highlight-one-line (point))))

(defun helm-gtags2--find-line-number (cand)
  (if (string-match "\\s-+\\([1-9][0-9]+\\)\\s-+" cand)
      (string-to-number (match-string-no-properties 1 cand))
    (error "Can't find line number in %s" cand)))

(defsubst helm-gtags2--has-drive-letter-p (path)
  (string-match-p "\\`[a-zA-Z]:" path))

(defun helm-gtags2--extract-file-and-line (cand)
  (if (and (helm-gtags2--windows-p) (helm-gtags2--has-drive-letter-p cand))
      (when (string-match "\\(\\`[a-zA-Z]:[^:]+\\):\\([^:]+\\)" cand)
        (cons (match-string-no-properties 1 cand)
              (string-to-number (match-string-no-properties 2 cand))))
    (let ((elems (split-string cand ":")))
      (cons (cl-first elems) (string-to-number (cl-second elems))))))

(defun helm-gtags2--action-openfile (cand &optional func)
  (let* ((file-and-line (helm-gtags2--extract-file-and-line cand))
         (filename (car file-and-line))
         (line (cdr file-and-line))
         (default-directory (helm-gtags2--base-directory)))
    (helm-gtags2--do-open-file (or func #'find-file) filename line)))

(defun helm-gtags2--action-openfile-other-window (cand)
  (helm-gtags2--action-openfile cand #'find-file-other-window))

(defun helm-gtags2--file-content-at-pos (file pos)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (format "%s:%d:%s:%s"
              file (line-number-at-pos)
              (helm-aif (which-function) (format "[%s]" it) "")
              (helm-current-line-contents)))))

(defun helm-gtags2--files-candidate-transformer (file)
  (let ((removed-regexp (concat "\\`" helm-gtags2--tag-location)))
    (replace-regexp-in-string removed-regexp "" file)))

(defun helm-gtags2--show-stack-init ()
  (cl-loop with context-stack = (plist-get (helm-gtags2--get-context-info) :stack)
           with stack-length = (length context-stack)
           for context in (reverse context-stack)
           for file = (plist-get context :file)
           for pos  = (plist-get context :position)
           for index = (1- stack-length) then (1- index)
           for line = (helm-gtags2--file-content-at-pos file pos)
           for cand = (helm-gtags2--files-candidate-transformer line)
           collect (cons cand (propertize cand 'index index))))

(defun helm-gtags2--persistent-action (cand)
  (let* ((file-and-line (helm-gtags2--extract-file-and-line cand))
         (filename (car file-and-line))
         (line (cdr file-and-line))
         (default-directory (helm-gtags2--base-directory)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))

(defvar helm-gtags2--find-file-action
  (helm-make-actions
   "Open file" #'helm-gtags2--action-openfile
   "Open file other window" #'helm-gtags2--action-openfile-other-window))

(defvar helm-source-gtags2-tags
  (helm-build-in-buffer-source "Jump to definitions"
    :init 'helm-gtags2--tags-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :real-to-display 'helm-gtags2--candidate-transformer
    :persistent-action 'helm-gtags2--persistent-action
    :action helm-gtags2--find-file-action))

(defvar helm-source-gtags2-rtags
  (helm-build-in-buffer-source "Jump to references"
    :init 'helm-gtags2--rtags-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :real-to-display 'helm-gtags2--candidate-transformer
    :persistent-action 'helm-gtags2--persistent-action
    :action helm-gtags2--find-file-action))

(defvar helm-source-gtags2-gsyms
  (helm-build-in-buffer-source "Jump to symbols"
    :init 'helm-gtags2--gsyms-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :real-to-display 'helm-gtags2--candidate-transformer
    :persistent-action 'helm-gtags2--persistent-action
    :action helm-gtags2--find-file-action))

(defun helm-gtags2--highlight-candidate (candidate)
  (let ((regexp (concat "\\<" helm-gtags2--last-input "\\>"))
        (limit (1- (length candidate)))
        (last-pos 0)
        (case-fold-search nil))
    (while (and (< last-pos limit)
                (string-match regexp candidate last-pos))
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'helm-gtags2-match
                         candidate)
      (setq last-pos (1+ (match-end 0))))
    candidate))

(defun helm-gtags2--transformer-regexp (candidate)
  (if (and (helm-gtags2--windows-p) (helm-gtags2--has-drive-letter-p candidate))
      "\\`\\([a-zA-Z]:[^:]+\\):\\([^:]+\\):\\(.*\\)"
    "\\`\\([^:]+\\):\\([^:]+\\):\\(.*\\)"))

(defun helm-gtags2--candidate-transformer (candidate)
  (if (not helm-gtags2-highlight-candidate)
      candidate
    (let ((regexp (helm-gtags2--transformer-regexp candidate)))
      (when (string-match regexp candidate)
        (format "%s:%s:%s"
                (propertize (match-string 1 candidate) 'face 'helm-gtags2-file)
                (propertize (match-string 2 candidate) 'face 'helm-gtags2-lineno)
                (helm-gtags2--highlight-candidate (match-string 3 candidate)))))))

(defun helm-gtags2--file-name (name)
  (let ((remote (file-remote-p default-directory)))
    (if (not remote)
        name
      (concat remote name))))

(defun helm-gtags2--find-file-common (open-fn cand)
  (let ((default-directory (helm-gtags2--base-directory)))
    (funcall open-fn (helm-gtags2--file-name cand))))

(defun helm-gtags2--find-file (cand)
  (helm-gtags2--find-file-common #'find-file cand))

(defun helm-gtags2--find-file-other-window (cand)
  (helm-gtags2--find-file-common #'find-file-other-window cand))

(defvar helm-gtags2--file-util-action
  (helm-make-actions
   "Open file" #'helm-gtags2--find-file
   "Open file other window" #'helm-gtags2--find-file-other-window))

(defun helm-gtags2--file-persistent-action (cand)
  (let ((default-directory (with-helm-current-buffer
                             default-directory)))
    (helm-ff-kill-or-find-buffer-fname (helm-gtags2--file-name cand))))

(defun helm-gtags2--searched-directory ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 (let ((dir (read-directory-name "Input Directory: ")))
         (setq helm-gtags2--local-directory (file-name-as-directory dir))))
    (16 (file-name-directory (buffer-file-name)))))

(defsubst helm-gtags2--using-other-window-p ()
  (< (prefix-numeric-value current-prefix-arg) 0))

(defun helm-gtags2--make-gtags-sentinel (action)
  (lambda (process _event)
    (when (eq (process-status process) 'exit)
      (if (zerop (process-exit-status process))
          (message "Success: %s TAGS" action)
        (message "Failed: %s TAGS(%d)" action (process-exit-status process))))))

(defsubst helm-gtags2--read-gtagslabel ()
  (let ((labels '("default" "native" "ctags" "new-ctags" "pygments")))
    (completing-read "GTAGSLABEL(Default: default): " labels nil t nil nil "default")))

(defsubst helm-gtags2--label-option (label)
  (concat "--gtagslabel=" label))

(defun helm-gtags2--find-tag-simple ()
  (or (getenv "GTAGSROOT")
      (locate-dominating-file default-directory "GTAGS")
      (if (not (yes-or-no-p "File GTAGS not found. Run 'gtags'? "))
          (user-error "Abort")
        (let* ((tagroot (read-directory-name "Root Directory: "))
               (label (helm-gtags2--read-gtagslabel))
               (default-directory tagroot))
          (message "gtags is generating tags....")
          (let ((label-opt (helm-gtags2--label-option label)))
            (unless (zerop (process-file "gtags" nil nil nil "-q" label-opt))
              (error "Failed: 'gtags -q %s'" label-opt)))
          tagroot))))

(defun helm-gtags2--current-file-and-line ()
  (let* ((buffile (buffer-file-name))
         (path (file-relative-name buffile (helm-gtags2--find-tag-directory))))
    (format "%s:%d" path (line-number-at-pos))))

(defsubst helm-gtags2--clear-variables ()
  (setq helm-gtags2--last-default-directory nil))

(defun helm-gtags2--common (srcs tagname)
  (helm-gtags2--clear-variables)
  (let ((helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (dir (helm-gtags2--searched-directory))
        (src (car srcs)))
    (when (symbolp src)
      (setq src (symbol-value src)))
    (unless helm-gtags2--use-otherwin
      (setq helm-gtags2--use-otherwin (helm-gtags2--using-other-window-p)))
    (when tagname
      (setq helm-gtags2--query tagname))
    (let ((tagroot (helm-gtags2--find-tag-simple)))
      (helm-set-attr 'helm-gtags2-base-directory dir src)
      (when tagname
        (helm-set-attr 'name (format "%s in %s" tagname (or dir tagroot)) src))
      (helm :sources srcs :buffer helm-gtags2--buffer))))

(defun helm-gtags2-find-tag (tag)
  "Jump to definition"
  (interactive
   (list (helm-gtags2--read-tagname 'tag)))
  (helm-gtags2--common '(helm-source-gtags2-tags) tag))

(defun helm-gtags2-find-rtag (tag)
  "Jump to referenced point"
  (interactive
   (list (helm-gtags2--read-tagname 'rtag (which-function))))
  (helm-gtags2--common '(helm-source-gtags2-rtags) tag))

(defun helm-gtags2-find-symbol (tag)
  "Jump to the symbol location"
  (interactive
   (list (helm-gtags2--read-tagname 'symbol)))
  (helm-gtags2--common '(helm-source-gtags2-gsyms) tag))

(defun helm-gtags2-pop-stack ()
  "Jump to previous point on the context stack and pop it from stack."
  (interactive)
  (let* ((context-info (helm-gtags2--get-context-info))
         (context-stack (plist-get context-info :stack))
         (context (pop context-stack)))
    (helm-gtags2--put-context-stack helm-gtags2--tag-location -1 context-stack)
    (helm-gtags2--move-to-context context)))

(defun helm-gtags2-clear-stack ()
  "Clear current context stack."
  (interactive)
  (let ((tag-location (helm-gtags2--find-tag-directory)))
    (message "Clear '%s' context stack." tag-location)
    (remhash tag-location helm-gtags2--context-stack)))

(defun helm-gtags2-clear-all-stacks ()
  "Clear all context stacks."
  (interactive)
  (message "Clear all context statks.")
  (setq helm-gtags2--context-stack (make-hash-table :test 'equal)))

(defun helm-gtags2-resume ()
  "Resurrect previously invoked `helm-gtags2` command."
  (interactive)
  (unless (get-buffer helm-gtags2--buffer)
    (error "Error: helm-gtags2 buffer is not existed."))
  (helm-resume helm-gtags2--buffer))

(defun helm-gtags2-tag-continue ()
  "Jump to next candidate position"
  (interactive)
  (unless (get-buffer helm-gtags2--buffer)
    (error "Error: helm-gtags2 buffer is not existed."))
  (let ((next-candidate
         (with-current-buffer (get-buffer helm-gtags2--buffer)
           (forward-line 1)
           (when (eobp)
             (error "No more candidate"))
           (buffer-substring-no-properties (point) (line-end-position)))))
    (helm-gtags2--action-openfile next-candidate)))

(defvar helm-gtags2-mode-name " HelmGtags2")
(defvar helm-gtags2-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-gtags2-mode
  "Enable helm-gtags2"
  :init-value nil
  :global     nil
  :keymap     helm-gtags2-mode-map
  :lighter    helm-gtags2-mode-name)

(provide 'helm-gtags2)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags2.el ends here
