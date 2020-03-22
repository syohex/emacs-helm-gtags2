;;; helm-gtags2.el --- GNU GLOBAL helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags2
;; Version: 1.5.6
;; Package-Requires: ((emacs "26.3") (helm "3.6.0"))

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

(defcustom helm-gtags2-path-style 'root
  "Style of file path"
  :type '(choice (const :tag "Root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute Path" absolute)))

(defcustom helm-gtags2-ignore-case nil
  "Ignore case in each search."
  :type 'boolean)

(defcustom helm-gtags2-read-only nil
  "Gtags read only mode."
  :type 'boolean)

(defcustom helm-gtags2-auto-update nil
  "*If non-nil, tag files are updated whenever a file is saved."
  :type 'boolean)

(defcustom helm-gtags2-pulse-at-cursor t
  "If non-nil, pulse at point after jumping"
  :type 'boolean)

(defcustom helm-gtags2-cache-select-result nil
  "*If non-nil, results of helm-gtags2-select and helm-gtags2-select-path are cached."
  :type 'boolean)

(defcustom helm-gtags2-cache-max-result-size (* 10 1024 1024) ;10M
  "Max size(bytes) to cache for each select result."
  :type 'integer)

(defcustom helm-gtags2-update-interval-second 60
  "Tags are updated in `after-save-hook' if this seconds is passed from last update.
Always update if value of this variable is nil."
  :type '(choice (integer :tag "Update interval seconds")
                 (boolean :tag "Update every time" nil)))

(defcustom helm-gtags2-highlight-candidate t
  "Highlight candidate or not"
  :type 'boolean)

(defcustom helm-gtags2-use-input-at-cursor nil
  "Use input at cursor"
  :type 'boolean)

(defcustom helm-gtags2-prefix-key "\C-c"
  "If non-nil, it is used for the prefix key of gtags-xxx command."
  :type 'string)

(defcustom helm-gtags2-preselect nil
  "If non-nil, preselect current file and line."
  :type 'boolean)

(defcustom helm-gtags2-display-style nil
  "Style of display result."
  :type '(choice (const :tag "Show in detail" detail)
                 (const :tag "Normal style" nil)))

(defcustom helm-gtags2-fuzzy-match nil
  "Enable fuzzy match"
  :type 'boolean)

(defcustom helm-gtags2-maximum-candidates (if helm-gtags2-fuzzy-match 100 9999)
  "Maximum number of helm candidates"
  :type 'integer)

(defcustom helm-gtags2-direct-helm-completing nil
  "Use helm mode directly."
  :type 'boolean)

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

(defmacro helm-declare-obsolete-variable (old new version)
  `(progn
     (defvaralias ,old ,new)
     (make-obsolete-variable ,old ,new ,version)))

(helm-declare-obsolete-variable
 'helm-c-gtags-path-style 'helm-gtags2-path-style "0.8")
(helm-declare-obsolete-variable
 'helm-c-gtags-ignore-case 'helm-gtags2-ignore-case  "0.8")
(helm-declare-obsolete-variable
 'helm-c-gtags-read-only 'helm-gtags2-read-only "0.8")

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
    (when (or (eq helm-gtags2-path-style 'absolute)
              (helm-gtags2--use-abs-path-p gtagslibpath))
      (push "-a" options))
    (when helm-gtags2-ignore-case
      (push "-i" options))
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
    (if (and tagname helm-gtags2-use-input-at-cursor)
        tagname
      (when (and (not tagname) default-tagname)
        (setq tagname default-tagname))
      (when tagname
        (setq prompt (format "%s(default \"%s\") " prompt tagname)))
      (let ((completion-ignore-case helm-gtags2-ignore-case)
            (completing-read-function 'completing-read-default))
        (if (and helm-gtags2-direct-helm-completing (memq type '(tag rtag symbol find-file)))
            (helm-comp-read prompt comp-func
                            :history 'helm-gtags2--completing-history
                            :exec-when-only-one t
                            :default tagname)
          (completing-read prompt comp-func nil nil nil
                           'helm-gtags2--completing-history tagname))))))

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
                 (cl-case helm-gtags2-path-style
                   (root (or helm-gtags2--real-tag-location
                             helm-gtags2--tag-location))
                   (otherwise default-directory))))
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

(defun helm-gtags2--open-file (file readonly)
  (if readonly
      (find-file-read-only file)
    (find-file file)))

(defun helm-gtags2--open-file-other-window (file readonly)
  (setq helm-gtags2--use-otherwin nil)
  (if readonly
      (find-file-read-only-other-window file)
    (find-file-other-window file)))

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

;;;###autoload
(defun helm-gtags2-clear-all-cache ()
  (interactive)
  (clrhash helm-gtags2--result-cache))

;;;###autoload
(defun helm-gtags2-clear-cache ()
  (interactive)
  (helm-gtags2--find-tag-directory)
  (let* ((tag-location (or helm-gtags2--real-tag-location
                           helm-gtags2--tag-location))
         (gtags-path (concat tag-location "GTAGS"))
         (gpath-path (concat tag-location "GPATH")))
    (remhash gtags-path helm-gtags2--result-cache)
    (remhash gpath-path helm-gtags2--result-cache)))

(defun helm-gtags2--move-to-context (context)
  (let ((file (plist-get context :file))
        (curpoint (plist-get context :position))
        (readonly (plist-get context :readonly)))
    (helm-gtags2--open-file file readonly)
    (goto-char curpoint)
    (recenter)))

;;;###autoload
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

;;;###autoload
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

(defun helm-gtags2--get-result-cache (file)
  (helm-gtags2--find-tag-directory)
  (let* ((file-path (concat (or helm-gtags2--real-tag-location
                                helm-gtags2--tag-location)
                            file))
         (file-mtime (nth 5 (file-attributes file-path)))
         (hash-value (gethash file-path helm-gtags2--result-cache))
         (cached-file-mtime (nth 0 hash-value)))
    (if (and cached-file-mtime (equal cached-file-mtime file-mtime))
        (nth 1 hash-value)
      nil)))

(defun helm-gtags2--put-result-cache (file cache)
  (helm-gtags2--find-tag-directory)
  (let* ((file-path (concat (or helm-gtags2--real-tag-location
                                helm-gtags2--tag-location)
                            file))
         (file-mtime (nth 5 (file-attributes file-path)))
         (hash-value (list file-mtime cache)))
    (puthash file-path hash-value helm-gtags2--result-cache)))

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

(defun helm-gtags2--show-detail ()
  (goto-char (point-min))
  (while (not (eobp))
    (let ((line (helm-current-line-contents)))
      (let* ((file-and-line (helm-gtags2--extract-file-and-line line))
             (file (car file-and-line))
             (ref-line (cdr file-and-line))
             (ref-func (helm-gtags2--referer-function file ref-line)))
        (when ref-func
          (search-forward ":" nil nil 2)
          (insert " " ref-func "|"))
        (forward-line 1)))))

(defun helm-gtags2--print-path-in-gtagslibpath (args)
  (let ((libpath (getenv "GTAGSLIBPATH")))
    (when libpath
      (dolist (path (parse-colon-path libpath))
        (let ((default-directory (file-name-as-directory path)))
          (apply #'process-file "global" nil t nil "-Poa" args))))))

(defun helm-gtags2--exec-global-command (type input &optional detail)
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
          (helm-gtags2--remove-carrige-returns)
          (when detail
            (helm-gtags2--show-detail)))))))

(defun helm-gtags2--construct-command (type &optional in)
  (setq helm-gtags2--local-directory nil)
  (let ((dir (helm-attr 'helm-gtags2-base-directory (helm-get-current-source))))
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
  (helm-gtags2--exec-global-command 'pattern input helm-gtags2-display-style))

(defun helm-gtags2--rtags-init (&optional input)
  (helm-gtags2--exec-global-command 'rtag input helm-gtags2-display-style))

(defun helm-gtags2--gsyms-init ()
  (helm-gtags2--exec-global-command 'symbol nil helm-gtags2-display-style))

(defun helm-gtags2--files-init ()
  (helm-gtags2--exec-global-command 'find-file nil))

(defun helm-gtags2--real-file-name ()
  (let ((buffile (buffer-file-name)))
    (unless buffile
      (error "This buffer is not related to file."))
    (if (file-remote-p buffile)
        (tramp-file-name-localname (tramp-dissect-file-name buffile))
      (file-truename buffile))))

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

(defsubst helm-gtags2--select-find-file-func ()
  (if helm-gtags2--use-otherwin
      #'helm-gtags2--open-file-other-window
    #'helm-gtags2--open-file))

(defun helm-gtags2--do-open-file (open-func file line)
  (funcall open-func file helm-gtags2-read-only)
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

(defun helm-gtags2--action-openfile (cand)
  (let* ((file-and-line (helm-gtags2--extract-file-and-line cand))
         (filename (car file-and-line))
         (line (cdr file-and-line))
         (open-func (helm-gtags2--select-find-file-func))
         (default-directory (helm-gtags2--base-directory)))
    (helm-gtags2--do-open-file open-func filename line)))

(defun helm-gtags2--action-openfile-other-window (cand)
  (let ((helm-gtags2--use-otherwin t))
    (helm-gtags2--action-openfile cand)))

(defun helm-gtags2--file-content-at-pos (file pos)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (format "%s:%d:%s:%s"
              file (line-number-at-pos)
              (helm-aif (which-function) (format "[%s]" it) "")
              (helm-current-line-contents)))))

(defun helm-gtags2--files-candidate-transformer (file)
  (if (eq helm-gtags2-path-style 'absolute)
      file
    (let ((removed-regexp (concat "\\`" helm-gtags2--tag-location)))
      (replace-regexp-in-string removed-regexp "" file))))

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
    (when (eq helm-gtags2-path-style 'relative)
      (setq helm-gtags2--last-default-directory default-directory))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))

(defvar helm-gtags2--find-file-action
  (helm-make-actions
   "Open file" #'helm-gtags2--action-openfile
   "Open file other window" #'helm-gtags2--action-openfile-other-window))

(defvar helm-source-gtags-tags
  (helm-build-in-buffer-source "Jump to definitions"
    :init 'helm-gtags2--tags-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :real-to-display 'helm-gtags2--candidate-transformer
    :persistent-action 'helm-gtags2--persistent-action
    :fuzzy-match helm-gtags2-fuzzy-match
    :action helm-gtags2--find-file-action))

(defvar helm-source-gtags-pattern
  (helm-build-in-buffer-source "Find pattern"
    :init 'helm-gtags2--pattern-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :real-to-display 'helm-gtags2--candidate-transformer
    :persistent-action 'helm-gtags2--persistent-action
    :fuzzy-match helm-gtags2-fuzzy-match
    :action helm-gtags2--find-file-action))

(defvar helm-source-gtags-rtags
  (helm-build-in-buffer-source "Jump to references"
    :init 'helm-gtags2--rtags-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :real-to-display 'helm-gtags2--candidate-transformer
    :persistent-action 'helm-gtags2--persistent-action
    :fuzzy-match helm-gtags2-fuzzy-match
    :action helm-gtags2--find-file-action))

(defvar helm-source-gtags-gsyms
  (helm-build-in-buffer-source "Jump to symbols"
    :init 'helm-gtags2--gsyms-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :real-to-display 'helm-gtags2--candidate-transformer
    :persistent-action 'helm-gtags2--persistent-action
    :fuzzy-match helm-gtags2-fuzzy-match
    :action helm-gtags2--find-file-action))

(defun helm-gtags2--highlight-candidate (candidate)
  (let ((regexp (concat "\\_<" helm-gtags2--last-input "\\_>"))
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

(defvar helm-source-gtags-find-tag-from-here
  (helm-build-in-buffer-source "Find tag from here"
    :init 'helm-gtags2--find-tag-from-here-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :real-to-display 'helm-gtags2--candidate-transformer
    :persistent-action 'helm-gtags2--persistent-action
    :fuzzy-match helm-gtags2-fuzzy-match
    :action helm-gtags2--find-file-action))

(defun helm-gtags2--show-stack-action (cand)
  (let* ((index (get-text-property 0 'index cand))
         (context-info (helm-gtags2--get-context-info))
         (context-stack (plist-get context-info :stack)))
    (helm-gtags2--put-context-stack helm-gtags2--tag-location
                                    index context-stack)
    (helm-gtags2--move-to-context (nth index context-stack))))

(defvar helm-source-gtags-show-stack
  (helm-build-sync-source "Show Context Stack"
    :candidates 'helm-gtags2--show-stack-init
    :volatile t
    :candidate-number-limit helm-gtags2-maximum-candidates
    :persistent-action 'helm-gtags2--persistent-action
    :fuzzy-match helm-gtags2-fuzzy-match
    :action 'helm-gtags2--show-stack-action))

;;;###autoload
(defun helm-gtags2-select ()
  (interactive)
  (helm-gtags2--common '(helm-source-gtags-select) nil))

(defsubst helm-gtags2--beginning-of-defun ()
  (cl-case major-mode
    ((c-mode c++-mode java-mode) 'c-beginning-of-defun)
    (php-mode 'php-beginning-of-defun)
    (otherwise #'beginning-of-defun)))

(defsubst helm-gtags2--end-of-defun ()
  (cl-case major-mode
    ((c-mode c++-mode java-mode malabar-mode) 'c-end-of-defun)
    (php-mode 'php-end-of-defun)
    (otherwise #'end-of-defun)))

(defun helm-gtags2--current-funcion-bound ()
  (save-excursion
    (let (start)
      (funcall (helm-gtags2--beginning-of-defun))
      (setq start (line-number-at-pos))
      (funcall (helm-gtags2--end-of-defun))
      (cons start (line-number-at-pos)))))

(defun helm-gtags2--tags-refered-from-this-function ()
  (let* ((file (helm-gtags2--real-file-name))
         (bound (helm-gtags2--current-funcion-bound))
         (start-line (car bound))
         (end-line (cdr bound)))
    (with-temp-buffer
      (unless (process-file "global" nil t nil "-f" "-r" file)
        (error "Failed: global -f -r %s" file))
      (goto-char (point-min))
      (let (tagnames finish)
        (while (and (not finish) (not (eobp)))
          (let* ((cols (split-string (helm-current-line-contents) nil t))
                 (lineno (string-to-number (cl-second cols))))
            (if (and (> lineno start-line) (< lineno end-line))
                (let* ((tag (cl-first cols))
                       (elm (cl-find tag tagnames :test 'equal)))
                  (unless elm
                    (push tag tagnames)))
              (when (>= lineno end-line)
                (setq finish t)))
            (forward-line 1)))
        (reverse tagnames)))))

(defun helm-gtags2--tag-in-function-persistent-action (cand)
  (let* ((bound (helm-gtags2--current-funcion-bound))
         (limit (save-excursion
                  (goto-char (point-min))
                  (forward-line (cdr bound))
                  (goto-char (line-end-position))
                  (point))))
    (when (search-forward cand nil limit)
      (helm-highlight-current-line))))

;;;###autoload
(defun helm-gtags2-tags-in-this-function ()
  "Show tagnames which are referenced in this function and jump to it."
  (interactive)
  (let ((tags (helm-gtags2--tags-refered-from-this-function)))
    (unless tags
      (error "There are no tags which are refered from this function."))
    (let* ((name (format "Tags in [%s]" (which-function)))
           (tag (helm-comp-read
                 "Tagnames: " tags
                 :must-match t :name name
                 :persistent-action 'helm-gtags2--tag-in-function-persistent-action)))
      (helm-gtags2-find-tag tag))))

(defun helm-gtags2--source-select-tag (candidate)
  (helm-build-in-buffer-source "Select Tag"
    :init (lambda () (helm-gtags2--tags-init candidate))
    :candidate-number-limit helm-gtags2-maximum-candidates
    :persistent-action 'helm-gtags2--persistent-action
    :fuzzy-match helm-gtags2-fuzzy-match
    :action helm-gtags2--find-file-action))

(defun helm-gtags2--source-select-rtag (candidate)
  (helm-build-in-buffer-source "Select Rtag"
    :init (lambda () (helm-gtags2--rtags-init candidate))
    :candidate-number-limit helm-gtags2-maximum-candidates
    :persistent-action 'helm-gtags2--persistent-action
    :fuzzy-match helm-gtags2-fuzzy-match
    :action helm-gtags2--find-file-action))

(defsubst helm-gtags2--action-by-timer (src)
  (run-with-timer 0.1 nil (lambda () (helm-gtags2--common (list src) nil))))

(defun helm-gtags2--select-tag-action (c)
  (helm-gtags2--action-by-timer (helm-gtags2--source-select-tag c)))

(defun helm-gtags2--select-rtag-action (c)
  (helm-gtags2--action-by-timer (helm-gtags2--source-select-rtag c)))

(defun helm-gtags2--select-cache-init-common (args tagfile)
  (let ((cache (helm-gtags2--get-result-cache tagfile)))
    (if cache
        (insert cache)
      (apply #'process-file "global" nil t nil args)
      (let* ((cache (buffer-string))
             (cache-size (length cache)))
        (when (<= cache-size helm-gtags2-cache-max-result-size)
          (helm-gtags2--put-result-cache tagfile cache))))))

(defun helm-gtags2--source-select-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (if (not helm-gtags2-cache-select-result)
        (progn
          (process-file "global" nil t nil "-c")
          (helm-gtags2--remove-carrige-returns))
      (helm-gtags2--select-cache-init-common '("-c") "GTAGS"))))

(defvar helm-source-gtags-select
  (helm-build-in-buffer-source "Find tag from here"
    :init 'helm-gtags2--source-select-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :persistent-action #'ignore
    :fuzzy-match helm-gtags2-fuzzy-match
    :action (helm-make-actions
             "Goto the location" #'helm-gtags2--select-tag-action
             "Goto the location(other buffer)"
             (lambda (c)
               (setq helm-gtags2--use-otherwin t)
               (helm-gtags2--select-tag-action c))
             "Move to the referenced point" #'helm-gtags2--select-rtag-action)))

(defun helm-gtags2--file-name (name)
  (let ((remote (file-remote-p default-directory)))
    (if (not remote)
        name
      (cl-case helm-gtags2-path-style
        (relative name)
        (otherwise (concat remote name))))))

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

;;;###autoload
(defun helm-gtags2-create-tags (dir label)
  (interactive
   (list (read-directory-name "Root Directory: ")
         (helm-gtags2--read-gtagslabel)))
  (let ((default-directory dir)
        (proc-buf (get-buffer-create " *helm-gtags2-create*")))
    (let ((proc (start-file-process "helm-gtags2-create" proc-buf
                                    "gtags" "-q" (helm-gtags2--label-option label))))
      (set-process-sentinel proc (helm-gtags2--make-gtags-sentinel 'create)))))

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
         (path (cl-case helm-gtags2-path-style
                 (absolute buffile)
                 (root
                  (file-relative-name buffile (helm-gtags2--find-tag-directory)))
                 (relative
                  (file-relative-name buffile (helm-gtags2--base-directory))))))
    (format "%s:%d" path (line-number-at-pos))))

(defsubst helm-gtags2--clear-variables ()
  (setq helm-gtags2--last-default-directory nil))

(defun helm-gtags2--common (srcs tagname)
  (helm-gtags2--clear-variables)
  (let ((helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (dir (helm-gtags2--searched-directory))
        (src (car srcs))
        (preselect-regexp (when helm-gtags2-preselect
                            (regexp-quote (helm-gtags2--current-file-and-line)))))
    (when (symbolp src)
      (setq src (symbol-value src)))
    (unless helm-gtags2--use-otherwin
      (setq helm-gtags2--use-otherwin (helm-gtags2--using-other-window-p)))
    (when tagname
      (setq helm-gtags2--query tagname))
    (let ((tagroot (helm-gtags2--find-tag-simple)))
      (helm-attrset 'helm-gtags2-base-directory dir src)
      (when tagname
        (helm-attrset 'name (format "%s in %s" tagname (or dir tagroot)) src))
      (helm :sources srcs :buffer helm-gtags2--buffer
            :preselect preselect-regexp))))

;;;###autoload
(defun helm-gtags2-delete-tags ()
  "Delete file GTAGS, GRTAGS, GPATH, ID etc. generated by gtags."
  (interactive)
  (let* ((root-dir (helm-gtags2--tag-directory))
         (re (concat "\\`" (regexp-opt '("GPATH" "GRTAGS" "GTAGS" "ID")) "\\'"))
         (files (cl-remove-if-not
                 (lambda (file)
                   ;; Don't trust `directory-files'.
                   (let ((case-fold-search nil))
                     (string-match-p re (file-name-nondirectory file))))
                 (directory-files root-dir t re)))
         (buffer "*GTags File List*"))
    (unless files
      (user-error "No tag files found"))
    (with-output-to-temp-buffer buffer
      (princ (mapconcat #'identity files "\n")))
    (let ((win (get-buffer-window buffer)))
      (unwind-protect
          (progn
            (fit-window-to-buffer win)
            (when (yes-or-no-p "Remove GNU Global tag files? ")
              (with-demoted-errors (mapc #'delete-file files))
              ))
        (when (window-live-p win)
          (quit-window t win))))))

;;;###autoload
(defun helm-gtags2-find-tag (tag)
  "Jump to definition"
  (interactive
   (list (helm-gtags2--read-tagname 'tag)))
  (helm-gtags2--common '(helm-source-gtags-tags) tag))

;;;###autoload
(defun helm-gtags2-find-tag-other-window (tag)
  "Jump to definition in other window."
  (interactive
   (list (helm-gtags2--read-tagname 'tag)))
  (setq helm-gtags2--use-otherwin t)
  (helm-gtags2-find-tag tag))

;;;###autoload
(defun helm-gtags2-find-rtag (tag)
  "Jump to referenced point"
  (interactive
   (list (helm-gtags2--read-tagname 'rtag (which-function))))
  (helm-gtags2--common '(helm-source-gtags-rtags) tag))

;;;###autoload
(defun helm-gtags2-find-symbol (tag)
  "Jump to the symbol location"
  (interactive
   (list (helm-gtags2--read-tagname 'symbol)))
  (helm-gtags2--common '(helm-source-gtags-gsyms) tag))

;;;###autoload
(defun helm-gtags2-find-pattern (pattern)
  "Grep and jump by gtags tag files."
  (interactive
   (list (helm-gtags2--read-tagname 'pattern)))
  (helm-gtags2--common '(helm-source-gtags-pattern) pattern))

(defun helm-gtags2--find-file-after-hook ()
  (helm-gtags2--push-context helm-gtags2--saved-context))

(defvar helm-source-gtags-files
  (helm-build-in-buffer-source "Find files"
    :init #'helm-gtags2--files-init
    :candidate-number-limit helm-gtags2-maximum-candidates
    :real-to-display #'helm-gtags2--files-candidate-transformer
    :persistent-action #'helm-gtags2--file-persistent-action
    :fuzzy-match helm-gtags2-fuzzy-match
    :action helm-gtags2--file-util-action))

;;;###autoload
(defun helm-gtags2-find-files (file)
  "Find file from tagged with gnu global."
  (interactive
   (list (helm-gtags2--read-tagname 'find-file)))
  (add-hook 'helm-after-action-hook 'helm-gtags2--find-file-after-hook)
  (unwind-protect
      (helm-gtags2--common '(helm-source-gtags-files) file)
    (remove-hook 'helm-after-action-hook 'helm-gtags2--find-file-after-hook)))

;;;###autoload
(defun helm-gtags2-find-tag-from-here ()
  "Jump point by current point information.
Jump to definition point if cursor is on its reference.
Jump to reference point if curosr is on its definition"
  (interactive)
  (helm-gtags2--common '(helm-source-gtags-find-tag-from-here) nil))

(defun helm-gtags2--find-preselect-line ()
  (let ((defun-bound (bounds-of-thing-at-point 'defun)))
    (if (not defun-bound)
        (line-number-at-pos)
      (let ((defun-begin-line (line-number-at-pos (car defun-bound)))
            (filename (helm-gtags2--real-file-name)))
        (with-temp-buffer
          (unless (zerop (process-file "global" nil t nil "-f" filename))
            (error "Failed: global -f"))
          (goto-char (point-min))
          (let (start-line)
            (while (and (not start-line)
                        (re-search-forward "^\\S-+\\s-+\\([1-9][0-9]*\\)" nil t))
              (let ((line (string-to-number (match-string-no-properties 1))))
                (when (>= line defun-begin-line)
                  (setq start-line line))))
            (or start-line (line-number-at-pos))))))))

;;;###autoload
(defun helm-gtags2-pop-stack ()
  "Jump to previous point on the context stack and pop it from stack."
  (interactive)
  (let* ((context-info (helm-gtags2--get-context-info))
         (context-stack (plist-get context-info :stack))
         (context (pop context-stack)))
    (helm-gtags2--put-context-stack helm-gtags2--tag-location -1 context-stack)
    (helm-gtags2--move-to-context context)))

;;;###autoload
(defun helm-gtags2-show-stack ()
  "Show current context stack."
  (interactive)
  (helm-other-buffer 'helm-source-gtags-show-stack
                     (get-buffer-create helm-gtags2--buffer)))

;;;###autoload
(defun helm-gtags2-clear-stack ()
  "Clear current context stack."
  (interactive)
  (let ((tag-location (helm-gtags2--find-tag-directory)))
    (message "Clear '%s' context stack." tag-location)
    (remhash tag-location helm-gtags2--context-stack)))

;;;###autoload
(defun helm-gtags2-clear-all-stacks ()
  "Clear all context stacks."
  (interactive)
  (message "Clear all context statks.")
  (setq helm-gtags2--context-stack (make-hash-table :test 'equal)))

(defun helm-gtags2--read-tag-directory ()
  (let ((dir (read-directory-name "Directory tag generated: " nil nil t)))
    ;; On Windows, "gtags d:/tmp" work, but "gtags d:/tmp/" doesn't
    (directory-file-name (expand-file-name dir))))

(defsubst helm-gtags2--how-to-update-tags ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 'entire-update)
    (16 'generate-other-directory)
    (otherwise 'single-update)))

(defun helm-gtags2--update-tags-command (how-to)
  (cl-case how-to
    (entire-update '("global" "-u"))
    (generate-other-directory (list "gtags" (helm-gtags2--read-tag-directory)))
    (single-update (list "global" "--single-update" (helm-gtags2--real-file-name)))))

(defun helm-gtags2--update-tags-p (how-to interactive-p current-time)
  (or interactive-p
      (and (eq how-to 'single-update)
           (buffer-file-name)
           (or (not helm-gtags2-update-interval-second)
               (>= (- current-time helm-gtags2--last-update-time)
                   helm-gtags2-update-interval-second)))))

;;;###autoload
(defun helm-gtags2-update-tags ()
  "Update TAG file. Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'"
  (interactive)
  (let ((how-to (helm-gtags2--how-to-update-tags))
        (interactive-p (called-interactively-p 'interactive))
        (current-time (float-time (current-time))))
    (when (helm-gtags2--update-tags-p how-to interactive-p current-time)
      (let* ((cmds (helm-gtags2--update-tags-command how-to))
             (proc (apply #'start-file-process "helm-gtags2-update-tag" nil cmds)))
        (if (not proc)
            (message "Failed: %s" (string-join cmds " "))
          (set-process-sentinel proc (helm-gtags2--make-gtags-sentinel 'update))
          (setq helm-gtags2--last-update-time current-time))))))

;;;###autoload
(defun helm-gtags2-resume ()
  "Resurrect previously invoked `helm-gtags2` command."
  (interactive)
  (unless (get-buffer helm-gtags2--buffer)
    (error "Error: helm-gtags2 buffer is not existed."))
  (helm-resume helm-gtags2--buffer))

(defvar helm-gtags2-mode-name " HelmGtags2")
(defvar helm-gtags2-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-gtags2-mode ()
  "Enable helm-gtags2"
  :init-value nil
  :global     nil
  :keymap     helm-gtags2-mode-map
  :lighter    helm-gtags2-mode-name
  (if helm-gtags2-mode
      (when helm-gtags2-auto-update
        (add-hook 'after-save-hook 'helm-gtags2-update-tags nil t))
    (when helm-gtags2-auto-update
      (remove-hook 'after-save-hook 'helm-gtags2-update-tags t))))

(provide 'helm-gtags2)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags2.el ends here
