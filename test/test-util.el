;;; test-util.el --- Test utilities of helm-gtags2

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'helm-gtags2)

(defmacro with-gtags-project (dirname &rest body)
  "Create temporary gtags project, executes body, remove temporary project."
  (declare (indent 1) (debug t))
  (let ((orig-dir (cl-gensym)))
    `(let ((,orig-dir default-directory))
       (make-directory ,dirname)
       (with-temp-file (concat ,dirname "test.c")
         (insert "int func(void) {}\n"))
       (let ((default-directory ,dirname))
         (unwind-protect
             (progn
               (process-file "gtags")
               ,@body)
           (let ((default-directory ,orig-dir))
             (delete-directory ,dirname t nil)))))))

(defsubst dummy-directory ()
  (file-name-as-directory (concat (expand-file-name default-directory) "dummy")))

(ert-deftest helm-gtags2--path-libpath-p ()
  "Test utility `helm-gtags2--path-libpath-p'"
  (let ((process-environment '("GTAGSLIBPATH=/foo:/bar:/baz")))
    (should (helm-gtags2--path-libpath-p "/foo/"))))

(ert-deftest helm-gtags2--tag-directory ()
  "Test utility `helm-gtags2--tag-directory'"
  (let ((dummy (dummy-directory)))
    (with-gtags-project dummy
      (should (string= (helm-gtags2--tag-directory) dummy)))))

(ert-deftest helm-gtags2--find-tag-directory ()
  "Test utility `helm-gtags2--find-tag-directory'"
  (let ((dummy (dummy-directory)))
    (with-gtags-project (dummy-directory)
      (let ((got (helm-gtags2--find-tag-directory)))
        (should (string= (helm-gtags2--find-tag-directory) dummy))
        (should (string= helm-gtags2--tag-location dummy))))))

(ert-deftest helm-gtags2--find-tag-directory-in-libpath ()
  "Test utility `helm-gtags2--find-tag-directory' in library path"
  (let ((dummy (dummy-directory)))
    (with-gtags-project (dummy-directory)
      (let* ((process-environment (list (concat "GTAGSLIBPATH=" dummy)))
             (helm-gtags2--tag-location "/tmp/")
             (got (helm-gtags2--find-tag-directory)))
        (should (string= (helm-gtags2--find-tag-directory) "/tmp/"))
        (should (string= helm-gtags2--real-tag-location dummy))))))

(ert-deftest helm-gtags2--construct-options ()
  "Test utility `helm-gtags2--construct-options'"
  (let ((got (helm-gtags2--construct-options 'find-file t)))
    (should (equal got '("-Poa" "-c"))))

  (let ((got (helm-gtags2--construct-options 'tag nil)))
    (should (equal got '("--result=grep"))))

  (let* ((current-prefix-arg t)
         (process-environment (list "GTAGSLIBPATH=foo:bar" ))
         (got (helm-gtags2--construct-options 'symbol t)))
    (should (equal got '("-T" "-l" "-s" "-c" "--result=grep")))))

(ert-deftest helm-gtags2--construct-options-force-abs-option ()
  "Test utility `helm-gtags2--construct-options' for special case of Windows system"

  (let* ((system-type 'windows-nt)
         (process-environment (list "GTAGSLIBPATH=foo" ))
         (helm-gtags2-path-style 'relative)
         (got (helm-gtags2--construct-options 'tag t)))
    (should (member "-a" got)))

  (let* ((system-type 'gnu/linux)
         (process-environment (list "GTAGSLIBPATH=foo" ))
         (helm-gtags2-path-style 'root)
         (got (helm-gtags2--construct-options 'tag t)))
    (should-not (member "-a" got))))

(ert-deftest helm-gtags2--extract-file-and-line ()
  "Test utility `helm-gtags2--extract-file-and-line'"
  (let ((input "C:/Program Files/Microsoft SDKs/Windows/v7.1/Include/Fci.h:44:typedef unsigned int UINT; /* ui */"))
    (let* ((system-type 'windows-nt)
           (got (helm-gtags2--extract-file-and-line input))
           (file (car got))
           (line (cdr got)))
      (should (string= file "C:/Program Files/Microsoft SDKs/Windows/v7.1/Include/Fci.h"))
      (should (= line 44)))

    ;; https://github.com/syohex/emacs-helm-gtags2/issues/80
    (let* ((system-type 'windows-nt)
           (got (helm-gtags2--extract-file-and-line "../include/stdio.h:30:#define hoge 1")))
      (should got)
      (let ((file (car got))
            (line (cdr got)))
       (should (string= file "../include/stdio.h"))
       (should (= line 30))))

    (let* ((system-type 'gnu/linux)
           (got (helm-gtags2--extract-file-and-line "/usr/include/stdio.h:30:#define hoge 1"))
           (file (car got))
           (line (cdr got)))
      (should (string= file "/usr/include/stdio.h"))
      (should (= line 30)))))

(ert-deftest helm-gtags2--transformer-regexp ()
  "Test utility `helm-gtags2--transformer-regexp'"
  (let ((input "C:/Program Files/Microsoft SDKs/Windows/v7.1/Include/Fci.h:44:typedef unsigned int UINT; /* ui */"))
    (let* ((system-type 'windows-nt)
           (regexp (helm-gtags2--transformer-regexp input)))
      (should (string-match regexp input))
      (should (string= (match-string-no-properties 1 input)
                       "C:/Program Files/Microsoft SDKs/Windows/v7.1/Include/Fci.h"))
      (should (string= (match-string-no-properties 2 input) "44")))

    (let* ((system-type 'gnu/linux)
           (input "/usr/include/stdio.h:30:#define hoge 1")
           (regexp (helm-gtags2--transformer-regexp input)))
      (should (string-match regexp input))
      (should (string= (match-string-no-properties 1 input) "/usr/include/stdio.h"))
      (should (string= (match-string-no-properties 2 input) "30")))))

(ert-deftest helm-gtags2--label-option ()
  "Test utility `helm-gtags2--label-option'"
  (let ((option (helm-gtags2--label-option "ctags")))
    (should (string= option "--gtagslabel=ctags"))))
