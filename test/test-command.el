;;; test-command.el --- test commands of helm-gtags2

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

(ert-deftest helm-gtags2-clear-all-cache ()
  "Clear all caches"
  (let ((helm-gtags2--result-cache (make-hash-table :test 'equal)))
    (puthash "foo" 'foo helm-gtags2--result-cache)
    (puthash "bar" 'bar helm-gtags2--result-cache)
    (call-interactively 'helm-gtags2-clear-all-cache)
    (should (= (hash-table-count helm-gtags2--result-cache) 0))))

(ert-deftest helm-gtags2-clear-cache ()
  "Clear caches"
  (cl-letf (((symbol-function 'helm-gtags2--find-tag-directory)
             'ignore))
    (let ((helm-gtags2--result-cache (make-hash-table :test 'equal))
          (helm-gtags2--real-tag-location "foo/"))
      (puthash "foo/GTAGS" 'foo1 helm-gtags2--result-cache)
      (puthash "foo/GPATH" 'foo2 helm-gtags2--result-cache)
      (puthash "bar" 'bar helm-gtags2--result-cache)
      (call-interactively 'helm-gtags2-clear-cache)
      (should (= (hash-table-count helm-gtags2--result-cache) 1))
      (should (eq (gethash "bar" helm-gtags2--result-cache) 'bar)))))

(ert-deftest helm-gtags2-clear-stack ()
  "Clear current stack"
  (cl-letf (((symbol-function 'helm-gtags2--find-tag-directory)
             (lambda () "foo")))
    (let ((helm-gtags2--context-stack (make-hash-table :test 'equal)))
      (puthash "foo" 'foo helm-gtags2--context-stack)
      (puthash "bar" 'bar helm-gtags2--context-stack)
      (call-interactively 'helm-gtags2-clear-stack)
      (should (eq (gethash "foo"  helm-gtags2--context-stack 'deleted) 'deleted))
      (should (= (hash-table-count helm-gtags2--context-stack) 1)))))

(ert-deftest helm-gtags2-clear-all-stacks ()
  "Clear current stack"
  (let ((helm-gtags2--context-stack (make-hash-table :test 'equal)))
    (puthash "foo" 'foo helm-gtags2--context-stack)
    (puthash "bar" 'bar helm-gtags2--context-stack)
    (call-interactively 'helm-gtags2-clear-all-stacks)
    (should (= (hash-table-count helm-gtags2--context-stack) 0))))

;;; test-command.el ends here
