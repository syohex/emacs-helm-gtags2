# helm-gtags2.el ![](https://github.com/syohex/emacs-helm-gtags2/workflows/CI/badge.svg)

## Introduction
`helm-gtags2.el` is GNU GLOBAL helm interface.


## Screenshot

![helm-gtags2](image/helm-gtags.png)


## Use Ctags with helm-gtags2

You can use `helm-gtags2` for languages which are supported by `ctags`(Both [exuberant ctags](http://ctags.sourceforge.net/) and [universal ctags](https://ctags.io/))
with ctags backend feature of `GNU global`. You can generate `ctags` backend
tags by following command.

```
# exuberant ctags
% gtags --gtagslabel=ctags

# universal ctags
% gtags --gtagslabel=new-ctags
```


## Basic Usage

#### `helm-gtags2-mode`

Enable `helm-gtags2-mode`.

#### `helm-gtags2-find-tag`

Input tag name and move to the definition.

#### `helm-gtags2-find-tag-from-here`

Find tag from here and move to its definition.

#### `helm-gtags2-find-rtag`

Input tag name and move to the referenced point.

#### `helm-gtags2-find-symbol`

Input symbol and move to the locations.

#### `helm-gtags2-pop-stack`

Move to previous point on the stack.
helm-gtags2 pushes current point to stack before executing each jump functions.

#### `helm-gtags2-next-history`

Move to next history on context stack.

#### `helm-gtags2-previous-history`

Move to previous history on context stack.

#### `helm-gtags2-clear-stack`

Clear current context stack.

#### `helm-gtags2-resume`

Resurrect previously invoked `helm-gtags2` command.
This is similar to `helm-resume` however this command resurrects helm gtags
buffer if other helm commands are called.

#### `helm-gtags2-tag-continue`

Jump to next candidate

## Customize Variables

#### `helm-gtags2-highlight-candidate`(Default `t`)

Highlighting candidates if this value is `non-nil`

#### `helm-gtags2-display-style`(Default `nil`)

Show detail information if this value is `'detail`,
show reference point of function etc.

#### `helm-gtags2-pulse-at-cursor`(Default `nil`)

If this variable is non-nil, pulse at point after jumping

#### `helm-gtags2-maximum-candidates`

Maximum number of helm candidates in `helm-gtags2.el`.
Please set small number if you feel slow for large source tree
such as Linux kernel.

Default value is 9999

## Faces

#### `helm-gtags2-file`

Face of file name of candidates

#### `helm-gtags2-lineno`

Face of line number of candidates


## anything-gtags.el

**helm-gtags2.el** is not compatible **anything-gtags.el**.
But `helm-gtags2.el` is designed for faster search than `anything-gtags.el`.

`anything-gtags.el` is slow in large source tree such as Linux kernel,
FreeBSD, Android etc. Because `anything-gtags.el` creates candidates
by processing output of `gtags.el`. `helm-gtags2.el` creates candidates
by itself, so `helm-gtags2.el` is faster than `anything-gtags.el`.


## Sample Configuration

```lisp
;;; Enable helm-gtags2-mode
(add-hook 'c-mode-hook #'helm-gtags2-mode)
(add-hook 'c++-mode-hook #'helm-gtags2-mode)
(add-hook 'asm-mode-hook #'helm-gtags2-mode)

;; customize
(custom-set-variables
 '(helm-gtags2-ignore-case t))

;; key bindings
(with-eval-after-load 'helm-gtags2
  (define-key helm-gtags2-mode-map (kbd "M-t") 'helm-gtags2-find-tag)
  (define-key helm-gtags2-mode-map (kbd "M-r") 'helm-gtags2-find-rtag)
  (define-key helm-gtags2-mode-map (kbd "M-s") 'helm-gtags2-find-symbol)
  (define-key helm-gtags2-mode-map (kbd "C-c <") 'helm-gtags2-previous-history)
  (define-key helm-gtags2-mode-map (kbd "C-c >") 'helm-gtags2-next-history)
  (define-key helm-gtags2-mode-map (kbd "M-,") 'helm-gtags2-pop-stack))
```
