
;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; wgrep allows editing & search/replace directly in grep results and then saving
(require 'wgrep)

;; Better defaults for file opening, eg C-x C-f defaults to file or URL at point.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html
(ffap-bindings)

(fset 'yes-or-no-p 'y-or-n-p) ; stop forcing me to spell out "yes"

;; been killing by mistake a lot lately.
(setq confirm-kill-emacs 'yes-or-no-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)
;; ... except that breaks buffer-menu
;; ... this doesn't seem to help :(
(add-to-list 'global-auto-revert-ignore-modes 'buffer-menu-mode)

;; Always end a file with a newline, avoids confusing some tools.
(setq require-final-newline t)

;; Turn on auto-compression so we can read and write .gz files
(auto-compression-mode t)

;; Text mode by default.
(setq major-mode 'text-mode)

;; word wrap when in text mode.
(setq text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 79)
;; I like visual wrapping.
(setq-default truncate-lines nil)

;; I never want to upcase a whole region, so, disable this stupid command.
;; got very tired of doing it by accident.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; How to distinguish files with same name:
;; instead of adding a number, show part of directory path.
;; Options are 'forward, 'post-forward, and 'reverse.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-after-kill-buffer-p t
)

;; xref navigation via tags file
(setq tags-file-name "TAGS")
(setq tags-revert-without-query 1)

;; recent files
(load "recentf")
(recentf-mode 1)

;; Hate hate hate auto-vscrolling to center of screen.
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Don't prompt me to follow symlinks, just do it
(setq vc-follow-symlinks t)
