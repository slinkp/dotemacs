
;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; Dynamic timestamps of the form `Time-stamp: <>` or `Time-stamp: ""`
;; as per https://www.gnu.org/software/emacs/manual/html_node/emacs/Time-Stamps.html

(add-hook 'before-save-hook 'time-stamp)

;; wgrep allows editing & search/replace directly in grep results and then saving
(require 'wgrep)

;; Better defaults for file opening, eg C-x C-f defaults to file or URL at point.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html
(ffap-bindings)

(fset 'yes-or-no-p 'y-or-n-p) ; stop forcing me to spell out "yes"

;; been killing by mistake a lot lately.
(setq confirm-kill-emacs 'yes-or-no-p)


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


;;============================================================================
;; "Here's a pretty comprehensive group of magic invocations to make Emacs use
;; UTF-8 everywhere by default"
;; http://stackoverflow.com/questions/2901541/x/2903256#2903256
;;============================================================================
(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;============================================================================
;; Remember where we were.
;;============================================================================

(when we-have-gui?
  ;; Remember state of everything... if we're in a GUI.
  ;; Otherwise I probably don't want that much.
  (desktop-save-mode 1))

;; Save point position between sessions.
;; http://whattheemacsd.com/init.el-03.html
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; ======================================================================
;; Path
;; ======================================================================

;; fix up exec-path and $PATH used for subprocesses
;; TODO: does "exec-path-from-shell" installed above affect this?
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/bin/py"))
(add-to-list 'exec-path (expand-file-name "~/sh"))
(add-to-list 'exec-path "/nix/var/nix/gcroots/dev-profiles/user-extra-profile/bin/")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/opt/homebrew/sbin")
(setenv "PATH" (mapconcat 'identity exec-path path-separator))

;; ======================================================================
;; Setting goal-column makes behavior more intuitive when moving up/down
;; after deleting text, see:
;; https://irreal.org/blog/?p=266

(put 'set-goal-column 'disabled nil)

;; ======================================================================
;; emacsclient

(require 'server)
(unless (server-running-p)
  (server-start))
