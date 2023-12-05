;; ========================================================================
;; MISC MODES
;; Language modes go elsewhere
;; ========================================================================

;; Whitespace mode is kinda cool, kinda awful...
;; really useful for those cases where the mode i'm using is wrong about
;; what indentation i want (grrr js2-mode).
;; Clean it up a lot as per http://xahlee.org/emacs/whitespace-mode.html
; Make whitespace-mode use just basic coloring.
(setq whitespace-style (quote
  ( spaces tabs newline space-mark tab-mark newline-mark)))
;; make whitespace-mode use “¶” for newline and “▷” for tab.
;; together with the rest of its defaults
(setq whitespace-display-mappings
 '(
   ;(space-mark 32 [183] [46]) ; normal space, ·
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [182 10]) ; newlne, ¶
   (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
))

;; ===========================================================
;; Helm
;; ===========================================================

(require 'helm)
(when (fboundp 'helm)
  ;; way cooler than default M-x
  (global-set-key (kbd "M-x") 'helm-M-x)
  ;; I never use the default M-y "yank-pop" command.
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  ;; Replace switch-to-buffer.
  (global-set-key (kbd "C-x b") 'helm-mini)

  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)

  ;; Performance tweak per #emacs on shopify slack. Try 40 if too slow?
  (setq helm-candidate-number-limit 60)

  ;; Searching commands vaguely on par with VSCode
  (global-set-key (kbd "M-P") 'helm-projectile)
  (global-set-key (kbd "M-F") 'helm-git-grep)
)

;;============================================================================
;; Diminish mode: Clean up modeline.
;;
;; Leaves more room for stuff we care about.
;;============================================================================
;; see http://whattheemacsd.com/init.el-04.html

;; XXX Is diminish this fucking up imenu?

;(diminish 'wrap-region-mode)
;(diminish 'yas/minor-mode)

;; (eval-after-load "flymake"
;;   '(diminish 'flymake-mode))

;; Argh you can't diminish major modes.
;; (eval-after-load "python"
;;   '(diminish 'python-mode "Py"))

(eval-after-load "multi-web"
  '(diminish 'multi-web-mode "mw"))

(eval-after-load "auto-complete-mode"
  '(diminish 'auto-complete-mode ""))

(eval-after-load "outline-mode"
  '(diminish 'outline-mode "out"))

;; This causes warnings about archaic use of hi-lock-mode. TBD, do i still use it?
;; (eval-after-load "hi-lock-mode"
;;   '(diminish 'hi-lock-mode ""))

;; No dice with any variant of this I tried; hacked into the pymacs loading func
;; (eval-after-load "ropemacs-mode"
;;   '(diminish 'ropemacs-mode " R"))

;;============================================================================
;; What function/class/method are we currently in?

;; Show it at top instead of modeline, as per https://www.emacswiki.org/emacs/WhichFuncMode
;; ... note this is not a straight copy/paste, there were some suggested edits,
;; had to read the whole text and do the suggestions.

(setq mode-line-misc-info (delete (assoc 'which-function-mode
                                      mode-line-misc-info) mode-line-misc-info)
      which-func-header-line-format '(which-function-mode ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-function-mode
    (setq mode-line-misc-info (delete (assoc 'which-function-mode
                                          mode-line-misc-info) mode-line-misc-info)
          header-line-format which-func-header-line-format
          )))


;; ============================================================================
;; AUTO REVERT
;;
;; This is handy for most things, but has a few things that need tweaking.
;; ============================================================================

;; Auto refresh buffers.
(global-auto-revert-mode 1)
;; Enable this to make it work for dired, but be quiet about it.
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Disable it for Buffer-menu, because refresh discards any changes in progress.
;; GOTCHA: It's spelled Buffer-menu-mode, case sensitive >:-(
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; ============================================================================
;; DIRED
;; Doing this last because the subtree stuff seems to break if I do it too early
;; but I'm not sure what it depends on.
;; ============================================================================

;; Dired: M-< and M-> should go to first and last file.
;; From http://whattheemacsd.com/setup-dired.el-02.html
(defun dired-back-to-top ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line (if dired-omit-mode 2 4)))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; ===========================================
;; Git / Github
;; ===========================================

(defun git-path ()
  "Find the path to the current file relative to repository root. Like git-link but just the relative path"
  (interactive)
  (require 'git-link)
  (setq filename    (git-link--relative-filename))
  (cond ((null filename)
          (message "Can't figure out what to link to"))
        (t
          (kill-new filename)
          (message filename))
  ))

;; Magit commit messages can get crazy slow esp after rebase. Don't need diff
(remove-hook 'server-switch-hook 'magit-commit-diff)

;; ==============================================
;; Projectile
;; ==============================================

;; Makes helm-projectile usable on giant repos. Do `C-c p i` or `projectile-invalidate-cache` to refresh
(setq projectile-enable-caching t)

;; similar to git-path but based on projectile rather than git repo
(defun projectile-path ()
  "Find the path to the current file relative to projectile root. Like git-path but for projects, not repos"
  (interactive)
  (setq filename (string-remove-prefix (projectile-project-root)
                        (file-truename (buffer-file-name))))
  (kill-new filename)
  (message filename))

;; Projectile speedup over tramp? UNTESTED
;; https://github.com/bbatsov/projectile/issues/1232#issuecomment-536281335
(defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

;; ================================================
;; Tramp
;; ================================================

;; Tramp speedups
(setq vc-handled-backends '(Git))

;; Avoid tramp hang per https://www.emacswiki.org/emacs/TrampMode#h5o-9
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; =====================================
;; UNDO-TREE
;; =====================================
(setq undo-tree-auto-save-history 't)

;; ===================================================
;; Multiple cursors
;; ===================================================

;; Mark continuous lines.
(add-hook 'multiple-cursors-mode-hook
  (lambda ()
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    ;; Or mark a keyword then use one of these to mark others like it.
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
    ;; Use newline to insert newline in multi cursors mode. Use Ctrl-G to exit.
    (define-key mc/keymap (kbd "<return>") nil)))

;; ======================================================
;; String inflection. Useful for change between camel, snake, etc case
;; ======================================================

(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(global-set-key (kbd "C-c i") 'my-string-inflection-cycle-auto)
