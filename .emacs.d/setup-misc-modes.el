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
