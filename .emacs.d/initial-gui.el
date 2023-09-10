;;============================================================================
;; Initial appearance.
;; Do these early to avoid things jumping in and out on startup.
;; ============================================================================

;; Simple clean interface.
(if (boundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Don't show the GNU splash screen on startup.
(setq inhibit-startup-message t)
;; Cleaner scratch buffer.
(setq initial-scratch-message "**scratch**\n\n")

;; VSCode-like theme
(solaire-global-mode 1)
(load-theme 'vscode-dark-plus t)

; Try to allow scalable fonts under X, see
; http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_637.html
(setq scalable-fonts-allowed t)

;; Show column number at cursor.
(setq column-number-mode t)
(setq line-number-mode t)

;; Don't beep.
(setq visible-bell t)

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs?)
	(global-font-lock-mode t)
	(setq font-lock-maximum-decoration t)
))

;; Show selection.
(transient-mark-mode t)

;; Colorize matching delimiters.
(when (load "rainbow-delimiters" t)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
)
