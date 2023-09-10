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
