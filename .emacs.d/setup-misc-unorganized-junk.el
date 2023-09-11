;; ========================================================================
;; MAIL
;; ========================================================================

;; Set up mail headers
(setq user-mail-address "slinkp@gmail.com")
(setq mail-default-reply-to "slinkp@gmail.com")
(setq user-full-name "Paul Winkler")

;; Mail mode: use for mutt, and only in this buffer wrap more aggressively.
(setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist))
(add-hook 'mail-mode
    (lambda () (set-fill-column 73)))

