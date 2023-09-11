;; ========================================================================
;; URLs
;; ========================================================================

(setq browse-url-browser-function (quote browse-url-generic))
(when (or running-on-spin? running-on-darwin?)
  (setq browse-url-generic-program "open")
)

(global-set-key [S-mouse-2] 'browse-url-at-mouse)

;; ========================================================================
;; KEYBOARD, MOVEMENT
;; ========================================================================


;; Don't add new lines when scrolling past end of buffer.
(setq next-line-add-newlines nil)

;; Typing when there's a selection causes delete.
;; Also, "DEL" deletes the selection.
(delete-selection-mode t)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Handy window movement keys courtesy Iwillig
(global-set-key
 (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key
 (kbd "C-x C-O") (lambda () (interactive) (other-window -2)))
(global-set-key
 (kbd "C-x C-o") (lambda () (interactive) (other-frame 1)))

;; Also use OSX-style application window switching.
;; Note this leaves `tmm-menubar` unbound; I don't use it.
(global-set-key
 (kbd "M-`") (lambda () (interactive) (other-frame 1)))


;; These may be set by default in trunk? not sure.
;; ... eh, they don't work; overridden somewhere.
;; Anyway, this makes S-left move point to the window to the left, etc.
(when (fboundp 'windmove-default-keybindings)
   (windmove-default-keybindings))

;; Other handy keys courtesy of Chris M.
(global-set-key "\M-g" 'goto-line)

;; no tabs by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;============================================================================
;; OS X specific settings, thanks Will McCutchen & others
;;============================================================================
(when running-on-darwin?
  ;; I used to bind Command to meta but it conflicts with too much
  ;; (from http://www.webweavertech.com/ovidiu/emacs.html)
  ;; ... Trying something new:
  ;; Use left-option OR right-command everywhere! thanks to karabiner
  (setq mac-option-modifier 'meta)
  ;; stop interference from OS X
  (setq mac-pass-control-to-system nil)
  (setq mac-pass-command-to-system nil)
  ;; fix subprocess connections
  (setq process-connection-type nil)

  (global-set-key
   ;; I like it bound to both command-` and option-`
   (kbd "S-`") (lambda () (interactive) (other-frame 1)))
)

;; NEVER close frames via Command-W
;; even if I change other bindings
(global-set-key [(super w)] nil)
