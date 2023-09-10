;; ========================================================================
;; FONTS
;; Rotate fonts globally in all frames & buffers, with keybindings. Yay.
;; Tweaked from http://ergoemacs.org/emacs/emacs_switching_fonts.html
;; TODO: Check out another approach here https://www.emacswiki.org/emacs/GlobalTextScaleMode
;; ========================================================================

(defun set-font-in-frames (frames fontToUse)
  "Sets font in each frame and redraws it."
  ;; Future frames.
  (set-frame-font fontToUse nil t)
  ;; All current frames.
  (if (not (eq frames nil))
      (progn ;;(message "we got a frame %s" (car frames))
             (set-frame-parameter (car frames) 'font fontToUse)
             ;;(redraw-frame (car frames))  ;; this happens automatically
             (set-font-in-frames (cdr frames) fontToUse)
       )
  )
)

(defun linux-font (points)
  "Make my fave font at the given point size"
  (format "-*-DejaVu Sans Mono-normal-normal-normal-*-%d-*-*-*-m-0-*" points)
)

(defun darwin-font (points)
  ;; Old font:
  ;; (format "-outline-menlo-medium-r-normal--%d-*-*-*-*-*-iso10646-1" points)
  ;; Now trying: https://github.com/intel/intel-one-mono
  ;; installed by downloading the OTF release file and using the "font book" application
  (format " -*-IntelOne Mono-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1" points)
)

;; Set the default.
(when (and running-on-linux? we-have-gui?)
  ;; (set-font-in-frames (visible-frame-list) "-*-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*")
  (set-font-in-frames (visible-frame-list) (linux-font 14))
)


(when (and running-on-darwin? we-have-gui?)
  (set-font-in-frames (visible-frame-list) (darwin-font 13))
)

;; TODO automate the copy/paste font name crap
(defun cycle-font (num)
  "Change font in all visible frames.
Each time this is called, font cycles thru a predefined set of fonts.
If NUM is 1, cycle forward.
If NUM is -1, cycle backward."
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (fontList fontToUse currentState nextState )
    (when (and running-on-darwin? we-have-gui?)
      (setq fontList (list
                      (darwin-font 8)
                      (darwin-font 9)
                      (darwin-font 10)
                      (darwin-font 11)
                      (darwin-font 12)
                      (darwin-font 13)
                      (darwin-font 14)
                      (darwin-font 15)
                      (darwin-font 16)
                      (darwin-font 17)
                      (darwin-font 18)
                      (darwin-font 19)
                      (darwin-font 20)
                      (darwin-font 21)
                      (darwin-font 22)
                      ))
    )
    (when (and running-on-linux? we-have-gui?)

      (setq fontList (list
                      (linux-font 10)
                      (linux-font 11)
                      (linux-font 12)
                      ;; 13-point saves only verticl space compared to 14
                      (linux-font 13)
                      (linux-font 14)
                      (linux-font 15)
                      (linux-font 16)
                      (linux-font 17)
                      (linux-font 22)
                      ))
    )
    (setq currentState (if (get 'cycle-font 'state) (get 'cycle-font 'state) 0))
    (setq nextState (% (+ currentState (length fontList) num) (length fontList)))

    (setq fontToUse (nth nextState fontList))
    ;; Update all visible frames.
    ;;; We should be able to do something like this:
    ;; (modify-all-frames-parameters (list (cons 'font (list (. fontToUse)))))
    ;; (redraw-display)
    ;;; ... but that gives errors like "invalid font", don't know why.
    ;;; So, here's a homegrown function to do it:
    (set-font-in-frames (visible-frame-list) fontToUse)
    (message "Current font is: %s" fontToUse )
    (put 'cycle-font 'state nextState)
  )
)
(defun cycle-font-forward ()
  "Switch to the next font, in all frames.
See `cycle-font'."
  (interactive)
  (cycle-font 1)
)

(defun cycle-font-backward ()
  "Switch to the previous font, all frames.
See `cycle-font'."
  (interactive)
  (cycle-font -1)
)

;;;; While emacs has something similar built-in...
;; (global-set-key [(meta +)] 'text-scale-adjust )
;; (global-set-key [(meta _)] 'text-scale-adjust )
;;;; ... it only applies to the current buffer, bleh.
;;;; So I'll use mine, at least for now.
(global-set-key [(meta +)] 'cycle-font-forward)
(global-set-key (kbd "<C-+>") 'cycle-font-forward)
(global-set-key [(control +)] 'cycle-font-forward)
(global-set-key [(meta _)] 'cycle-font-backward)
