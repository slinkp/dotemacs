;;; modeline-posn-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "modeline-posn" "../../../../../.emacs.d/elpa/modeline-posn-22.0/modeline-posn.el"
;;;;;;  "04d1f9968774df1bd60c8b246f42a026")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/modeline-posn-22.0/modeline-posn.el

(defface modelinepos-column-warning '((t (:foreground "Red"))) "\
*Face used to highlight the modeline column number.
This is used when the current column number is greater than
`modelinepos-column-limit'." :group (quote Modeline) :group (quote Convenience) :group (quote Help) :group (quote faces))

(defface modelinepos-region '((t :inherit region)) "\
*Face used to highlight the modeline position and size when
the region is active." :group (quote Modeline) :group (quote Convenience) :group (quote Help) :group (quote faces))

(defvar modelinepos-column-limit 70 "\
*Current column greater than this means highlight column in mode-line.")

(custom-autoload 'modelinepos-column-limit "modeline-posn" t)

(defvar modelinepos-style '(" %d ch, %d l" (abs (- (mark t) (point))) (count-lines (mark t) (point))) "\
*What info to include about the region size, in mode-line.
Value `chars+lines' means print the number of characters and the number of lines.")

(custom-autoload 'modelinepos-style "modeline-posn" t)

(defvar size-indication-mode nil "\
Non-nil if Size-Indication mode is enabled.
See the `size-indication-mode' command
for a description of this minor mode.")

(custom-autoload 'size-indication-mode "modeline-posn" nil)

(autoload 'size-indication-mode "modeline-posn" "\
Toggle Size Indication mode.
With arg, turn Size Indication mode on iff arg is positive.
When Size Indication mode is enabled, the buffer or region size
appears in the mode line.  If Transient Mark mode is enabled, the
region size is shown; otherwise, the size of the accessible part
of the buffer is shown.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/modeline-posn-22.0/modeline-posn-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/modeline-posn-22.0/modeline-posn.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modeline-posn-autoloads.el ends here
