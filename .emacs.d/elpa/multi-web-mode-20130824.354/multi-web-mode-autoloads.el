;;; multi-web-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "multi-web-mode" "../../../../../.emacs.d/elpa/multi-web-mode-20130824.354/multi-web-mode.el"
;;;;;;  "90e6f8192fcea84ae890827400d4d09b")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multi-web-mode-20130824.354/multi-web-mode.el

(autoload 'multi-web-mode "multi-web-mode" "\
Enables the multi web mode chunk detection and indentation

\(fn &optional ARG)" t nil)

(defvar multi-web-global-mode nil "\
Non-nil if Multi-Web-Global mode is enabled.
See the `multi-web-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `multi-web-global-mode'.")

(custom-autoload 'multi-web-global-mode "multi-web-mode" nil)

(autoload 'multi-web-global-mode "multi-web-mode" "\
Toggle Multi-Web mode in all buffers.
With prefix ARG, enable Multi-Web-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Multi-Web mode is enabled in all buffers where
`multi-web-mode-maybe' would do it.
See `multi-web-mode' for more information on Multi-Web mode.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "multi-web-mode"
;;;;;;  "../../../../../.emacs.d/elpa/multi-web-mode-20130824.354/multi-web-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multi-web-mode-20130824.354/multi-web-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-web-mode" '("mweb-" "multi-web-mode-maybe")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/multi-web-mode-20130824.354/multi-web-mode-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/multi-web-mode-20130824.354/multi-web-mode-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/multi-web-mode-20130824.354/multi-web-mode.el"
;;;;;;  "../../../../../.emacs.d/elpa/multi-web-mode-20130824.354/mweb-example-config.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multi-web-mode-autoloads.el ends here
