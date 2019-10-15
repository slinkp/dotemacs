;;; shadowenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "shadowenv" "shadowenv.el" (0 0 0 0))
;;; Generated autoloads from shadowenv.el

(autoload 'shadowenv-mode "shadowenv" "\
Shadowenv environment shadowing.

\(fn &optional ARG)" t nil)

(defvar shadowenv-global-mode nil "\
Non-nil if Shadowenv-Global mode is enabled.
See the `shadowenv-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `shadowenv-global-mode'.")

(custom-autoload 'shadowenv-global-mode "shadowenv" nil)

(autoload 'shadowenv-global-mode "shadowenv" "\
Toggle Shadowenv mode in all buffers.
With prefix ARG, enable Shadowenv-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Shadowenv mode is enabled in all buffers where
`shadowenv-mode' would do it.
See `shadowenv-mode' for more information on Shadowenv mode.

\(fn &optional ARG)" t nil)

(autoload 'shadowenv-reload "shadowenv" "\
Reload shadowenv configuration.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shadowenv" '("shadowenv-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shadowenv-autoloads.el ends here
