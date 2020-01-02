;;; ack-and-a-half-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ack-and-a-half" "../../../../../.emacs.d/elpa/ack-and-a-half-1.2.0/ack-and-a-half.el"
;;;;;;  "b733d1b0d96b54711094e710383fdfac")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/ack-and-a-half-1.2.0/ack-and-a-half.el

(autoload 'ack-and-a-half "ack-and-a-half" "\
Run ack.
PATTERN is interpreted as a regular expression, iff REGEXP is
non-nil.  If called interactively, the value of REGEXP is
determined by `ack-and-a-half-regexp-search'.  A prefix arg
toggles the behavior.  DIRECTORY is the root directory.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns'.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set.

\(fn PATTERN &optional REGEXP DIRECTORY)" t nil)

(autoload 'ack-and-a-half-same "ack-and-a-half" "\
Run ack with --type matching the current `major-mode'.
The types of files searched are determined by
`ack-and-a-half-mode-type-alist' and
`ack-and-a-half-mode-extension-alist'.  If no type is configured,
the buffer's file extension is used for the search.  PATTERN is
interpreted as a regular expression, iff REGEXP is non-nil.  If
called interactively, the value of REGEXP is determined by
`ack-and-a-half-regexp-search'.  A prefix arg toggles that value.
DIRECTORY is the directory in which to start searching.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns`.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set.`

\(fn PATTERN &optional REGEXP DIRECTORY)" t nil)

(autoload 'ack-and-a-half-find-file "ack-and-a-half" "\
Prompt to find a file found by ack in DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" "\
Prompt to find a file found by ack in DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ack-and-a-half"
;;;;;;  "../../../../../.emacs.d/elpa/ack-and-a-half-1.2.0/ack-and-a-half.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/ack-and-a-half-1.2.0/ack-and-a-half.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ack-and-a-half" '("ack-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/ack-and-a-half-1.2.0/ack-and-a-half-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/ack-and-a-half-1.2.0/ack-and-a-half.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ack-and-a-half-autoloads.el ends here
