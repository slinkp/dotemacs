;;; markdown-toc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "markdown-toc" "../../../../../.emacs.d/elpa/markdown-toc-20170711.1949/markdown-toc.el"
;;;;;;  "34c8170a56ca8343ac184af2df5f261b")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/markdown-toc-20170711.1949/markdown-toc.el

(autoload 'markdown-toc-version "markdown-toc" "\
Markdown-toc version.

\(fn)" t nil)

(autoload 'markdown-toc-generate-toc "markdown-toc" "\
Generate a TOC for markdown file at current point.
Deletes any previous TOC.
If called interactively with prefix arg REPLACE-TOC-P, replaces previous TOC.

\(fn &optional REPLACE-TOC-P)" t nil)

(autoload 'markdown-toc-generate-or-refresh-toc "markdown-toc" "\
Generate a TOC for markdown file at current point or refreshes an already generated TOC.

\(fn)" t nil)

(autoload 'markdown-toc-refresh-toc "markdown-toc" "\
Refreshes an already generated TOC.

\(fn)" t nil)

(autoload 'markdown-toc-delete-toc "markdown-toc" "\
Deletes a previously generated TOC.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "markdown-toc"
;;;;;;  "../../../../../.emacs.d/elpa/markdown-toc-20170711.1949/markdown-toc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/markdown-toc-20170711.1949/markdown-toc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "markdown-toc" '("markdown-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/markdown-toc-20170711.1949/markdown-toc-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/markdown-toc-20170711.1949/markdown-toc-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/markdown-toc-20170711.1949/markdown-toc.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; markdown-toc-autoloads.el ends here
