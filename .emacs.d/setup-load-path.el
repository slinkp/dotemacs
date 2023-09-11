;;============================================================================
;; Load path
;;============================================================================

;; local stuff may be loaded from here.

;; per https://github.com/magnars/.emacs.d/blob/master/init.el
;; Add external projects to load path
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; I like my ~/.emacs.d/ to override, so it goes first.
(setq load-path (cons site-lisp-dir load-path))

;; Is this still needed for anything? Pre-dates using straight...
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

