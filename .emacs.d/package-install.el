;; ========================================================================
;; Package management
;; https://github.com/raxod502/straight.el
;; See also https://github.com/radian-software/straight.el#faq
;; ========================================================================


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; To reinstall all:
;; M-x straight-pull-all
;; This replaces the custom function we used to have, `package-reinstall-all-activated-packages`)
;; May help after Emacs upgrades or moving to a new system

;; To freeze versions:
;; M-x straight-freeze-versions
;; Then commit the lockfiles in ~/.emacs.d/straight/versions

(straight-use-package 'use-package)

(straight-use-package 'rg)
(straight-use-package 'projectile-ripgrep)
(straight-use-package 'helm)
(straight-use-package 'helm-rg)
(straight-use-package 'yaml-mode)
(straight-use-package 'aggressive-indent)
(straight-use-package 'flycheck)
(straight-use-package 'helm-flycheck)
(straight-use-package 'multi-web-mode)
(straight-use-package 'git-link)
(straight-use-package 'fill-column-indicator)
(straight-use-package 'diminish)
(straight-use-package 'sphinx-doc)
(straight-use-package 'highlight-indentation)
(straight-use-package 's)
(straight-use-package 'pyvenv)
(straight-use-package 'python-mode)
(straight-use-package 'php-mode)
(straight-use-package 'multiple-cursors)
(straight-use-package 'markdown-preview-mode)
(straight-use-package 'magit)
(straight-use-package 'js2-mode)
;; Temporarily disabling jedi-core as it's failing to install via straight :(
;; (straight-use-package 'jedi-core)
(straight-use-package 'helm-projectile)
(straight-use-package 'go-mode)
(straight-use-package 'find-file-in-repository)
;; (straight-use-package 'exec-path-from-shell) ;; This seems to clobber my manual additions and doesn't find homebrew
(straight-use-package 'dumb-jump)
(straight-use-package 'ctable)
(straight-use-package 'auto-complete)
(straight-use-package 'wgrep)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'undo-tree)
(straight-use-package 'lua-mode)
(straight-use-package 'graphql-mode)
(straight-use-package 'protobuf-mode)
; (straight-use-package 'shadowenv)
(straight-use-package 'solaire-mode)
(straight-use-package 'vscode-dark-plus-theme)
(straight-use-package 'gdscript-mode)
(straight-use-package '(chuck-mode :type git :host github :repo "jintwo/chuck-mode" :branch "master"))
(straight-use-package 'haskell-mode)
(straight-use-package 'string-inflection)

;; (eval-when-compile
;;   ;; Following line is not needed if use-package.el is in ~/.emacs.d
;;   ;; (add-to-list 'load-path "<path where use-package is installed>")
;;   (require 'use-package))
