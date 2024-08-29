;; ========================================================================
;; Package management
;; https://github.com/raxod502/straight.el
;; See also https://github.com/radian-software/straight.el#faq
;; ========================================================================


;; It seems we need to remove build-in python.el early.
(when (featurep 'python) (unload-feature 'python t))

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

;; Make use-package use straight under the hood by default.
;; This makes it easier for me to reuse use-package recipes from the web that use eg the :command keyword.
(setq straight-use-package-by-default 't)

(straight-use-package 'python-mode)
(straight-use-package 'rg)
(straight-use-package 'projectile-ripgrep)
(straight-use-package 'helm)
(straight-use-package 'helm-rg)
(straight-use-package 'yaml-mode)
(straight-use-package 'aggressive-indent)
(straight-use-package 'flycheck)
(straight-use-package 'helm-flycheck)
(straight-use-package 'git-link)
(straight-use-package 'fill-column-indicator)
(straight-use-package 'diminish)
(straight-use-package 'highlight-indentation)
(straight-use-package 's)
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
(straight-use-package 'solaire-mode)
(straight-use-package 'vscode-dark-plus-theme)
(straight-use-package 'gdscript-mode)
(straight-use-package '(chuck-mode :type git :host github :repo "jintwo/chuck-mode" :branch "master"))
(straight-use-package 'haskell-mode)
(straight-use-package 'string-inflection)
(straight-use-package 'csv-mode)
(straight-use-package 'envrc) ;; This supports 'direnv' shell command for buffer-local environment vars.
(straight-use-package 'yasnippet) ;; used by lsp

(use-package copilot
  :ensure t
(use-package which-key
  :config
    (which-key-mode))

;; LSP for python at least
(straight-use-package 'lsp-mode)

(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode
  :straight t)

;; (eval-when-compile
;;   ;; Following line is not needed if use-package.el is in ~/.emacs.d
;;   ;; (add-to-list 'load-path "<path where use-package is installed>")
;;   (require 'use-package))

;; More stuff to make python-mode use python-mode.el instead of python.el
(autoload 'python-mode "python-mode" "Python Mode." t)

;; ========================================================================
;; Copilot, per https://github.com/copilot-emacs/copilot.el
;; you can utilize :map :hook and :config to customize copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :config
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))

;; Copilot bootstrapping
(copilot-install-server)
