;; ========================================================================
;; PYTHON part 2 - main config
;; ========================================================================

;; Per https://gitlab.com/python-mode-devs/python-mode set this nil??
(setq py-load-pymacs-p nil)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vpy$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cpy$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook
  (lambda () 
   (require 'flycheck)
    (setq flycheck-checker-error-threshold 800)  ;; default 400
    (flycheck-mode t)))

;; Get dired to consider .pyc and .pyo files to be uninteresting
(add-hook 'dired-load-hook
  (lambda ()
    (load "dired-x")
  ))
(add-hook 'dired-mode-hook
  (lambda ()
    (setq dired-omit-mode t)
;    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  ))
(load "dired")
(setq dired-omit-extensions (append '(".pyc" ".pyo" ".bak")
                             dired-omit-extensions))

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

(defvar ropemacs-was-loaded nil)

(defun load-pymacs-and-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-enable-shortcuts t)
  (setq ropemacs-was-loaded t)
  ;; Can't seem to find the right place to hook this :(
  (diminish 'ropemacs-mode)
)
(global-set-key "\C-xpl" 'load-pymacs-and-ropemacs)

;; Neat function from Evan Bender: if a python function def is too long,
;; this splits it into multiple lines

(defun multiline-it ()
  (interactive)
  ;; narrow to region around current line
  (end-of-line)
  (set-mark-command nil)
  (beginning-of-line)
  (narrow-to-region (mark) (point))
  ;; TODO insert newline after only the FIRST left-paren in region.
  ;; replace in region
  (replace-string "\(" "(\n")
  (replace-string ", " ",\n")
  ;; indent in region
  (goto-char (point-min))
  (set-mark-command nil)
  (goto-char (point-max))
  (indent-region (mark) (point))
  ;; back to normal
  (widen))


;; sphinx-doc
;; https://github.com/naiquevin/sphinx-doc.el
;; use C-c M-d
;; can insert skeletons and update existing docstrings!
(add-hook 'python-mode-hook
  (lambda ()
    (require 'sphinx-doc)
    (sphinx-doc-mode t)))

;; Highlight lines with pdb.set_trace
;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)


;; Long line column marker
(add-hook 'python-mode-hook
  (lambda ()
     (set-fill-column 100)
     ;; (fci-mode)
  )
)

;; handy M-p binding for quick python debugging.
(defun slinkp-pdb-set-trace ()
  "Insert a set_trace() call after the previous line, maintaining indentation"
  (interactive)
  (forward-line -1)
  (end-of-line)
  (insert "\n")
  (indent-according-to-mode)
  (insert "import ipdb; ipdb.set_trace()")
  (indent-according-to-mode)
;;  (annotate-pdb)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtualenvs for python
;(require 'virtualenvwrapper)
; (venv-initialize-interactive-shells) ;; if you want interactive shell support
; (venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(setq venv-location "/Users/paul/.emacs.d/.python-environments/")



;; Simpler way to activate as per https://github.com/porterjamesj/virtualenvwrapper.el#automatically-activating-a-virtualenv-when-using-projectile
;; ... just use a venv or .venv dir in a projectile project root!
(setq projectile-switch-project-action 'venv-projectile-auto-workon)
(add-hook 'python-mode-hook 'jedi:setup)

;; These seem to be sensitive to order.
;; Works ok if I do which-function-mode late enough?
;; Show current function in status bar.
;; ... was horribly slow on some .py files before emacs 24.5, seems ok now?
(add-hook 'python-mode-hook 'which-function-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python keyboard overrides.
;; python-mode.el clobbers slinkp-vi-join, grr.
;; TODO: Why does this seem to sometimes work, and sometimes not
;; unless I've done (require 'python-mode)?
;; Why isn't (portable-load-library) enough?
;; (portable-load-library "pymacs")
;; (portable-load-library "python-mode")
;; (require 'python-mode)
;; (require 'pymacs)


(add-hook 'python-mode-hook
  (lambda ()
    (define-key python-mode-map (kbd "C-j") 'slinkp-vi-join)
    (define-key python-mode-map (kbd "M-p") 'slinkp-pdb-set-trace) 
    ;;; DISABLING ROPE BY DEFAULT ... it is choking too much on big stuff.
    ;;; ... or maybe that was just which-function-mode? Trying without that.
    ;; (unless ropemacs-was-loaded
    ;;   (load-pymacs-and-ropemacs))
    ;; Override rope-goto-definition binding because jedi has a back button!
    (define-key python-mode-map (kbd "C-c g") 'jedi:goto-definition)
    (define-key python-mode-map (kbd "C-c C-g") 'jedi:goto-definition-pop-marker)
    ;; Jedi complete is super annoying when implicit
    (setq jedi:complete-on-dot nil)
  )
)
