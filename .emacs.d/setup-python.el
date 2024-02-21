;; ========================================================================
;; PYTHON part 2 - main config
;; ========================================================================

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vpy$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cpy$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


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

;; Highlight lines with pdb.set_trace
;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
(defun annotate-pdb ()
  "Highlight lines with set_trace() in them"
  (interactive)
  (highlight-lines-matching-regexp "import .*pdb")
  (highlight-lines-matching-regexp ".set_trace()")
  (highlight-lines-matching-regexp "breakpoint()"))
(add-hook 'python-mode-hook 'annotate-pdb)


;; Long line column marker
(add-hook 'python-mode-hook
  (lambda ()
     (set-fill-column 100)
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
  (insert "breakpoint()")
  (indent-according-to-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python keyboard overrides.

(add-hook 'python-mode-hook
  (lambda ()
    (message "Paul's python emacs hook")
    ;; python-mode.el clobbers slinkp-vi-join, grr.
    (define-key python-mode-map (kbd "C-j") 'slinkp-vi-join)
    (define-key python-mode-map (kbd "M-p") 'slinkp-pdb-set-trace)
    ;; Apparently (kbd "TAB") is still bound to 'py-indent-or-complete',
    ;; but somehow that Just Works with LSP and the jedi completion backend?
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 'lsp-deferred)

(with-eval-after-load 'lsp-mode
  ;; Which-key helps me remember / learn keybindings
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  ;; Backend for formatting
  (setq lsp-pylsp-plugins-black-enabled 't)
  ;; Completion
  (setq lsp-pylsp-plugins-jedi-completion-enabled 't)
  ;; Rebind xref reference keys
  (add-hook 'lsp-mode-hook
   (lambda ()
     (
      (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)  ; M-.
      (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)  ; M-?
      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP UI config

;; Sideline UI
(setq lsp-ui-sideline-enable 't
      lsp-ui-sideline-show-diagnostics 't ; show diagnostics messages in sideline, eg type errors.
      lsp-ui-sideline-show-hover 't ; show hover messages in sideline. Often type info.
      lsp-ui-sideline-show-code-actions 't ; show code actions in sideline. Example??
      lsp-ui-sideline-update-mode "point" ; When set to 'line' the information will be updated when
      ;; user changes current line otherwise the information will be updated when user changes current point.
      lsp-ui-sideline-delay 0.02 ; seconds to wait before showing sideline
      )

;; Other UI
(setq lsp-ui-doc-enable 't ; docstrings on hover.
      lsp-ui-peek-enable 't ; peek at definition or matches, instead of a big context switch
      lsp-ui-peek-always-show 't
      )


;; Could also try via lsp-pyright (Microsoft's thing)
;; (use-package lsp-pyright
;;   :straight t
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))
