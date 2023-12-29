;; ========================================================================
;; PYTHON part 2 - main config
;; ========================================================================

;; LSP AUTO-MAGIC TODO:
;; 1. Get direnv working manually in shell:
;;    ... yep "source bin/activate" in an .envrc works.
;;    Adding stub .envrc files seems worth it, very handy in shell too!
;; DONE
;;;
; 2. Get direnv working inside emacs as per https://github.com/purcell/envrc
;; ... this should make it correctly buffer-local.
;; DONE
;; 3. Switch from python-mode.el to python.el, at least for now.
;; DONE
;; 4. Disable all the various emacs virtualenv gunk I've tried.
;; DONE
;; 5. Confirm virtualenv correctly automatically activated for each python buffer in emacs
;; DONE
;; 6. THEN try LSP again.
;; DONE. WORKS!! Have to `pip install python-lsp-server`
;; 7. Try flake8 w/ LSP
;; WORKS! have to `pip install flake8`
;; 8. Try pylsp-mypy
;; WORKS! Have to `pip install pylsp-mypy`
;; 9. TODO more cleanup, there's crap here I don't use: pymacs, ropemacs, jedi
;; DONE
;; 10. TODO Try going back to python-mode, I miss the indentation support
;; DONE

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vpy$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cpy$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; TODO - disable flycheck IF pylsp is enabled
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
    (sphinx-doc-mode t)
    (diminish 'sphinx-doc-mode)
    ))

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
    (define-key python-mode-map (kbd "M-p") 'slinkp-pdb-set-trace)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 'lsp-deferred)

;; Could also try via lsp-pyright (Microsoft's thing)
;; (use-package lsp-pyright
;;   :straight t
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))
