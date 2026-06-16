;; ========================================================================
;; Static HTML/CSS — sharkweasel.com and similar projects
;;
;; Requires in package-install.el:
;;   (straight-use-package 'apheleia)
;; flycheck is already installed.
;;
;; Project needs:
;;   .htmlvalidate.json   (marker for slinkp-in-sharkweasel-p)
;;   .prettierrc.json
;;   .stylelintrc.json
;;   node_modules/        (make install)
;;
;; Optional .envrc with:  PATH_add node_modules/.bin
;; (works with envrc-global-mode in late-init.el)
;;
;; Safe to M-x eval-buffer on this file after flycheck is loaded.
;; ========================================================================

(require 'json)

(defvar slinkp-sharkweasel-html-modes '(html-mode mhtml-mode)
  "Major modes treated as HTML for sharkweasel tooling.")

(defun slinkp-in-sharkweasel-p ()
  "True when buffer is under a project with .htmlvalidate.json."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name ".htmlvalidate.json")))

(defun slinkp-sharkweasel-config-file (filename)
  "Return absolute path to project config FILENAME, or nil."
  (when-let ((root (and buffer-file-name
                        (locate-dominating-file buffer-file-name ".htmlvalidate.json"))))
    (expand-file-name filename root)))

(defun slinkp-sharkweasel-json-list (value)
  "Return VALUE as a list; JSON arrays arrive as vectors in Emacs."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun slinkp-flycheck-parse-html-validate (output _checker _ignored)
  "Parse html-validate JSON output into `flycheck-error' objects."
  (let ((trimmed (string-trim output))
        errors)
    (when (not (string-empty-p trimmed))
      (dolist (result (slinkp-sharkweasel-json-list
                      (json-parse-string trimmed :object-type 'alist)))
        (let ((file (cdr (assoc-string "filePath" result))))
          (dolist (msg (slinkp-sharkweasel-json-list
                         (cdr (assoc-string "messages" result))))
            (push
             (flycheck-error-new-at
              (cdr (assoc-string "line" msg))
              (cdr (assoc-string "column" msg))
              'error
              (cdr (assoc-string "message" msg))
              :id (cdr (assoc-string "ruleId" msg))
              :checker _checker
              :filename file)
             errors)))))
    (nreverse errors)))

(defun slinkp-sharkweasel-web-mode-setup ()
  (when (slinkp-in-sharkweasel-p)
    (setq-local indent-tabs-mode nil)
    (electric-indent-mode -1)
    (when (fboundp 'apheleia-mode)
      (apheleia-mode 1))
    (flycheck-mode 1)
    (setq-local flycheck-checkers '(html-validate))))

(defun slinkp-sharkweasel-css-mode-setup ()
  (when (slinkp-in-sharkweasel-p)
    (setq-local indent-tabs-mode nil)
    (electric-indent-mode -1)
    (when (fboundp 'apheleia-mode)
      (apheleia-mode 1))
    (flycheck-mode 1)
    (setq-local flycheck-checkers '(css-stylelint))))

(defun slinkp-sharkweasel-register-flycheck ()
  "Register html-validate checker. Idempotent for eval-buffer reloads."
  (require 'flycheck)
  (unless (flycheck-valid-checker-p 'html-validate)
    (flycheck-define-checker html-validate
      "HTML standards via html-validate (project .htmlvalidate.json)."
      :command ("html-validate"
                "--formatter" "json"
                "--config" (eval (slinkp-sharkweasel-config-file ".htmlvalidate.json"))
                source)
      :error-parser slinkp-flycheck-parse-html-validate
      :modes (html-mode mhtml-mode)
      :predicate (lambda () (slinkp-in-sharkweasel-p))))
  (dolist (mode slinkp-sharkweasel-html-modes)
    (unless (memq mode (flycheck-checker-get 'html-validate 'modes))
      (flycheck-add-mode 'html-validate mode)))
  (add-to-list 'flycheck-checkers 'html-validate))

(defun slinkp-sharkweasel-add-hooks ()
  "Install mode hooks. Idempotent for eval-buffer reloads."
  (dolist (mode slinkp-sharkweasel-html-modes)
    (let ((hook (intern (format "%s-hook" mode))))
      (remove-hook hook #'slinkp-sharkweasel-web-mode-setup)
      (add-hook hook #'slinkp-sharkweasel-web-mode-setup)))
  (remove-hook 'css-mode-hook #'slinkp-sharkweasel-css-mode-setup)
  (add-hook 'css-mode-hook #'slinkp-sharkweasel-css-mode-setup))

(defun slinkp-sharkweasel-setup ()
  "Full setup: flycheck checker + mode hooks."
  (slinkp-sharkweasel-register-flycheck)
  (slinkp-sharkweasel-add-hooks))

(with-eval-after-load 'flycheck #'slinkp-sharkweasel-setup)

;; eval-buffer path: flycheck may already be loaded before this form runs.
(when (featurep 'flycheck)
  (slinkp-sharkweasel-setup))

;; If flycheck loads later, with-eval-after-load above still runs once.
(when (not (featurep 'flycheck))
  (slinkp-sharkweasel-add-hooks))
