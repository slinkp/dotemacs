;; ======================================================================
;; Misc language modes that don't merit their own file (yet)

;; turn on SYNTAX HIGHLIGHTING for language modes

(add-hook 'c-mode-hook 'font-lock-mode)
(add-hook 'emacs-lisp-mode-hook 'font-lock-mode)
(add-hook 'lisp-mode-hook 'font-lock-mode)
(add-hook 'makefile-mode-hook 'font-lock-mode)
(add-hook 'perl-mode-hook 'font-lock-mode)
(add-hook 'python-mode-hook 'font-lock-mode)
(add-hook 'sgml-mode-hook 'font-lock-mode)
(add-hook 'sh-mode-hook 'font-lock-mode)
(add-hook 'shell-script-mode-hook 'font-lock-mode)

(setq auto-mode-alist (cons '("\\.html\.raw$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.dtml$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zpt$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pt$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zcml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rdf$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mako$" . html-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.sgml$" . sgml-mode) auto-mode-alist))

;; this is useful with Ian B's svncommit shell function
(setq auto-mode-alist (cons '("svn-commit.tmp" . diff-mode) auto-mode-alist))


;; Handle ANSI colorization in shell mode.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Fill + markup = hell.
(add-hook 'sgml-mode-hook 'turn-off-auto-fill)
(add-hook 'html-mode-hook 'turn-off-auto-fill)
(add-hook 'nxml-mode-hook 'turn-off-auto-fill)

;; And for some reason this wasn't a problem until recently...
(add-hook 'python-mode-hook 'turn-off-auto-fill)
(add-hook 'js-mode-hook 'turn-off-auto-fill)
(add-hook 'sh-mode-hook 'turn-off-auto-fill)


;; ========================================================================
;; Godot game scripts
(add-to-list 'auto-mode-alist '("\\.gd$" . gdscript-mode))
(add-to-list 'auto-mode-alist '("\\.tscn$" . gdscript-mode))

;; ========================================================================
;; TCL

(add-to-list 'auto-mode-alist '("\\.adp$" . tcl-mode))

;; ========================================================================
;; YAML
;; ========================================================================

(add-to-list 'auto-mode-alist '("\\.raml$" . yaml-mode))

;; ========================================================================
;; ReST
;; ========================================================================

(add-hook 'rst-mode-hook 'fci-mode)

;; ========================================================================
;; ChucK
;; ========================================================================

;; Unclear why i need to `require` here, as straight seems to be symlinking properly?
(require 'chuck-mode)
(add-to-list 'auto-mode-alist '("\\.ck$" . chuck-mode))

;; =============================================================================
;; CSV
;; =============================================================================

(add-hook 'csv-mode 'rainbow-csv-mode)
(add-hook 'csv-mode 'csv-align-mode)


;; ========================================================================
;; JAVASCRIPT
;; ========================================================================

;; Nicer js mode, thanks Whit.
;; Prefer forked and improved version from:
;; https://github.com/mooz/js2-mode

(when (locate-library "js2-mode")
  (defun my-js2-mode-hook ()
    ;; However, its indentation is really obnoxious when working with existing
    ;; code.  This helps... some.
        (setq js2-auto-indent-flag nil)
    (setq js2-enter-indents-newline nil)
    (setq js2-mode-indent-ignore-first-tab t)
    (setq js2-mode-indent-inhibit-undo t)
    ;; Will likes these.
    (setq js2-auto-indent-p t
          js2-highlight-level 3
          ;; available in forked js2-mode
          js2-consistent-level-indent-inner-bracket-p t
          js2-pretty-multiline-decl-indentation-p t)
    (whitespace-mode 1)  ;; I like to see wtf is going on with indentation
    (message "Custom js2-mode hook"))

  (autoload 'js2-mode "js2-mode" nil t)

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

  (add-hook 'js2-mode-hook 'my-js2-mode-hook))


;; js2-mode is not that happy with raw JSON
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))

;; ========================================================================
;; C
;; ========================================================================

; use python-style indentation.
(add-hook 'c-mode-hook
  (function (lambda ()
    (setq c-basic-offset 4)
    (setq c-indent-level 4))))


;; (setq insert-date-format "%c")
;; (defun insert-date ()
;;   "Insert the current date according to the variable
;; \"insert-date-format\"."
;;   (interactive "*")
;;   (insert (format-time-string insert-date-format
;;                               (current-time))))

;; (global-set-key [(control ?c) ?d] 'insert-date)



;;============================================================================
;; go-mode
;;============================================================================
(when (locate-library "go-mode")
  (defun my-go-mode-hook ()
    (setq indent-tabs-mode t
          tab-width 4
          whitespace-style '(lines-tail face))
    (add-hook 'before-save-hook 'gofmt-before-save))
  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
  (add-hook 'go-mode-hook 'my-go-mode-hook))


;; ========================================================================
;; CSS
;; (add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))

;; ========================================================================
;; RUBY
;; ========================================================================

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rbi\\'" . ruby-mode))

(add-to-list 'interpreter-mode-alist
  '("ruby1.9" . ruby-mode))

;; For some reason, shebang lines like '#!/usr/bin/env -S ruby --disable-gems' didn't work.
(add-to-list 'magic-mode-alist
 '("#\\!.*ruby.*" . ruby-mode))

(add-hook 'ruby-mode-hook 'which-function-mode)

;; Navigation
;; ... nope too slow for Shopify/shopify
;; (add-hook 'ruby-mode-hook 'robe-mode)

(defun annotate-pry ()
  (interactive)
  (highlight-lines-matching-regexp "require .*pry")
  (highlight-lines-matching-regexp "require .*byebug")
  (highlight-lines-matching-regexp "binding.pry"))

(add-hook 'ruby-mode-hook 'annotate-pry)


;; M-p binding for ruby debugging.
(defun slinkp-binding-pry ()
  "Insert a binding.pry call after the previous line, maintaining indentation"
  (interactive)
  (forward-line -1)
  (end-of-line)
  (insert "\n")
  (indent-according-to-mode)
  (insert "require 'pry-byebug'; binding.pry")
  (indent-according-to-mode)
)


;; Smart TODO helper
(defun smart-todo ()
  "Insert a smart TODO as per https://github.com/Shopify/smart_todo"
  (interactive)
  (forward-line -1)
  (end-of-line)
  (insert "\n")
  (indent-according-to-mode)
  (insert
   (concat
    "# TODO(on: date('"
    (format-time-string "%Y-%m-%d" (time-add (current-time) (* 60 60 24 60)))
     "'), to: 'slinkp@gmail.com')\n"
     ))
  (indent-according-to-mode)
  (insert "#   ")
)

;; Hideshow support for ruby, minimal per https://chrisbarrettnz.wordpress.com/2013/06/15/ruby-code-folding-with-emacs/

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                `(ruby-mode
                  ,(rx (or "def" "class" "module" "{" "[")) ; Block start
                  ,(rx (or "}" "]" "end"))                  ; Block end
                  ,(rx (or "#" "=begin"))                   ; Comment start
                  ruby-forward-sexp nil)))

(defun my-find-file-hook ()
  "If a file is over a given size, disable expensive things."
  (when (> (buffer-size) (* 1024 50))
    (set (make-variable-buffer-local 'flycheck-mode) nil)))

(add-hook 'find-file-hook 'my-find-file-hook)

;; Flycheck for ruby
(add-hook 'ruby-mode-hook
  (lambda ()
    (require 'flycheck)
    ;; The related modes (ruby-rubocop, ruby-rubylint, etc) should come automatically if available.
    (setq flycheck-checker-error-threshold 800)  ;; default 400
    (flycheck-mode t)
    (define-key ruby-mode-map (kbd "M-p") 'slinkp-binding-pry)
    ;; (setq flycheck-ruby-rubocop-executable "bundle-exec-rubocop.sh")
    (setq flycheck-ruby-executable "/var/folders/vc/0jdl4b553039ywqyjgl398m40000gn/T/frum_15129_1685631593134/bin/ruby")
    )
)

;; ======================================================================
;; Markdown

;; Markdown-mode needs imenu enabled in order to work with which-function-mode?
(add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
(setq imenu-auto-rescan t)
;; ... does not seem to help :-(
;; Issue filed & closed here: https://github.com/jrblevin/markdown-mode/issues/765

;; ---------------------------------------------------------------------------
;; Markdown realtime preview:
;;
;; use `M-x markdown-preview-mode` to start.
;;
;; It auto-reloads in browser.
;; config'd via Customize so not much to see here.
;; Notably we want good support for gfm markdown extensions.
;; A usable customization for that is eg '(markdown-command "pandoc --from gfm --to html")
;; as long as pandoc is installed.
;;
;; Things we can't customize:
;; Stylesheet override, the default is unreadably dark.
;; TODO: support highlights for code block language modes.
;; Tables are a bit bland too.

;; This is more readable than the default solarized-dark theme, but i want table borders
;; (setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))

;; This is closer to github with white background.
;; Decent, usable, kind of narrow document body.
;; Problem: links look like plain text.
(setq markdown-preview-stylesheets
      (list
       "https://cdn.jsdelivr.net/gh/pixelbrackets/gfm-stylesheet/dist/gfm.min.css"
       ;; This site proxies github and changes content-type to text/css
       "https://raw.githack.com/slinkp/dotemacs/main/.emacs.d/markdown-preview-local-overrides.css"
       ;; https://raw.githubusercontent.com/slinkp/dotemacs/main/.emacs.d/markdown-preview-local-overrides.css"
       ))

;; Similar, like github but dark theme.
;; Problem: links are dark/invisible.
;; Still narrow doc body.
;; Article background is black but doc body is white and there's no margin, so text runs right up to edge :(
;; Code blocks have slightly lighter background so they stand out nice.
;; (setq markdown-preview-stylesheets (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.5.0/github-markdown.min.css"))

;; Combined solarized dark and github dark? Works ok, but links are still dark/invisible
;; (setq markdown-preview-stylesheets
;;       (list
;;        "http://thomasf.github.io/solarized-css/solarized-dark.min.css"
;;        "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.5.0/github-markdown.min.css"
;;        ))
