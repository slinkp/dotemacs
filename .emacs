;; Paul M. Winkler .emacs file

;;============================================================================
;; Load path
;;============================================================================

;; local stuff may be loaded from here.

;; Try that again, per https://github.com/magnars/.emacs.d/blob/master/init.el
;; Add external projects to load path
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; I like my ~/.emacs.d/ to override, so it goes first.
(setq load-path (cons site-lisp-dir load-path))

(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;;============================================================================
;; Modularizing my config, gradually...

(defconst slinkp:config-dir "~/.emacs.d/" "")

;; utility function to auto-load other config files
(defun slinkp:load-config-file (filelist)
  (dolist (file filelist)
    (load (expand-file-name 
           (concat slinkp:config-dir file)))
    (message "Loaded config file:%s" file)
    ))

(slinkp:load-config-file
 '("setup-native-comp"
   "package-install"
   "platform-detection"
   "initial-gui"
   "setup-python"
   "setup-misc-functions"
   "setup-font-defaults-and-switching"
   ;; ... add more files here
   ))


;; ========================================================================
;; URLs
;; ========================================================================

(setq browse-url-browser-function (quote browse-url-generic))
(when (or running-on-spin? running-on-darwin?)
  (setq browse-url-generic-program "open")
)

(global-set-key [S-mouse-2] 'browse-url-at-mouse)

;; ========================================================================
;; KEYBOARD, MOVEMENT
;; ========================================================================


;; Don't add new lines when scrolling past end of buffer.
(setq next-line-add-newlines nil)

;; Typing when there's a selection causes delete.
;; Also, "DEL" deletes the selection.
(delete-selection-mode t)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Handy window movement keys courtesy Iwillig
(global-set-key
 (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key
 (kbd "C-x C-O") (lambda () (interactive) (other-window -2)))
(global-set-key
 (kbd "C-x C-o") (lambda () (interactive) (other-frame 1)))

;; Also use OSX-style application window switching.
;; Note this leaves `tmm-menubar` unbound; I don't use it.
(global-set-key
 (kbd "M-`") (lambda () (interactive) (other-frame 1)))


;; These may be set by default in trunk? not sure.
;; ... eh, they don't work; overridden somewhere.
;; Anyway, this makes S-left move point to the window to the left, etc.
(when (fboundp 'windmove-default-keybindings)
   (windmove-default-keybindings))

;; Other handy keys courtesy of Chris M.
(global-set-key "\M-g" 'goto-line)

;; no tabs by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; ========================================================================
;; MAIL
;; ========================================================================

;; Set up mail headers
(setq user-mail-address "slinkp@gmail.com")
(setq mail-default-reply-to "slinkp@gmail.com")
(setq user-full-name "Paul Winkler")

;; Mail mode: use for mutt, and only in this buffer wrap more aggressively.
(setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist))
(add-hook 'mail-mode
    (lambda () (set-fill-column 73)))


;; ========================================================================
;; CUSTOM output
;; ========================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(case-fold-search t)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(current-language-environment "English")
 '(custom-safe-themes
   '("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default))
 '(default-frame-alist '((menu-bar-lines . 1)))
 '(dumb-jump-max-find-time 4)
 '(flycheck-display-errors-delay 0.25)
 '(git-link-open-in-browser t)
 '(global-font-lock-mode t nil (font-lock))
 '(helm-M-x-fuzzy-match nil)
 '(helm-ff-fuzzy-matching nil)
 '(helm-projectile-fuzzy-match nil)
 '(helm-projectile-git-grep-command "git --no-pager grep -P --no-color -n%c -e %p -- %f")
 '(inhibit-startup-echo-area-message "pw")
 '(jit-lock-stealth-time 0.035)
 '(markdown-command "pandoc --from gfm --to html --standalone")
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-position mode-line-misc-info mode-line-modes
     (vc-mode vc-mode)
     mode-line-end-spaces))
 '(protect-buffer-bury-p nil)
 '(py-load-pymacs-p nil t)
 '(py-pdbtrack-do-tracking-p t)
 '(safe-local-variable-values
   '((test-case-name . buildbot\.test\.test_transfer)
     (test-case-name . buildbot\.test\.test_vc)
     (test-case-name . buildbot\.test\.test_steps\,buildbot\.test\.test_properties)
     (test-case-name . buildbot\.test\.test_run)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(tramp-default-method "ssh")
 '(undo-outer-limit 24000000)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-doc-face ((t (:inherit font-lock-string-face))))
 '(font-lock-reference-face ((((class color) (background light)) (:foreground "Yellow"))) t)
 '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:background "#303040" :foreground "#dd8070"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Goldenrod" :background "DarkSlateBlue"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "#B0B0FF"))))
 '(which-func ((((class color) (background dark)) (:foreground "#B0B0FF"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#c0bfcf"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#c0c173"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#d79078"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#8ede98"))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black" :inverse-video t))) t)
 '(rst-level-2-face ((t (:background "grey78" :foreground "black" :inverse-video t))) t)
 '(rst-level-3-face ((t (:background "grey71" :foreground "black" :inverse-video t))) t)
 '(trailing-whitespace ((t (:background "#002232")))))


;; ========================================================================
;; MODES
;; ========================================================================

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

(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html\.raw$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.dtml$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zpt$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pt$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zcml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rdf$" . nxml-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons '("\\.php3$" . html-mode) auto-mode-alist))
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

;; Whitespace mode is kinda cool, kinda awful...
;; really useful for those cases where the mode i'm using is wrong about
;; what indentation i want (grrr js2-mode).
;; Clean it up a lot as per http://xahlee.org/emacs/whitespace-mode.html
; Make whitespace-mode use just basic coloring.
(setq whitespace-style (quote
  ( spaces tabs newline space-mark tab-mark newline-mark)))
;; make whitespace-mode use “¶” for newline and “▷” for tab.
;; together with the rest of its defaults
(setq whitespace-display-mappings
 '(
   ;(space-mark 32 [183] [46]) ; normal space, ·
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [182 10]) ; newlne, ¶
   (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
))

; (autoload 'cc-mode "cc-mode" "CC Mode" t)


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


;; ======================================================================
;; HTML / MULTI-WEB
;; ======================================================================

;; Override new HTML template

(setq html-helper-new-buffer-template
  '(html-helper-htmldtd-version
    "<html>\n<head>\n"
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n"
    "<title>" p "</title>\n</head>\n\n"
    "<body>\n"
    "</body>\n</html>\n"))

;; Multi-web mode, see https://github.com/fgallina/multi-web-mode

(setq mweb-default-major-mode 'html-mode)
 (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  ;; (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
;;                   (ruby-mode "<%= " " %>")
))

 (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


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

;(add-hook 'ruby-mode-hook 'shadowenv-mode)
(add-hook 'ruby-mode-hook 'which-function-mode)

;; Navigation
;; ... nope too slow for Shopify/shopify
;; (add-hook 'ruby-mode-hook 'robe-mode)

(defun annotate-pry ()
  (interactive)
  (highlight-lines-matching-regexp "require 'pry'")
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

;; ===========================================================
;; Helm
;; ===========================================================

(require 'helm)
(when (fboundp 'helm)
  ;; way cooler than default M-x
  (global-set-key (kbd "M-x") 'helm-M-x)
  ;; I never use the default M-y "yank-pop" command.
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  ;; Replace switch-to-buffer.
  (global-set-key (kbd "C-x b") 'helm-mini)

  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)

  ;; Performance tweak per #emacs on shopify slack. Try 40 if too slow?
  (setq helm-candidate-number-limit 60)

  ;; Searching commands vaguely on par with VSCode
  (global-set-key (kbd "M-P") 'helm-projectile)
  (global-set-key (kbd "M-F") 'helm-git-grep)
)


;; ======================================================================
;; Flycheck
;; ======================================================================

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

(defun my-find-file-hook ()
  "If a file is over a given size, disable expensive things."
  (when (> (buffer-size) (* 1024 50))
    (set (make-variable-buffer-local 'flycheck-mode) nil)))

(add-hook 'find-file-hook 'my-find-file-hook)


;; ===========================================================================
;; MISC UNSORTED
;; ===========================================================================

;;============================================================================
;; "Here's a pretty comprehensive group of magic invocations to make Emacs use
;; UTF-8 everywhere by default"
;; http://stackoverflow.com/questions/2901541/x/2903256#2903256
;;============================================================================
(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;============================================================================
;; Remember where we were.
;;============================================================================

(when we-have-gui?
  ;; Remember state of everything... if we're in a GUI.
  ;; Otherwise I probably don't want that much.
  (desktop-save-mode 1))

;; Save point position between sessions.
;; http://whattheemacsd.com/init.el-03.html
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; Cool function from Dante Catafalmo to align code in columns
;;

(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t)
  (indent-region BEG END))


;;============================================================================
;; What function/class/method are we currently in?

;; Show it at top instead of modeline, as per https://www.emacswiki.org/emacs/WhichFuncMode
;; ... note this is not a straight copy/paste, there were some suggested edits,
;; had to read the whole text and do the suggestions.

(setq mode-line-misc-info (delete (assoc 'which-function-mode
                                      mode-line-misc-info) mode-line-misc-info)
      which-func-header-line-format '(which-function-mode ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-function-mode
    (setq mode-line-misc-info (delete (assoc 'which-function-mode
                                          mode-line-misc-info) mode-line-misc-info)
          header-line-format which-func-header-line-format
          )))

;; Markdown-mode needs imenu enabled in order to work with which-function-mode?
(add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
(setq imenu-auto-rescan t)
;; ... does not seem to help :-(
;; Issue filed & closed here: https://github.com/jrblevin/markdown-mode/issues/765


;;============================================================================
;; OS X specific settings, thanks Will McCutchen & others
;;============================================================================
(when running-on-darwin?
  ;; I used to bind Command to meta but it conflicts with too much
  ;; (from http://www.webweavertech.com/ovidiu/emacs.html)
  ;; ... Trying something new:
  ;; Use left-option OR right-command everywhere! thanks to karabiner
  (setq mac-option-modifier 'meta)
  ;; stop interference from OS X
  (setq mac-pass-control-to-system nil)
  (setq mac-pass-command-to-system nil)
  ;; fix subprocess connections
  (setq process-connection-type nil)

  (global-set-key
   (kbd "S-`") (lambda () (interactive) (other-frame 1)))
)

;; NEVER close frames via Command-W
;; even if I change other bindings
(global-set-key [(super w)] nil)


;; ======================================================================
;; Path
;; ======================================================================

;; fix up exec-path and $PATH used for subprocesses
;; TODO: does "exec-path-from-shell" installed above affect this?
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/bin/py"))
(add-to-list 'exec-path (expand-file-name "~/sh"))
(add-to-list 'exec-path "/nix/var/nix/gcroots/dev-profiles/user-extra-profile/bin/")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/opt/homebrew/sbin")
(setenv "PATH" (mapconcat 'identity exec-path path-separator))



;;============================================================================
;; Modeline tweaks.
;; --------------------------
;; Diminish minor modes in the modeline to leave room for stuff we care about.
;;============================================================================
;; see http://whattheemacsd.com/init.el-04.html

;; XXX Is diminish this fucking up imenu?

;(diminish 'wrap-region-mode)
;(diminish 'yas/minor-mode)

;; (eval-after-load "flymake"
;;   '(diminish 'flymake-mode))

;; Argh you can't diminish major modes.
;; (eval-after-load "python"
;;   '(diminish 'python-mode "Py"))

(eval-after-load "multi-web"
  '(diminish 'multi-web-mode "mw"))

(eval-after-load "auto-complete-mode"
  '(diminish 'auto-complete-mode ""))

(eval-after-load "outline-mode"
  '(diminish 'outline-mode "out"))

(eval-after-load "hi-lock-mode"
  '(diminish 'hi-lock-mode ""))

;; No dice with any variant of this I tried; hacked into the pymacs loading func
;; (eval-after-load "ropemacs-mode"
;;   '(diminish 'ropemacs-mode " R"))
(put 'set-goal-column 'disabled nil)


;; ============================================================================
;; DIRED
;; Doing this last because the subtree stuff seems to break if I do it too early
;; but I'm not sure what it depends on.
;; ============================================================================

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Dired: M-< and M-> should go to first and last file.
;; From http://whattheemacsd.com/setup-dired.el-02.html
(defun dired-back-to-top ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line (if dired-omit-mode 2 4)))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; ===========================================
;; Git / Github
;; ===========================================

(defun git-path ()
  "Find the path to the current file relative to repository root. Like git-link but just the relative path"
  (interactive)
  (require 'git-link)
  (setq filename    (git-link--relative-filename))
  (cond ((null filename)
          (message "Can't figure out what to link to"))
        (t
          (kill-new filename)
          (message filename))
  ))

;; Magit commit messages can get crazy slow esp after rebase. Don't need diff
(remove-hook 'server-switch-hook 'magit-commit-diff)

;; ==============================================
;; Projectile
;; ==============================================

;; Makes helm-projectile usable on giant repos. Do `C-c p i` or `projectile-invalidate-cache` to refresh
(setq projectile-enable-caching t)

;; similar to git-path but based on projectile rather than git repo
(defun projectile-path ()
  "Find the path to the current file relative to projectile root. Like git-path but for projects, not repos"
  (interactive)
  (setq filename (string-remove-prefix (projectile-project-root)
                        (file-truename (buffer-file-name))))
  (kill-new filename)
  (message filename))

;; Projectile speedup over tramp? UNTESTED
;; https://github.com/bbatsov/projectile/issues/1232#issuecomment-536281335
(defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

;; ================================================
;; Tramp
;; ================================================

;; Tramp speedups
(setq vc-handled-backends '(Git))

;; Avoid tramp hang per https://www.emacswiki.org/emacs/TrampMode#h5o-9
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; =====================================
;; UNDO-TREE
;; =====================================
(setq undo-tree-auto-save-history 't)

;; ===================================================
;; Multiple cursors
;; ===================================================

;; Mark continuous lines.
(add-hook 'multiple-cursors-mode-hook
  (lambda ()
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    ;; Or mark a keyword then use one of these to mark others like it.
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
    ;; Use newline to insert newline in multi cursors mode. Use Ctrl-G to exit.
    (define-key mc/keymap (kbd "<return>") nil)))

;; ======================================================
;; String inflection. Useful for change between camel, snake, etc case
;; ======================================================

(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(global-set-key (kbd "C-c i") 'my-string-inflection-cycle-auto)

;; ======================================================================
;; emacsclient

(require 'server)
(unless (server-running-p)
  (server-start))

;; ========================================================
;; Default GUI windows.
;; ========================================================

(when we-have-gui?
  (add-to-list 'initial-frame-alist '(height . 33))
  (add-to-list 'initial-frame-alist '(width . 120))
  (make-frame))
