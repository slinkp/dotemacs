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
 '("early-init"
   "setup-native-comp"
   "package-install"
   "platform-detection"
   "setup-misc-behavior"
   "initial-gui"
   "setup-keyboard"
   "setup-misc-functions"
   "setup-font-defaults-and-switching"
   "misc-language-modes"
   "setup-misc-modes"
   "setup-python"
   "setup-misc-unorganized-junk"
   ;; ... add more files here
   ))

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



;; ===========================================================================
;; MISC UNSORTED
;; ===========================================================================



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
