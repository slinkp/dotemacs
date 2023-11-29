;; Paul M. Winkler .emacs file

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
 '("setup-load-path"
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
   "cclisp"
   "cc-edit-text-menu"
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

;; Enable cc*el edit menus as per http://yummymelon.com/devnull/moving-text-elegantly-in-emacs.html
(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    cc/transpose-menu "Fill")

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    cc/move-text-menu "Fill")
