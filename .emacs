;; Paul M. Winkler .emacs file

;;============================================================================
;; Initial appearance.
;; Do these early to avoid things jumping in and out on startup.
;; ============================================================================


;; Simple clean interface.
(if (boundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Don't show the GNU splash screen on startup.
(setq inhibit-startup-message t)
;; Cleaner scratch buffer.
(setq initial-scratch-message "**scratch**\n\n")

;;============================================================================
;; Platform information, from mccutchen
;;============================================================================
(defconst windows?
  (eq system-type 'windows-nt))

(defconst linux?
  (or (eq system-type 'gnu/linux)
      (eq system-type 'linux)))

(defconst darwin?
  (eq system-type 'darwin))

(defconst unixp?
  (or linux?
      darwin?
      (eq system-type 'usg-unix-v)
      (eq system-type 'berkeley-unix)))

(defconst gui?
  (or (or window-system)
      nil))

;; Are we running XEmacs or Emacs?
(defconst running-xemacs?
  (string-match "XEmacs\\|Lucid" emacs-version))

;;============================================================================
;; Load path
;;============================================================================

;; local stuff may be loaded from here.

;; ;; Recursive loading, thanks Will McCutchen
;; (let ((default-directory "~/.emacs.d/site-lisp/"))
;;   (setq load-path
;;         (append
;;          (let ((load-path (copy-sequence load-path)))
;;            (normal-top-level-add-subdirs-to-load-path))
;;          load-path))
;;   (add-to-list 'load-path (expand-file-name default-directory)))

;; Try that again, per https://github.com/magnars/.emacs.d/blob/master/init.el
;; Add external projects to load path
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; I like my ~/.emacs.d/ to override, so it goes first.
(setq load-path (cons site-lisp-dir load-path))
;; (add-to-list 'load-path site-lisp-dir)

;; Elpa stuff not being found for some reason.
(add-to-list 'load-path (expand-file-name "elpa" user-emacs-directory))

(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; ========================================================================
;; Package management
;; ========================================================================

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))


;; ========================================================================
;; PYTHON part 1
;; it seems we need to do this early.
;; Load latest python-mode.el first. And remove the
;; built-in python.el.
;; ========================================================================

(when (featurep 'python) (unload-feature 'python t))
;; I would like to just load the latest from ~/.emacs.d but
;; I haven't been able to get it work without forcing the path here.
;; (setq py-install-directory "~/.emacs.d/elpa/python-mode-20170102.523/")
;; (add-to-list 'load-path "~/.emacs.d/elpa/python-mode-20170102.523")
;; (setq py-install-directory "~/.emacs.d/elpa/python-mode-20170106.729/")
;; (add-to-list 'load-path "~/.emacs.d/elpa/python-mode-20170106.729")
(setq py-install-directory "~/.emacs.d/elpa/python-mode-20170108.801/")
(add-to-list 'load-path py-install-directory)

;; Per https://gitlab.com/python-mode-devs/python-mode set this nil??
(setq py-load-pymacs-p nil)


;; ========================================================================
;; FUNCTIONS AND COMMANDS
;; ========================================================================

;; For tips on keybindings, see
;; http://tiny-tools.sourceforge.net/emacs-keys.html

;; For dealing with NON-UNIX TEXT FILES:
;; Here is a handy pair of Lisp functions which Earl Stutes
;; wrote about in Linux Gazette #10.
;; I renamed them to match my shell scripts.
;; Not sure if I got the mac stuff right-- don't have any mac files handy.
;; To invoke e.g. undosify, just do M-x undosify

(defun undosify ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun dosify ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun unmacify ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "\n")))

(defun macify ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r")))

;; From http://www.emacswiki.org/emacs/KeyboardMacrosTricks
(defun save-macro (name)
    "save a macro. Take a name as argument
     and save the last defined macro under 
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro    
     (kmacro-name-last-macro name)         ; use this name for the macro    
     (find-file (user-init-file))                   ; open ~/.emacs or other user init file 
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro 
     (newline)                             ; insert a newline
     (switch-to-buffer nil))               ; return to the initial buffer

;; Only loads the library if it exists
(defun portable-load-library(lib)
  "Only loads the library if it exists"
  (if (locate-library lib)
      (load-library lib)
    ;; (message "portable-load-library: library %s not loaded" lib)
    (message "portable-load-library: Cannot load library %s!" lib)))


;; A function to emulate vim's dw command.
;; XXX have a look at vi-forward-word in emulation/vi.el
;; XXX Not quite there yet.  vi's w & dw move to the next word *beginning*
;; So dw deletes any trailing whitespace after point.
(defun forward-to-next-word-boundary (&optional n)
"Move forward until encountering any word boundary,
up to end of line.
With argument, do this that many times.
XXX argument untested"
 (interactive "p")
 (or n (setq n 1))
 (let ((line-end
	(save-excursion (end-of-line) (point)))
       (started_at (point))
      )
  ;; XXX this only works right if we're not already on a boundary.
  ;; if we are, how do I get it to go to the NEXT boundary?
  ;; and why does it jump when crossing lines?
  (if (re-search-forward  "\\b\\s-*"  line-end nil n)
     (  ;; found a boundary.
        if (eq (point) started_at)
           ;; we were already ON the boundary, force a move.
	   (forward-word 1)
     )
     (forward-line 1) ;; didn't find a boundary on this line, go to next.
  )
 )
)


(defun kill-to-word-boundary (&optional n)
"Kill characters forward until encountering ANY word boundary.
 The idea is to behave like vi's dw command.
 Which it doesn't; that would be forward to word *beginning.*
 Fix that :)
 With argument, do this that many times.
XXX argument untested"
 (interactive "p")
 (or n (setq n 1))
 (kill-region (point) (progn (forward-to-next-word-boundary n) (point)))
)

;; This is the only way i can find to spell shifted letters.
;; otherwise, M-d and M-D are equivalent, bah.
(global-set-key [(meta shift d)] 'kill-to-word-boundary)
(global-set-key [(meta shift f)] 'forward-to-next-word-boundary)

;; Highlight lines with pdb.set_trace
;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

;; handy M-p binding for quick python debugging.
(defun slinkp-pdb-set-trace ()
  "Insert a set_trace() call after the previous line, maintaining indentation"
  (interactive)
  (forward-line -1)
  (end-of-line)
  (insert "\n")
  (indent-according-to-mode)
  (insert "import pdb; pdb.set_trace()")
  (indent-according-to-mode)
;;  (annotate-pdb)
)

;; To bind it globally:
; (global-set-key [(meta p)] 'slinkp-pdb-set-trace)

;; I've got a bit of vi-envy :)
;; I like the vi way of joining lines. Bind that to C-j.
;; i don't care about the default C-j binding of newline-and-indent
;; which is already run on "enter" in every mode I care about.

(defun slinkp-vi-join ()
  "Join the next line to this one, like in vi."
  (interactive)
  ; (forward-line 1)
  (join-line -1)
)

(global-set-key "\C-j" 'slinkp-vi-join)

;; Rename file & current buffer in one step.
;; From http://whattheemacsd.com/file-defuns.el-01.html
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; I don't use "find file read-only", steal its keybinding.
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)


;; Open new line above or below current position, even if in mid-sentence.
;; From http://whattheemacsd.com/editing-defuns.el-01.html
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)


;; Show line numbers only when running goto-line
;; from http://whattheemacsd.com/key-bindings.el-01.html

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (with-no-warnings
          (goto-line (read-number "Goto line: "))))
    (linum-mode -1)))


;; ;; expand-region, really cool selection tools from
;; ;; https://github.com/magnars/expand-region.el
;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)

;; ========================================================================
;; APPEARANCE
;; ========================================================================

; Try to allow scalable fonts under X, see 
; http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_637.html
(setq scalable-fonts-allowed t)

;; Show column number at cursor.
(setq column-number-mode t)
(setq line-number-mode t)

;; Don't beep.
(setq visible-bell t)

;; Don't add new lines when scrolling past end of buffer.
(setq next-line-add-newlines nil)

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs?)
	(global-font-lock-mode t)
	(setq font-lock-maximum-decoration t)
))

;; Show current function in status bar.
;; ... was horribly slow on some .py files before emacs 24.5, now YAYY i can
;; use it again.
;; ... And now horribly slow again as of 24.5.1 and python-mode 6.2.2+ :(
; (which-function-mode t)

;; show me the time
; (display-time)

;; Show selection.
(transient-mark-mode t)

;; Typing when there's a selection causes delete.
;; Also, "DEL" deletes the selection.
(delete-selection-mode t)

(when (load "rainbow-delimiters" t)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
)

;; ========================================================================
;; URLs
;; ========================================================================

(setq browse-url-generic-program "chrome")
(global-set-key [S-mouse-2] 'browse-url-at-mouse)

;; ========================================================================
;; KEYBOARD, MOVEMENT
;; ========================================================================


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
;; NO, this masks all the help keys
;(define-key global-map "\C-h" 'backward-delete-char)


;; no tabs by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Multiple cursors!

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-^") 'mc/mark-all-like-this)

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
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default)))
 '(default-frame-alist (quote ((menu-bar-lines . 1))))
 '(flymake-allowed-file-name-masks
   (quote
    ((".+\\.rake$" flymake-ruby-init)
     ("Rakefile$" flymake-ruby-init)
     (".+\\.rb$" flymake-ruby-init)
     ("\\.xml\\'" flymake-xml-init)
     ("\\.cs\\'" flymake-simple-make-init)
     ("\\.p[ml]\\'" flymake-perl-init)
     ("\\.php[345]?\\'" flymake-php-init)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
     ("\\.tex\\'" flymake-simple-tex-init)
     ("\\.idl\\'" flymake-simple-make-init))))
 '(flymake-compilation-prevents-syntax-check t)
 '(flymake-log-level 0)
 '(flymake-no-changes-timeout 0.75)
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-echo-area-message "pw")
 '(jit-lock-stealth-time 0.035)
 '(markdown-command "pandoc --from markdown_github --to html --standalone")
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-position mode-line-misc-info mode-line-modes
     (vc-mode vc-mode)
     mode-line-end-spaces)))
 '(package-selected-packages
   (quote
    (exec-path-from-shell jedi yaml-mode virtualenvwrapper sphinx-doc smart-mode-line scala-mode2 rainbow-delimiters pyvenv python-mode pymacs php-mode multiple-cursors multi-web-mode markdown-toc markdown-preview-mode magit lorem-ipsum less-css-mode js2-mode jedi-direx idomenu helm-projectile helm-git-grep helm-git handlebars-mode go-mode flymake-python-pyflakes flymake-cursor flycheck-pyflakes find-file-in-repository easy-kill diminish crontab-mode coffee-mode ack-and-a-half)))
 '(protect-buffer-bury-p nil)
 '(py-load-pymacs-p nil)
 '(py-pdbtrack-do-tracking-p t)
 '(safe-local-variable-values
   (quote
    ((test-case-name . buildbot\.test\.test_transfer)
     (test-case-name . buildbot\.test\.test_vc)
     (test-case-name . buildbot\.test\.test_steps\,buildbot\.test\.test_properties)
     (test-case-name . buildbot\.test\.test_run))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(tramp-default-method "ssh")
 '(undo-outer-limit 24000000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#c4deb0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(flymake-errline ((t (:background "#d99" :foreground "black"))))
 '(flymake-warnline ((t (:background "#226"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face))))
 '(font-lock-reference-face ((((class color) (background light)) (:foreground "Yellow"))) t)
 '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:background "#253040" :foreground "#E0B93E"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Goldenrod" :background "DarkSlateBlue"))))
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

;; Whoops! This was double-toggling... font-lock-mode is already loaded by
;; sgml-mode, so loading again for html-mode turns it off...
; (add-hook 'html-mode-hook 'font-lock-mode)

(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html\.raw$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.dtml$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zpt$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pt$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zcml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rdf$" . nxml-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons '("\\.php3$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mako$" . html-mode) auto-mode-alist))

;; flunc/twill stuff... sh mode is decent i guess.
(setq auto-mode-alist (cons '("\\.twill$" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tsuite$" . sh-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.sgml$" . sgml-mode) auto-mode-alist))

;; this is useful with Ian B's svncommit shell function
(setq auto-mode-alist (cons '("svn-commit.tmp" . diff-mode) auto-mode-alist))

;; Io
;; (require 'io-mode)
;; (setq auto-mode-alist (cons '("\\.io$" . io-mode) auto-mode-alist))
;; (add-hook 'io-mode-hook 'font-lock-mode)

;; Handle ANSI colorization in shell mode.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; this should be in my .emacs.d/
(load "django-html-mode.el")

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

;;; ASP & VB highlighting
;(autoload 'visual-basic-mode "visual-basic-mode" nil t)
;(setq auto-mode-alist (cons '("\\.cls$" . visual-basic-mode) auto-mode-alist))

;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(setq auto-mode-alist (cons '("\\.asp$" . html-helper-mode) auto-mode-alist))

;; PHP
(portable-load-library "php-mode-improved")

;; TCL

(add-to-list 'auto-mode-alist '("\\.adp$" . tcl-mode))

;; ========================================================================
;; NXML
;; ========================================================================

;; Set indentation, per Will McCutchen.
(defun nxml-mode-setup()
  (message "NXML hook run")
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t))
(add-hook 'nxml-mode-hook 'nxml-mode-setup)


;; ========================================================================
;; YAML
;; ========================================================================

(add-to-list 'auto-mode-alist '("\\.raml$" . yaml-mode))


;; ========================================================================
;; PYTHON part 2 - main config
;; ========================================================================

(require 'python-mode)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vpy$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cpy$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; No more flymake? I'm using flycheck and flycheck-flake8
(add-hook 'python-mode-hook
  (lambda ()
    (require 'flycheck)
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

(when (load "flymake" t)
    (add-hook 'find-file-hook 'flymake-find-file-hook)

    ;; I found that flymake wasn't recognizing pep8 output.
    ;; ... but this doesn't seem to result in anything being highlighted,
    ;; although if I test it in M-x regex-builder it works fine.
    ;(add-to-list 'flymake-err-line-patterns
    ;	      '("^\\([^:]*\\):\\([0-9]+\\):\\([0-9]+\\): WARNING \\(.*\\)$" 1 2 3 4))

    ;; Installed from melpa,
    ;; ... "makes flymake error messages appear in the minibuffer
    ;; when point is on a line containing a flymake error. This
    ;; saves having to mouse over the error, which is a keyboard
    ;; user's annoyance". See  http://www.emacswiki.org/emacs/flymake-cursor.el
    ; (portable-load-library "flymake-cursor")
    ;; ... hmm, redundant? Seem to have by default already.

    ;; For HTML, use Tidy, don't treat it like XML
    ;; (which is bad for HTML4).
    ;; Thanks to http://www.emacswiki.org/emacs/FlymakeHtml
    (defun flymake-html-init ()
	  (let* ((temp-file (flymake-init-create-temp-buffer-copy
	                     'flymake-create-temp-inplace))
	         (local-file (file-relative-name
	                      temp-file
	                      (file-name-directory buffer-file-name))))
	    (list "tidy" (list local-file))))
    ;; Or actually, flymake on html with embedded js, templating, etc.
    ;; is nearly useless... disable it.
    ;; (add-to-list 'flymake-allowed-file-name-masks
    ;;             '("\\.html$\\|\\.ctp" flymake-html-init))
    (add-to-list 'flymake-err-line-patterns
	             '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
	               nil 1 2 4))
)


;; Keep flymake from throwing an exception if the compile phase passes
;; but the actual checks do not.
;; http://stackoverflow.com/questions/9358086/emacs-flymake-mode-fails-for-coffeescrit
(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtualenvs for python
(require 'virtualenvwrapper)
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
  )
)



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

;; ===================================================================
;; DOCTEST
;; ===================================================================


(autoload 'doctest-mode "doctest-mode" "doctest mode" t)
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))

(fset 'yes-or-no-p 'y-or-n-p) ; stop forcing me to spell out "yes"


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

;; ; Multi-web mode, see https://github.com/fgallina/multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
 (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  ;; (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
;;                   (ruby-mode "<%= " " %>")
))

 (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


;; CSS

(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))

;; ========================================================================
;; RUBY
;; ========================================================================

;; Flymake for ruby, see http://www.emacswiki.org/emacs/FlymakeRuby
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '(".+\\.rake$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns
     )

(add-hook 'ruby-mode-hook
          '(lambda ()

	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))


(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; ===========================================================================
;; HTML / XML
;; ==========================================================================

;; ;; Trying nxhtml-mode for multi-mode html support for rails erb files.
;; ;; ... or not; seems to mess up my python mode somehow??
;; (load "~/.emacs.d/nxhtml/autostart")
;; (add-to-list 'load-path "~/.emacs.d/nxhtml/util/")
;; (require 'mumamo-fun)
;; (setq mumamo-chunk-coloring 'submode-colored)
;; (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))


;; ============================================================================
;; TEMPLATES
;; ============================================================================

(require 'autoinsert)
(auto-insert-mode 1)
(setq auto-insert t)
(setq auto-insert-query nil)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-alist
      '(("\\.\\zcml$" . "template.zcml")
	("\\.\\zpt$" . "template.zpt")
	("\\.pt" . "template.zpt")
	("setup.py" . "setup.py")
	)
      )

;;============================================================================
;; fic mode (highlights TODO FIXME etc in comments)
;;============================================================================
(when (locate-library "fic-mode")
  (require 'fic-mode)
  (add-hook 'prog-mode-hook 'fic-mode))


;;============================================================================
;; markdown mode
;;============================================================================
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; ==================================================================
;; SHELL MODE
;; ==================================================================

;; see http://snarfed.org/why_i_run_shells_inside_emacs
(require 'tramp)



;; You can't really run less inside emacs
(setenv "PAGER" "cat")

(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

(defvar my-shells
  '("*shell*" "*shell0*" "*shell1*" "*shell2*" "*shell3*" "*shell4*"))

(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun my-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
  (when (member (buffer-name) my-shells)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)>" 1 nil))
    (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)


(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((member (buffer-name) my-shells) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
      (progn (fset 'message 'ignore) ad-do-it)
    (fset 'message old-message))))

(defadvice comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (cl-flet ((end-of-line () (goto-char (point-max))))
    ad-do-it))

;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
(load "comint.elc")


;; ===========================================================================
;; MENUS
;; ===========================================================================

;; I like the imenu list of functions, etc. in programming language modes.
;;(autoload 'idomenu "idomenu" nil t)

;; ... broken lately in python-mode? :(
;; I get 'File mode specification error: (user-error "The mode `Py' does not support Imenu")'
; (add-hook 'python-mode-hook 'imenu-add-menubar-index)
(add-hook 'c-mode-hook 'imenu-add-menubar-index)
(add-hook 'ruby-mode-hook 'imenu-add-menubar-index)
(add-hook 'java-mode-hook 'imenu-add-menubar-index)
(add-hook 'js2-mode-hook 'imenu-add-menubar-index)
(add-hook 'js-mode-hook 'imenu-add-menubar-index)
(add-hook 'sh-mode-hook 'imenu-add-menubar-index)
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'lisp-mode-hook 'imenu-add-menubar-index)

;; Similar menu but keyboard-driven, thanks Kevin!
;; https://gist.github.com/kevinbirch/8344414
;; (autoload 'ido-goto "ido-goto" nil t)

(setq auto-mode-alist (cons '("\\.saol$" . c-mode) auto-mode-alist))

;; ===========================================================================
;; MISC UNSORTED
;; ===========================================================================

;; gentoo stuff
;; should be done by the global site-lisp.el
; (load "/usr/share/emacs/site-lisp/site-gentoo")

;; been killing by mistake a lot lately.
(setq confirm-kill-emacs 'yes-or-no-p)

;; dired on cygwin has path problems, grrr.
;; So use lisp version of ls, this should always work.
(load-library "ls-lisp")

;; Auto refresh buffers
(global-auto-revert-mode 1)

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


;; Always end a file with a newline, avoids confusing some tools.
(setq require-final-newline t)

;; Turn on auto-compression so we can read and write .gz files
(auto-compression-mode t)

;; Text mode by default.
(setq major-mode 'text-mode)

;; word wrap when in text mode.
(setq text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 79)
;; I like visual wrapping.
(setq-default truncate-lines nil)

;; I never want to upcase a whole region, so, disable this stupid command.
;; got very tired of doing it by accident.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; How to distinguish files with same name:
;; instead of adding a number, show part of directory path.
;; Options are 'forward, 'post-forward, and 'reverse.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-after-kill-buffer-p t
)

;; etags maybe?
(setq tags-file-name "TAGS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recent files
(load "recentf")
(recentf-mode 1)

;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

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

(when gui?
  ;; Remember state of everything... if we're in a GUI.
  ;; Otherwise I probably don't want that much.
  (desktop-save-mode 1))

;; Save point position between sessions.
;; http://whattheemacsd.com/init.el-03.html
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;;============================================================================
;; OS X specific settings, thanks Will McCutchen
;;============================================================================
(when darwin?
  ;; These seem to fix the command key
  ;; (from http://www.webweavertech.com/ovidiu/emacs.html)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  ;; fix subprocess connections
  (setq process-connection-type nil)
  ;; stop interference from OS X
  (setq mac-pass-command-to-system nil)
  (setq mac-pass-control-to-system nil)
  ;; fixup exec-path and $PATH used for subprocesses
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path (expand-file-name "~/bin"))
  (add-to-list 'exec-path (expand-file-name "~/bin/py"))
  (add-to-list 'exec-path (expand-file-name "~/sh"))
  (setenv "PATH" (mapconcat 'identity exec-path path-separator))

  ;; We may not have slinkp's favorite font.
  ;; TODO: See http://emacswiki.org/emacs/SetFonts about how to detect if a font is installed before setting it.

  (set-frame-font "-outline-menlo-medium-r-normal--12-*-*-*-*-*-iso10646-1")
  ;; And in every new frame. 
  (add-to-list 'default-frame-alist
    ;; '(font . "-apple-Source_Code_Pro-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
    '(font . "-outline-menlo-medium-r-normal--12-*-*-*-*-*-iso10646-1"))

)

;; ========================================================================
;; Rotate fonts, with keybindings. Yay. 
;; Tweaked from http://ergoemacs.org/emacs/emacs_switching_fonts.html
;; ========================================================================

(defun set-font-in-frames (frames fontToUse)
  "Sets font in each frame and redraws it."
  ;; Future frames.
  (set-frame-font fontToUse nil t)
  ;; All current frames.
  (if (not (eq frames nil))
      (progn ;;(message "we got a frame %s" (car frames))
             (set-frame-parameter (car frames) 'font fontToUse)
             ;;(redraw-frame (car frames))  ;; this happens automatically
             (set-font-in-frames (cdr frames) fontToUse)
       )
  )
)

(defun linux-font (points)
  "Make my fave font at the given point size"
  (format "-*-DejaVu Sans Mono-normal-normal-normal-*-%d-*-*-*-m-0-*" points)
)

(defun darwin-font (points)
  (format "-outline-menlo-medium-r-normal--%d-*-*-*-*-*-iso10646-1" points)
)

;; Set the default.
(when (and linux? gui?)
  ;; (set-font-in-frames (visible-frame-list) "-*-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-*")
  (set-font-in-frames (visible-frame-list) (linux-font 14))
)


(when (and darwin? gui?)
  (set-font-in-frames (visible-frame-list) (darwin-font 11))
)

;; TODO automate the copy/paste font name crap
(defun cycle-font (num)
  "Change font in all visible frames.
Each time this is called, font cycles thru a predefined set of fonts.
If NUM is 1, cycle forward.
If NUM is -1, cycle backward."
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (fontList fontToUse currentState nextState )
    (when (and darwin? gui?)
      (setq fontList (list
                      (darwin-font 8)
                      (darwin-font 9)
                      (darwin-font 10)
                      (darwin-font 11)
                      (darwin-font 12)
                      (darwin-font 13)
                      (darwin-font 14)
                      (darwin-font 15)
                      (darwin-font 16)
                      (darwin-font 17)
                      (darwin-font 18)
                      (darwin-font 19)
                      (darwin-font 20)
                      (darwin-font 21)
                      (darwin-font 22)
                      ))
    )
    (when (and linux? gui?)

      (setq fontList (list
                      (linux-font 10)
                      (linux-font 11)
                      (linux-font 12)
                      ;; 13-point saves only verticl space compared to 14
                      (linux-font 13)
                      (linux-font 14)
                      (linux-font 15)
                      (linux-font 16)
                      (linux-font 17)
                      (linux-font 22)
                      ))
    )
    (setq currentState (if (get 'cycle-font 'state) (get 'cycle-font 'state) 0))
    (setq nextState (% (+ currentState (length fontList) num) (length fontList)))

    (setq fontToUse (nth nextState fontList))
    ;; Update all visible frames.
    ;;; We should be able to do something like this:
    ;; (modify-all-frames-parameters (list (cons 'font (list (. fontToUse)))))
    ;; (redraw-display)
    ;;; ... but that gives errors like "invalid font", don't know why.
    ;;; So, here's a homegrown function to do it:
    (set-font-in-frames (visible-frame-list) fontToUse)
    (message "Current font is: %s" fontToUse )
    (put 'cycle-font 'state nextState)
  )
)
(defun cycle-font-forward ()
  "Switch to the next font, in all frames.
See `cycle-font'."
  (interactive)
  (cycle-font 1)
)

(defun cycle-font-backward ()
  "Switch to the previous font, all frames.
See `cycle-font'."
  (interactive)
  (cycle-font -1)
)

;;;; While emacs has something similar built-in...
;; (global-set-key [(meta +)] 'text-scale-adjust )
;; (global-set-key [(meta _)] 'text-scale-adjust )
;;;; ... it only applies to the current buffer, bleh.
;;;; So I'll use mine, at least for now.
(global-set-key [(meta +)] 'cycle-font-forward)
(global-set-key (kbd "<C-+>") 'cycle-font-forward)
(global-set-key [(control +)] 'cycle-font-forward)
(global-set-key [(meta _)] 'cycle-font-backward)


;;============================================================================
;; Modeline tweaks.
;; --------------------------
;; Diminish minor modes in the modeline to leave room for stuff we care about.
;;============================================================================
;; see http://whattheemacsd.com/init.el-04.html

;; XXX Is this fucking up imenu?
; (require 'diminish)
;(diminish 'wrap-region-mode)
;(diminish 'yas/minor-mode)

(eval-after-load "flymake"
  '(diminish 'flymake-mode))

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
)

;; ===========================================================
;; Percolate hacks
;; ===========================================================

;; ;; thanks Nayef Copty
;; (portable-load-library "s")
;; (defun get-github-url ()
;;   "Return the Github url for the current line."
;;   (defvar github-url)
;;   (setq github-url
;;    (s-concat
;;     "https://github.com/percolate/hotlanta/blob/master/"
;;     (s-chop-prefix
;;      "/home/pw/hacking/devolate/hotlanta/" buffer-file-name)
;;     "#L"
;;     (number-to-string (line-number-at-pos)))))
;; (defun copy-hotlanta-github-url ()
;;   "Put hotlanta URL for the line at point in kill ring."
;;   (interactive)
;;   (kill-new (get-github-url))
;;   (message "Copied: %s" (get-github-url)))

;; (global-set-key "\C-x a u" 'copy-hotlanta-github-url)

;; ==========================================================
;; Misc
;; ==========================================================

;; Hate hate hate auto-vscrolling to center of screen.
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)


;; ========================================================
;; Default GUI windows.
;; ========================================================

(when gui?
  (add-to-list 'initial-frame-alist '(height . 29))
  (add-to-list 'initial-frame-alist '(width . 80))
  (make-frame))
