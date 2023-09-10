;; ========================================================================
;; FUNCTIONS AND COMMANDS
;; ========================================================================

;; For tips on keybindings, see
;; http://tiny-tools.sourceforge.net/emacs-keys.html

;; For dealing with NON-UNIX TEXT FILES:
;; Here is a handy pair of Lisp functions which Earl Stutes
;; wrote about in Linux Gazette #10.
;; I renamed them to match my shell scripts.
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
