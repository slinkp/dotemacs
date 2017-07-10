(when (load "flymake" t)
         (defun flymake-flake8-init ()
           (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-with-folder-structure))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
             ; If we're in a virtualenv, use the version of flake8
             ; there (along with any plugins). Otherwise, assume we
             ; have a special copy installed globally.
             (list (if (getenv "VIRTUAL_ENV")
               (concat (getenv "VIRTUAL_ENV") "/bin/flake8")
             (concat (getenv "HOME") "/.virtualenvs/emacs_tools/bin/flake8")
             )
           (list local-file))))

         (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-flake8-init)))

   (add-hook 'find-file-hook 'flymake-find-file-hook)

;; Keep flymake from throwing an exception if the compile phase passes
;; but the actual checks do not.
;; http://stackoverflow.com/questions/9358086/emacs-flymake-mode-fails-for-coffeescrit
(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)
