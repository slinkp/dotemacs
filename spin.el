;;; spin.el --- Shopify spin pin integration  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Shopify

;; Author: Dante Catalfamo

;;; Commentary:

;; Shopify `spin' integration

;;; Code:
(require 'async)
(require 'json)
(require 'seq)

(when async-in-child-emacs ; Spoof these functions if called async
  (defmacro transient-define-prefix (&rest _ignored) nil)
  (defmacro transient-args (&rest _ignored) nil)
  (defmacro transient-arg-value (&rest _ignored) nil))

(when (not async-in-child-emacs)
  (require 'transient))


(defvar spin-executable "spin"
  "Path to spin executable.")

(defvar spin-token-location "~/.spin/token.spin.up.dev.json"
  "Path to the spin auth token.")

(defvar spin-vpn-name "Spin"
  "The name of the spin VPN profile.")

(defvar spin-status-buffer "*Spin Status*"
  "Name of the spin-status buffer.")

(defconst spin--async-process
  `(lambda ()
     (load-file ,(or load-file-name buffer-file-name))
     (spin--table-format)))

(defun spin--vpn-status ()
  "Return the status of the spin VPN.
Returns a plist."
  (let (vpn-lines vpn-line vpn-split vpn-status vpn-uuid)
    (setq vpn-lines (process-lines "scutil" "--nc" "list"))
    (setq vpn-line (car (seq-filter (apply-partially #'string-match-p spin-vpn-name) vpn-lines)))
    (unless vpn-line
      (error "VPN profile %s not installed" spin-vpn-name))
    (setq vpn-split (split-string vpn-line))
    (setq vpn-status (string-trim (cadr vpn-split) "(" ")"))
    (setq vpn-uuid (caddr vpn-split))
    (list :uuid vpn-uuid :status vpn-status)))

(defun spin--decode-token ()
  "Decode the spin auth token.
Returns an alist."
  (let* ((jwt (json-read-file spin-token-location))
         (jwt-access (split-string (alist-get 'access_token jwt) "\\."))
         (jwt-data (cadr jwt-access))
         (jwt-json (base64-decode-string jwt-data t))
         (jwt-alist (json-read-from-string jwt-json)))
    jwt-alist))


(defun spin--parse-workspace (workspace)
  "Parse WORKSPACE into lines for display."
  (let ((workspace (alist-get 'Name workspace))
        (suspended (not (eq (alist-get 'Suspended workspace) :json-false)))
        (primary-service (alist-get 'PrimaryService workspace))
        (services (alist-get 'Services workspace))
        services-out)
    (dolist (service services services-out)
      (push (list :workspace workspace
                  :suspended suspended
                  :primary-service primary-service
                  :name (alist-get 'Name service)
                  :domain (alist-get 'FQDN service)
                  :repository (alist-get 'Repository service)
                  :branch (alist-get 'MergeBase service)
                  :created (alist-get 'CreatedAt service)
                  :clone-only (not (eq (alist-get 'CloneOnly service) :json-false))
                  :status (alist-get 'Status service)
                  :state (alist-get 'State (alist-get 'Status service)))
            services-out))))


(defun spin--parse-list ()
  "Return list of spin VMs."
  (apply #'nconc (seq-map #'spin--parse-workspace
                          (json-read-from-string (shell-command-to-string "spin list -o json")))))


(defun spin-status--revert-buffer (_autoignore _noconfirm)
  "Revert `spin-status' buffer."
  (setq revert-buffer-in-progress-p t)
  (setq mode-line-process ":refreshing")
  (async-start
   spin--async-process
   (lambda (result)
     (setq async-result result)
     (with-current-buffer spin-status-buffer
       (setq tabulated-list-entries result)
       (tabulated-list-revert)
       (setq revert-buffer-in-progress-p nil)
       (setq mode-line-process "")))))


(defun spin--table-format ()
  "Format spin status for `spin-status-mode' table."
  (let (rows
        (lines (spin--parse-list)))
    (dolist (line lines rows)
      (let ((tramp-location (format "/ssh:%s:" (plist-get line :domain)))
            (browser-url (format "http://%s" (plist-get line :domain))))
        (push (list (plist-get line :workspace)
                    (vector
                     `("Connect" action
                       (lambda (_button)
                         (let ((default-directory ,tramp-location))
                           (message "Connecting using TRAMP...")
                           (dired ".")
                           (message "Connected!"))))
                     `("Open" action
                       (lambda (_button)
                         (browse-url-default-browser ,browser-url)))
                     (plist-get line :workspace)
                     (plist-get line :name)
                     (if (string= (plist-get line :primary-service) (plist-get line :name)) "✔" "")
                     (plist-get line :repository)
                     (plist-get line :branch)
                     (if (plist-get line :clone-only) "Yes" "No")
                     (plist-get line :created)
                     (plist-get line :state)
                     (if (plist-get line :suspended) "Yes" "No")
                     (plist-get line :domain)))
              rows)))))


(transient-define-prefix spin-transient-create ()
  "Spin create transient menu."
  ["Arguments"
   ("-n" "Name" ("-n" "--name="))
   ("-t" "Timeout (default 40)" ("-t" "--timeout="))]
  ["Actions"
   ("c" "Spin create" spin--create)])

(transient-define-prefix spin-transient-try ()
  "Spin try transient menu."
  ["Arguments"
   ("-n" "Name" ("-n" "--name="))
   ("-t" "Timeout (default 40)" ("-t" "--timeout="))]
  ["Actions"
   ("t" "Spin try" spin--try)])

(transient-define-prefix spin-transient-destroy ()
  "Spin trantient destroy menu."
  ["Actions"
   ("a" "All" spin--destroy-all)
   ("d" "Workspace" spin--destroy)])

(transient-define-prefix spin-transient-vpn ()
  "Spin transneit vpn menu."
  ["Actions"
   ("u" "Start" spin--vpn-start)
   ("d" "Stop" spin--vpn-stop)
   ("s" "Status" spin--vpn-check)])

(transient-define-prefix spin-transient ()
  "Transient keymap for `spin-status'."
  ["Actions"
   ("c" "Create" spin-transient-create)
   ("d" "Destroy" spin-transient-destroy)
   ("t" "Try" spin-transient-try)
   ("v" "VPN" spin-transient-vpn)])

(defun spin--create (repo &optional args)
  "Spin create a REPO workspace using ARGS arguments."
  (interactive
   (list (read-string "Repo Name: ")
         (transient-args 'spin-transient-create)))
  (when (string= (plist-get (spin--vpn-status) :status) "Disconnected")
    (user-error "VPN not running! Aborting"))
  (let ((name (transient-arg-value "--name=" args))
        (timeout (transient-arg-value "--timeout=" args)))
    (when (string= repo "")
      (user-error "Invalid repo name"))

    (setq mode-line-process ":creating")
    (apply
     #'async-start-process
     (format "spin create %s" repo)
     spin-executable
     (lambda (_proc)
       (message "New %s workspace created!" repo)
       (setq mode-line-process "")
       (with-current-buffer spin-status-buffer
         (revert-buffer)))
     "create"
     repo
     args)))

(defun spin--try (repo &optional args)
  "Spin try a REPO workspace using ARGS arguments."
  (interactive
   (list (read-string "Repo Name: ")
         (transient-args 'spin-transient-try)))
  (when (string= (plist-get (spin--vpn-status) :status) "Disconnected")
    (user-error "VPN not running! Aborting"))
  (let ((name (transient-arg-value "--name=" args))
        (timeout (transient-arg-value "--timeout=" args)))
    (when (string= repo "")
      (user-error "Invalid repo name"))

    (setq mode-line-process ":creating")
    (apply
     #'async-start-process
     (format "spin try %s" repo)
     spin-executable
     (lambda (_proc)
       (message "New %s workspace created!" repo)
       (setq mode-line-process "")
       (with-current-buffer spin-status-buffer
         (revert-buffer)))
     "try"
     repo
     args)))

(defun spin--destroy-all (allow &optional args)
  "Spin destroy all workspaces.
ALLOW must be t. ARGS."
  (interactive
   (list
    (yes-or-no-p "Are you sure you want to destroy all workspaces? ")
    (transient-args 'spin-transient-destroy)))
  (unless allow
    (user-error "Aborted"))

  (message "Destroying all workspaces...")
  (setq mode-line-process ":destroying")
  (async-start-process
   "spin destroy all"
   spin-executable
   (lambda (_proc)
     (message "All workspaces destroyed!")
     (with-current-buffer spin-status-buffer
       (setq mode-line-process "")
       (revert-buffer))
     "destroy"
     "--all")))

(defun spin--destroy (allow &optional args)
  "Spin destroy workspace under cursor if ALLOW with ARGS."
  (interactive
   (list
    (yes-or-no-p "Are you sure you want to destroy this workspace? ")
    (transient-args 'spin-transient-destroy)))
  (unless allow
    (user-error "Aborted"))
  (unless (tabulated-list-get-entry)
    (user-error "Cursor not on a valid entry"))

  (let* ((entry (tabulated-list-get-entry))
         (workspace (elt entry 2)))

    (message "Destroying workspace %s..." workspace)
    (setq mode-line-process ":destroying")
    (async-start-process
     (format "spin destroy %s" workspace)
     spin-executable
     (lambda (_proc)
       (message "Workspace destroyed!")
       (with-current-buffer spin-status-buffer
         (setq mode-line-process "")
         (revert-buffer)))
     "destroy"
     workspace)))

(defun spin--vpn-start (&optional args)
  "Spin VPN stop with ARGS."
  (interactive
   (list
    (transient-args 'spin-transient-vpn)))
  (message "Starting VPN...")
  (setq mode-line-process ":vpn-start")
  (async-start-process
   "spin vpn start"
   "scutil"
   (lambda (_proc)
     (message "VPN started!")
     (with-current-buffer spin-status-buffer
       (setq mode-line-process "")))
   "--nc"
   "start"
   (plist-get (spin--vpn-status) :uuid)))

(defun spin--vpn-stop (&optional args)
  "Spin VPN stop with ARGS."
  (interactive
   (list
    (transient-args 'spin-transient-vpn)))
  (message "Stopping VPN...")
  (setq mode-line-process ":vpn-stop")
  (async-start-process
   "spin vpn stop"
   "scutil"
   (lambda (_proc)
     (message "VPN stopped!")
     (with-current-buffer spin-status-buffer
       (setq mode-line-process "")))
   "--nc"
   "stop"
   (plist-get (spin--vpn-status) :uuid)))

(defun spin--vpn-check (&optional args)
  "Spin VPN check status with ARGS."
  (interactive (list (transient-args 'spin-transient-vpn)))
  (let ((status (spin--vpn-status)))
    (message "%s" (plist-get status :status))))

(defvar spin-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" #'spin-transient)
    (define-key map "c" #'spin-transient-create)
    (define-key map "d" #'spin-transient-destroy)
    (define-key map "v" #'spin-transient-vpn)
    map))

(define-derived-mode spin-status-mode tabulated-list-mode "spin-status"
  "Spin ststus mode, show the status of spin VMs."
  (setq tabulated-list-format [("TRAMP" 8 nil)
                               ("Browser" 8 nil)
                               ("Workspace" 9 nil)
                               ("Service" 20 nil)
                               ("Primary" 7 nil)
                               ("Repository" 50 nil)
                               ("Branch" 10 nil)
                               ("Clone-Only" 10 nil)
                               ("Created" 20 nil)
                               ("State" 10 nil)
                               ("Suspended" 9 nil)
                               ("Domain" 30 nil)])
  (setq tabulated-list-sort-key '("Workspace" . nil))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (setq revert-buffer-function #'spin-status--revert-buffer))


;;;###autoload
(defun spin-status ()
  "List spin VM status."
  (interactive)
  (switch-to-buffer "*Spin Status*")
  (spin-status-mode)
  (revert-buffer))


(provide 'spin)
;;; spin.el ends here
