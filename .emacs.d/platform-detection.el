;;============================================================================
;; Platform information, from mccutchen
;;============================================================================
(defconst running-on-windows?
  (eq system-type 'windows-nt))

(defconst running-on-linux?
  (or (eq system-type 'gnu/linux)
      (eq system-type 'linux)))

(defconst running-on-darwin?
  (eq system-type 'darwin))

(defconst running-on-spin?
  (and running-on-linux? (equal (getenv "SPIN") "1")))

(defconst running-on-unix?
  (or running-on-linux?
      running-on-darwin?
      (eq system-type 'usg-unix-v)
      (eq system-type 'berkeley-unix)))

(defconst we-have-gui?
  (or (or window-system)
      nil))

;; Are we running XEmacs or Emacs?
(defconst running-xemacs?
  (string-match "XEmacs\\|Lucid" emacs-version))
