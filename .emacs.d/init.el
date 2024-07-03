;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *use-personal-kbd* t) ;; Enable with t if you prefer, and disable with nil if not


;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config
(require 'init-package)  ; Machinery for installing required packages

;; Load configs for specific features and modes
(require 'init-modes)
(require 'init-view)       ; rainbow
(require 'init-edit)       ; multi-line edit, spell check, git, dir navigation
(require 'init-sessions)   ; recentf, session, desktop
(require 'init-progmodes)  ; Flycheck, auto-complete
(require 'init-org)
(require 'init-utils)
(require 'init-opt)        ; Wakatime, sunshine
(require 'init-gnus)
(require 'init-crypto)

; Personal key-bindings preferences
(when *use-personal-kbd*
  (require 'init-kbd))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
