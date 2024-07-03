;;; init-sessions.el --- Keep track of the recent files, history and sessions. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'files)
; File input and output commands for Emacs
; file:///usr/share/emacs/29.1/lisp/files.el.gz
(setq auto-save-default nil
      make-backup-files nil)

(require 'recentf)
; keep track of recently opened files
; file:///usr/share/emacs/29.1/lisp/recentf.el.gz
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 100
      recentf-exclude `("/tmp/" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

(require 'savehist)
; Save minibuffer history, and restore histories and registers after saving
; file:///usr/share/emacs/29.1/lisp/savehist.el.gz
(add-hook 'after-init-hook 'savehist-mode)

(require 'desktop)
; Save partial status of Emacs when killed
; file:///usr/share/emacs/29.1/lisp/desktop.el.gz
(setq desktop-path (list user-emacs-directory)
      desktop-load-locked-desktop 'check-pid
      desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ivy-history              . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        ))

; Closing emacs results in "Current desktop was not loaded from a file" even though desktop-save-mode was set before start
; https://emacs.stackexchange.com/a/66822
(desktop-change-dir ".")
(desktop-save-mode t)
(desktop-read)


(provide 'init-sessions)
;;; init-sessions.el ends here
