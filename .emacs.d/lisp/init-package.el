;;; init-package.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq-local package-list '(
  ; opt
  wakatime-mode sunshine
  ; utils
  which-key list-unicode-display
  ; org
  ob-go ob-php ob-sagemath
  org-fragtog xenops org-ref
  gnuplot
  imenu-list rainbow-mode
  yaml yaml-mode
  rust-mode sage-shell-mode gnuplot-mode
  ; progmodes
  flycheck flycheck-clang-tidy flycheck-rust
  yasnippet-snippets yasnippet
  company-math company-c-headers company-auctex company
  projectile ibuffer-projectile
  auctex
  ; edit
  magit git-gutter ibuffer-vc
  anzu
  move-dup xclip multiple-cursors symbol-overlay whole-line-or-region
  wrap-region
  ; lang
  cmake-mode crontab-mode go-mode magma-mode markdown-mode php-mode web-mode
  ; view
  htmlize
  highlight-escape-sequences
  vertico rainbow-delimiters mode-line-bell page-break-lines diminish
  vscode-dark-plus-theme
  ; keyring
  gnu-elpa-keyring-update))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-package)
;;; init-package.el ends here
