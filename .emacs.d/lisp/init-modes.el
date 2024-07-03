;;; init-modes.el --- Major modes settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.emacs\\.grep\\'"   . grep-mode))
(add-to-list 'auto-mode-alist '("\\.emacs\\.eshell\\'" . eshell-mode))

(require 'make-mode)
; makefile editing commands for Emacs
; file:///usr/share/emacs/29.1/lisp/progmodes/make-mode.el.gz
(add-to-list 'auto-mode-alist '("\\Makefile\\(?:\\..*\\)\\'" . makefile-mode))

(require 'sh-script)
; Shell-script editing commands for Emacs
; file:///usr/share/emacs/29.1/lisp/progmodes/sh-script.el.gz
(add-to-list 'auto-mode-alist '("\\.bash\\(rc\\|_\\w+\\)\\'" . sh-mode))

(require 'crontab-mode)
; crontab-mode - MELPA
; https://melpa.org/#/crontab-mode
(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))

(require 'htmlize)

(require 'markdown-mode)
; Markdown Mode for Emacs
; https://jblevins.org/projects/markdown-mode/
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
  '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(require 'css-mode)
; Major mode to edit CSS files
; file:///usr/share/emacs/29.1/lisp/textmodes/css-mode.el.gz
(add-hook 'css-mode-hook 'rainbow-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . css-mode))
(with-eval-after-load 'rainbow-mode
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

(require 'web-mode)
; web-mode.el - html template editing for emacs
; https://web-mode.org/
(add-to-list 'html-mode-hook 'web-mode)

(provide 'init-modes)
;;; init-modes.el ends here
