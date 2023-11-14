;;; init-view.el --- View settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common view settings (of both TTY frames & GUI frames) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Attention: the variables re-defined here are all defined in 'C source code'
;; of Emacs, so that you must use 'setq-default' instead of 'setq'.

;; display line numbers
(setq-default display-line-numbers-width 4)
(add-hook 'find-file-hook
          (lambda ()
            (unless (memq major-mode '(doc-view-mode image-mode))
              (display-line-numbers-mode t))))

;; display fill column indicator on column 80
(setq-default indicate-buffer-boundaries 'left
              display-fill-column-indicator-character ?\u254e
              fill-column 80)
(add-hook 'find-file-hook 'display-fill-column-indicator-mode)

;; display tab bar
(tab-bar-mode t)

;; disable the tool bar (It's very ugly and provides limited functionalities)
(tool-bar-mode -1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xterm: Integrate with terminals such as xterm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(xterm-mouse-mode t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gui-frame: Behaviour specific to non-TTY frames ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suppress GUI features
; C source code
(setq use-file-dialog nil
      use-dialog-box nil)
(setq-default cursor-type 'bar)

;; Window size and features
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Change global font size
(set-face-attribute 'default nil
                    :family  "Hack"
                    :foundry "SRC"
                    :slant   'normal
                    :weight  'regular
                    :height  68     ; (Hack 7: height 68; Hack 8: height 83)
                    :width   'normal)

(require 'frame)
; multi-frame management independent of window systems
; file:///usr/share/emacs/29.1/lisp/frame.el.gz
(blink-cursor-mode t)



;;;;;;;;;;;;;;;
;; Scrolling ;;
;;;;;;;;;;;;;;;

; C source code
(setq scroll-preserve-screen-position 'always)

(require 'scroll-bar)
; window system-independent scroll bar support
; file:///usr/share/emacs/29.1/lisp/scroll-bar.el.gz
(set-scroll-bar-mode 'left)

(require 'mwheel)
; Mouse wheel support
; file:///usr/share/emacs/29.1/lisp/mwheel.el.gz
(setq mouse-wheel-scroll-amount
      '(3 ((shift)  . hscroll)
	  ((meta))
	  ((control meta) . global-text-scale)
	  ((control) . text-scale))
      mouse-wheel-progressive-speed nil)  ; don"t accelerate scrolling

(require 'pixel-scroll)
; Scroll a line smoothly
; file:///usr/share/emacs/29.1/lisp/pixel-scroll.el.gz
;(pixel-scroll-mode nil)
; you can enable smooth scrolling, but I don't think it's useful



;;;;;;;;;;;;;;;;
;; load theme ;;
;;;;;;;;;;;;;;;;

(load-theme 'vscode-dark-plus t)



;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elements in a frame ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'simple)
; basic editing commands for Emacs
; file:///usr/share/emacs/29.1/lisp/simple.el.gz
(global-visual-line-mode t)
(column-number-mode t)
(diminish 'visual-line-mode)

(require 'hl-line)
; highlight the current line
; file:///usr/share/emacs/29.1/lisp/hl-line.el.gz
(global-hl-line-mode t)

(require 'rainbow-delimiters)
; Fanael/rainbow-delimiters: Emacs rainbow delimiters mode
; A "rainbow parentheses"-like mode which highlights delimiters such as
; parentheses, brackets or braces according to their depth.
; https://github.com/Fanael/rainbow-delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'mode-line-bell)
; purcell/mode-line-bell: Flash the Emacs mode line instead of ringing the bell
; Flash the Emacs mode line instead of ringing the bell
; https://github.com/purcell/mode-line-bell
(add-hook 'after-init-hook 'mode-line-bell-mode)

(require 'page-break-lines)
; Display ugly ^L page breaks as tidy horizontal lines
; https://github.com/purcell/page-break-lines
(add-hook 'git-gutter-mode-hook 'page-break-lines-mode)
(diminish 'page-break-lines-mode)

(require 'highlight-escape-sequences)
; Highlight escape sequences in Emacs
; https://github.com/dgutov/highlight-escape-sequences
(add-hook 'after-init-hook 'hes-mode)
(put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
(put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)

(require 'vertico)
; VERTical Interactive COmpletion
; https://github.com/minad/vertico
(add-hook 'after-init-hook 'vertico-mode)
(global-set-key (kbd "C-x C-M-v") 'vertico-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure uniquification of buffer names ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify)
; unique buffer names dependent on file name
; file:///usr/share/emacs/29.1/lisp/uniquify.el.gz
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "@"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;              Dired mode             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)
; directory-browsing commands
; file:///usr/share/emacs/29.1/lisp/dired.el.gz
(setq dired-listing-switches "-alFo --time-style=long-iso")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax (auto-mode): auto major mode for file name pattern (extensions) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'init-view)
;;; init-view.el ends here
