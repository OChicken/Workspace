;;; init-kbd.el --- Personal key-bindings settings -*- lexical-binding: t -*-
;;; Commentary:

;; These settings are tailored to my personal (a little bit left-handed)
;; preference, instead of the Vim-like evil and viper that recommended by many
;; Emacs users.

;; Most kbds mentioned here are modified from the default kbds of Emacs.  If you
;; don't like these kbds, go to init.el and set `*use-personal-kbd*' to nil.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;          Previous/Next Line         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-z") 'next-line)  ; origin: suspend-frame
(global-set-key (kbd "C-q") 'previous-line)  ; origin: quoted-insert


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;    quoted-inset & read-only-mode    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-q") 'quoted-insert)   ; origin: read-only-mode
(global-set-key (kbd "C-x C-r") 'read-only-mode)  ; origin: find-file-read-only
                                                  ; and/or ido-find-file-read-only


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;       Scroll one line up/down       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scroll up/down by one line via C-<up/down> is a VERY frequently used command
;; in Sublime Text.

;; The original "z" kbds C-z and M-z are barely used, so I exploit the potential
;; of the "z" kbds --- bounding them to the scrolling functions, so that I can
;; read a file solely with my left hand.

;; Perhaps "C-z" the suspend-frame, is also a, to some extend, a frequently used
;; command. Actually, suspend-frame bounds to both "C-z" and "C-x C-z", so you
;; can still use the latter one.

(global-set-key (kbd "C-<down>") 'scroll-up-line)    ; origin: forward paragraph
(global-set-key (kbd "C-S-n"   ) 'scroll-up-line)
(global-set-key (kbd "C-<up>"  ) 'scroll-down-line)  ; origin: backward paragraph
(global-set-key (kbd "C-S-p"   ) 'scroll-down-line)

;; "z" preference
(defun scroll-up-line-keep-cur ()
  "Scroll text next one line."
  (interactive)
  (progn
    (scroll-up-line)
    (forward-line -1)))
(defun scroll-down-line-keep-cur ()
  "Scroll text previous one line."
  (interactive)
  (progn
    (scroll-down-line)
    (forward-line 1)))
(global-set-key (kbd "C-M-z") 'scroll-up-line-keep-cur)    ; origin: suspend-frame
(global-set-key (kbd "C-M-q") 'scroll-down-line-keep-cur)  ; origin: quoted-insert


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;         recenter-top-bottom-hl      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recenter-top-bottom-hl (&optional arg)
  "Call `recenter-top-bottom' in the current window and highlight pulse it.
With numeric prefix ARG, move current line to window-line ARG."
  (interactive "P")
  (recenter-top-bottom arg)
  (pulse-momentary-highlight-one-line (point)))

(global-set-key (kbd "C-l") 'recenter-top-bottom-hl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;      Scroll half screen up/down     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The default "C-v" and "M-v" are bounded to scroll-up/down-command, which are
;; equivalent to <Page Down> and <Page Up> respectively. They scroll the full
;; screen, which are too much for me, so I set these two functions to scroll
;; half screen. However, I still don't think that scrolling the screen is a nice
;; way to read the file IN DETAILED, I would prefer the scroll one line up/down
;; commands.

;; scroll-up-half
(defun scroll-up-half ()
  "Scroll text of selected window upward for half window."
  (interactive)
  (progn
    (move-to-window-line -1)
    (recenter)))
(global-set-key (kbd "C-x C-v") 'scroll-up-half) ; origin: ido-find-alternative-file

;; scroll-down-half
(defun scroll-down-half ()
  "Scroll text of selected window down for half window."
  (interactive)
  (progn
    (move-to-window-line 0)
    (recenter)))
(global-set-key (kbd "C-x C-S-v") 'scroll-down-half)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;          Copy & paste/yank          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nobody would resist the sweet C-v paste/yank, especially when the original
;; C-v scrolls the full screen to distract your attention.
(global-set-key (kbd "C-v") 'yank)

; Mark the symbol at point.
(defun mark-symbol-at-point ()
  "Copy the symbol at point."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (when sym
        (forward-symbol -1)
        (mark-sexp)
        (message "Mark set"))))
(global-set-key (kbd "C-x C-M-SPC") 'mark-symbol-at-point)

; Copy the word at point.
(defun copy-word-at-point ()
  "Copy the word at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
        (kill-new word)
        (message "Copied '%s' to kill ring" word))))
(global-set-key (kbd "C-x M-w") 'copy-word-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;           Left-hand 'RET'           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; M-e originally maps to forward-sentence, but I am barely use it. Instead, I
; want a <return> for left-hand, so M-e, where `e' for `enter', is a nice choice.
(global-unset-key (kbd "M-e"))  ; origin: forward-sentence
(global-set-key (kbd "M-e") (kbd "RET"))
(global-set-key (kbd "M-E") (kbd "C-o"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					; Left-hand '{previous/next}-command' ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; During compiling, it's so frequently to do the following steps:
;; 1. M-! (shell-command, revoke minibuffer-local-shell-command-map)
;; 2. M-p (do previous-history-element in the above major mode)
;; 3. RET (hit <return>)
;; So I want to map M-q as what M-p does, and M-e as <return>.

; M-q maps to fill-paragraph in global-map. Map it to previous-history-element
; in minibuffer-local-completion-map is of course OK.
(define-key minibuffer-local-map (kbd "M-q") 'previous-history-element)
(define-key minibuffer-local-map (kbd "M-z") 'next-history-element)

(require 'em-hist)
; history list management
; file:///usr/share/emacs/29.1/lisp/eshell/em-hist.el.gz
(define-key eshell-hist-mode-map (kbd "M-q") 'eshell-previous-matching-input-from-input)
(define-key eshell-hist-mode-map (kbd "M-z") 'eshell-next-matching-input-from-input)

(require 'isearch)
; incremental search minor mode
; file:///usr/share/emacs/29.3/lisp/isearch.el.gz
(define-key isearch-mode-map (kbd "M-q") 'isearch-ring-retreat)

(require 'comint)
; general command interpreter in a window stuff
; file:///usr/share/emacs/29.3/lisp/comint.el.gz
(define-key comint-mode-map (kbd "M-q") 'comint-previous-input)
(define-key comint-mode-map (kbd "M-z") 'comint-next-input)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;          Beginning of line          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exchange the "beginning of the line" behaviour: the very beginning of the
;; visual line, and the beginning of the line with indentation. Remark: M-m
;; also bounds to back-to-indentation.

(global-set-key (kbd "C-a") 'back-to-indentation)       ; origin: beginning-of-visual-line
(global-set-key (kbd "M-a") 'beginning-of-visual-line)  ; origin: backward-sentence


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;             delete word             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "C-<backspace>") 'backward-delete-word)  ; origin: backward-kill-word
(global-set-key (kbd "C-<delete>")             'delete-word)  ; origin:          kill-word


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;        Togging among windows        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Toggling among windows. This modifies the default "C-<tab>" and "C-S-<tab>"
;; behaviour, that toggling among tabs. In practice, toggling among tabs is
;; barely used. You can still use tab-next command via "C-x t o" or simply mouse
;; click.

;; In magit-status-mode, the <tab> is used to toggle the modification of each
;; file and hunk (magit-section-toggle), which makes "C-<tab>" not able to
;; escape from the magit-status window. So I use "C-x C-<tab>" instead.

(global-set-key (kbd "C-<tab>") 'next-window-any-frame)                    ; origin: tab-next
(if window-system
    (global-set-key (kbd "C-S-<iso-lefttab>") 'previous-window-any-frame)  ; origin: tab-previous
  (global-set-key (kbd "C-S-<tab>") 'previous-window-any-frame))           ; origin: tab-previous

(defun magit-status-toggle-window ()
  "The kbds enable you to escape from the matig-status window to the next window."
  (global-set-key (kbd "C-x C-<tab>")           'next-window-any-frame)          ; origin: undefined
  (if window-system
      (global-set-key (kbd "C-x C-S-<iso-lefttab>") 'previous-window-any-frame)  ; origin: undefined
    (global-set-key (kbd "C-x C-S-<tab>") 'previous-window-any-frame)))          ; origin: undefined
(add-hook 'magit-status-mode-hook 'magit-status-toggle-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;         Toggling among tabs         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-<prior>") 'tab-bar-switch-to-prev-tab)  ; origin: scroll-right
(global-set-key (kbd "C-<next>")  'tab-bar-switch-to-next-tab)  ; origin: scroll-left


(provide 'init-kbd)
;;; init-kbd.el ends here
