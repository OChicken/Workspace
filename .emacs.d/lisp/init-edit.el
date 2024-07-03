;;; init-edit.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some basic preferences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; y-or-n instead of yes-or-no
(fset 'yes-or-no-p 'y-or-n-p)

(setq create-lockfiles nil
      mouse-yank-at-point t
      save-interprogram-paste-before-kill t
      set-mark-command-repeat-pop t
      truncate-partial-width-windows nil)

(require 'elec-pair)
; Automatic parenthesis pairing
; file:///usr/share/emacs/29.1/lisp/elec-pair.el.gz
(electric-pair-mode   t)  ; paired parentheses, brackets, and quotes
(electric-indent-mode t)  ; adjust indentation according to the context

(require 'autorevert)
; revert buffers when files on disk change
; file:///usr/share/emacs/29.1/lisp/autorevert.el.gz
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook (lambda()
  (delete-selection-mode   t)
  (transient-mark-mode     t)))

(require 'hideshow)
; minor mode cmds to selectively display code/comment blocks
; file:///usr/share/emacs/29.1/lisp/progmodes/hideshow.el.gz
(add-hook 'prog-mode-hook 'hs-minor-mode)  ; Enable code folding
(setq hs-hide-comments-when-hiding-all nil
      hs-isearch-open t)
(diminish 'hs-minor-mode)

(require 'ido)
; interactively do things with buffers and files
; file:///usr/share/emacs/29.1/lisp/ido.el.gz
(ido-mode t)

(require 'xclip)
; Copy&paste GUI clipboard from text terminal
; https://github.com/emacsmirror/xclip/tree/master
(xclip-mode t)

(require 'whole-line-or-region)
; Operate (cut/copy) on current line if no region is active
; https://github.com/purcell/whole-line-or-region
(add-hook 'after-init-hook 'whole-line-or-region-global-mode)
(with-eval-after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-local-mode))

(require 'multiple-cursors)
; Multiple cursors for emacs
; https://github.com/magnars/multiple-cursors.el
(global-set-key (kbd "C-c C-SPC") 'mc/mark-pop)
(global-set-key (kbd "C-M-l")   'mc/mark-next-like-this)     ; origin: reposition-window
(global-set-key (kbd "C-M-S-l") 'mc/mark-previous-like-this) ; origin: recenter-other-window
(global-set-key (kbd "C-M-w")   'mc/mark-next-like-this-word)      ; origin: append-next-kill
(global-set-key (kbd "C-M-S-w") 'mc/mark-previous-like-this-word)  ; origin: NULL
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(require 'move-dup)
; Moving and duplications of lines or selections with convenient key bindings.
; https://github.com/wyuenho/move-dup
(global-set-key (kbd "M-S-<up>")   'move-dup-move-lines-up)
(global-set-key (kbd "M-S-<down>") 'move-dup-move-lines-down)
; When paredit is enabled (e.g. in org mode), it will use those keybindings
; M-up and M-down. Therefore, you might prefer to use M-S-up and M-S-down,
; which will work even in lisp modes.



;;;;;;;;;;;;;
;; isearch ;;
;;;;;;;;;;;;;

(require 'isearch)
; incremental search minor mode
; file:///usr/share/emacs/29.1/lisp/isearch.el.gz
; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)

(require 'symbol-overlay)
; Highlight symbols with keymap-enabled overlays
; https://github.com/wolray/symbol-overlay
(dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
  (add-hook hook 'symbol-overlay-mode))
(with-eval-after-load 'symbol-overlay
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))
(diminish 'symbol-overlay-mode)

(require 'avy)
; Jump to things in Emacs tree-style
; https://github.com/abo-abJJump to things in Emaco/avy
(global-set-key (kbd "C-:") 'avy-goto-char-timer)

(require 'anzu)
; Displays current match and total matches info in the mode-line
; https://github.com/emacsorphanage/anzu
(global-anzu-mode +1)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)
(set-face-attribute 'anzu-mode-line nil
		    :foreground "white"
		    :weight 'bold)
(diminish 'anzu-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;
;; VC: Version Control ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ediff)
; A comprehensive visual interface to diff & patch
; file:///usr/share/emacs/29.1/lisp/vc/ediff.el.gz
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'magit)
; It's Magit! A Git Porcelain inside Emacs
; https://magit.vc/
(setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))

(require 'git-gutter)
; Emacs port of GitGutter which is Sublime Text Plugin
; https://github.com/emacsorphanage/git-gutter/
(global-git-gutter-mode +1)
; A long lasting bug: git-gutter annotation disappeared during buffer switching.
; https://github.com/emacsorphanage/git-gutter/issues/155
(global-set-key (kbd "C-x v g") 'git-gutter)  ; origin: vc-annotate
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
(diminish 'git-gutter-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer: operate on buffers according to git or projectile ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have two choices when using ibuffer: filter by either .git or project

(require 'ibuffer-vc)
; Let Emacs' ibuffer-mode group files by git project etc., and show file state
; https://github.com/purcell/ibuffer-vc

(require 'ibuffer-projectile)
; Group buffers in Emacs ibuffer-mode by their projectile root directory
; https://github.com/purcell/ibuffer-projectile

;; Set up the preferred filter.
(defun ibuffer-set-up-preferred-filters ()
  "Let ibuffer setup preferred filters.
Use either
  (ibuffer-vc-set-filter-groups-by-vc-root)
or
  (ibuffer-projectile-set-filter-groups)
Feel free to use command to toggle between them."
  (ibuffer-projectile-set-filter-groups)
  ;(ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))
(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(setq ibuffer-show-empty-filter-groups nil)


(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))


;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 14 14 :left :elide)
              " "
              vc-relative-file)
        (mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 14 14 :left :elide)
              " "
              (vc-status 12 12 :left)
              " "
              vc-relative-file)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)
(global-set-key (kbd "C-x C-b") 'ibuffer)  ; origin: list-buffers



;;;;;;;;;;;;;;;;;
;; Spell check ;;
;;;;;;;;;;;;;;;;;

(require 'ispell)
; interface to spell checkers
; file:///usr/share/emacs/29.1/lisp/textmodes/ispell.el.gz
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq ispell-dictionary "en")



;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;

(require 'imenu-list)
; Show the current buffer's imenu entries in a separate buffer
; https://github.com/bmag/imenu-list
(setq imenu-list-size 50)


(provide 'init-edit)
;;; init-edit.el ends here
