;;; init-progmodes.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'grep)
; run `grep' and display the results
; file:///usr/share/emacs/29.1/lisp/progmodes/grep.el.gz
(setq grep-use-null-device nil
      grep-command "grep --color=auto -nr -F --exclude-dir=.git --exclude-dir=fs --exclude-dir=drivers --exclude-dir=Documentation --exclude-dir=net --exclude=TAGS --exclude=.emacs.desktop ")

(require 'em-unix)
; UNIX command aliases
; file:///usr/share/emacs/29.3/lisp/eshell/em-unix.el.gz
(setq eshell-plain-grep-behavior t)
; https://emacs.stackexchange.com/questions/57714/how-to-keep-grep-results-in-eshell-buffer


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Projectile             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'projectile)
; Project Interaction Library for Emacs
; https://github.com/bbatsov/projectile
(projectile-mode +1)
; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
; A list of files considered to mark the root of a project
(dolist (file '(".gitignore" ".dir-locals.el" "compile_commands.json"))
  (add-to-list 'projectile-project-root-files-bottom-up file t))
(diminish 'projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;   Flycheck syntax checker settings  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flycheck)
; Flycheck --- Syntax checking for GNU Emacs --- Flycheck 33-cvs documentation
; https://www.flycheck.org/en/latest/
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'flycheck-mode))
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(setq flycheck-clang-include-path
      (list
       (expand-file-name "~/.local/include/")
       "/usr/share/verilator/include/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                 Xref                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'xref)
; Cross-referencing commands
; file:///usr/share/emacs/29.1/lisp/progmodes/xref.el.gz
(setq tags-table-list '("~/.emacs.d/ctags/TAGS"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;              Completion             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(require 'yasnippet-snippets)

(require 'company)
; company-mode for Emacs
; https://company-mode.github.io/
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-align-annotations t
      company-idle-delay 0.0
      company-show-quick-access t  ; Use M-1、M-2 to choose
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence))
(diminish 'company-mode)

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;; company-math
(require 'company-math)
(add-to-list 'company-backends 'company-math-symbols-latex)
(add-to-list 'company-backends 'company-math-symbols-unicode)

(require 'company-auctex)
; company-mode autocompletion for auctex
; https://github.com/alexeyr/company-auctex
(company-auctex-init)

(require 'company-org-block)
; xenodium/company-org-block
; https://github.com/xenodium/company-org-block
(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-org-block)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;             wrap-region             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'wrap-region)
; Wrap text with punctation or tag
; https://github.com/rejeep/wrap-region.el
(wrap-region-global-mode t)
(wrap-region-add-wrappers
 '(("\(" "\)" nil c-mode)
   ("\"" "\"" nil c-mode)
   ("*" "*" nil org-mode)
   ("/" "/" nil org-mode)
   ("~" "~" nil org-mode)
   ("=" "=" nil org-mode)
   ("+" "+" nil org-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                Eglot                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eglot)
; The Emacs Client for LSP servers
; file:///usr/share/emacs/29.1/lisp/progmodes/eglot.el.gz
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;          Emacs Lisp config          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-scratch-message
      (concat initial-scratch-message
       ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(add-hook 'lisp-interaction-mode-hook 'display-line-numbers-mode)

(require 'immortal-scratch)
; Respawn the scratch buffer when it's killed
; https://github.com/jpkotta/immortal-scratch
(add-hook 'after-init-hook 'immortal-scratch-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;             C/C++ config            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cc-vars)
; User customization variables for CC Mode
; file:///usr/share/emacs/29.1/lisp/progmodes/cc-vars.el.gz
(setq c-default-style "linux")
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook
	    (lambda()
	      (local-set-key (kbd "C-c C-c") 'compile)                  ; origin: whole-line-or-region-comment-region
	      (local-set-key (kbd "M-e") (kbd "RET"))                   ; origin: c-end-of-statement
	      (local-set-key (kbd "M-a") 'beginning-of-visual-line))))  ; origin: c-beginning-of-statement


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;            Python config            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'python)
; Python's flying circus support for Emacs
; file:///usr/share/emacs/29.1/lisp/progmodes/python.el.gz
(setq python-indent-offset 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;             LaTeX config            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; AUCTeX - Sophisticated document creation
; https://www.gnu.org/software/auctex/
(require 'latex)    ; ~/.emacs.d/elpa/auctex-13.2.1/latex.el
(require 'preview)  ; ~/.emacs.d/elpa/auctex-13.2.1/preview.el
(add-hook 'LaTeX-mode-hook (lambda ()
  (prettify-symbols-mode t)
  (add-to-list 'TeX-view-program-selection '(output-pdf "qpdfview"))
  (add-to-list 'TeX-view-program-list '("qpdfview" "qpdfview --unique  %o"))
  (setq
    LaTeX-math-mode 1          ; real-time preview
    TeX-auto-save t
    TeX-engine 'xetex          ; use XeLaTeX default
    TeX-show-compilation nil   ; NOT display compilation windows
    TeX-save-query nil
    preview-colors '((nil  nil  nil)
                     (1.0  1.0  1.0)
                     (nil  nil  nil))
    ; preview-pdf-color-adjust-method
)))

;; RefTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq
  reftex-plug-into-AUCTeX t ; supply arg for macros lick \label, \ref, \cite, \index
  reftex-enable-partial-scans t
  reftex-save-parse-info t
  reftex-use-multiple-selection-buffers t
  reftex-toc-split-windows-horizontally t ;;*toc*buffer在左侧。
  reftex-toc-split-windows-fraction 0.2  ;;*toc*buffer 使用整个frame的比例。
)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)


(provide 'init-progmodes)
;;; init-progmodes.el ends here
