;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)) ; origin: backward-up-list
(add-hook 'org-mode-hook 'org-indent-mode)
(diminish 'org-indent-mode)

(setq org-imenu-depth 3            ; The maximum level for Imenu access to Org headlines.
      org-support-shift-select t   ; make shift-cursor commands select text when possible
      org-log-done 'time           ; Information to record when a task moves to the DONE state.
      org-hide-emphasis-markers t  ; font-lock should hide the emphasis marker characters.
      org-tags-column -80          ; The column to which tags should be indented in a headline.
      org-fast-tag-selection-single-key 'expert)



;;;;;;;;;;;;
;; Agenda ;;
;;;;;;;;;;;;

(require 'org-agenda)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

(require 'org-capture)
; Fast note taking in Org
; file:///usr/share/emacs/29.1/lisp/org/org-capture.el.gz
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/Documents/gtd.org")
(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* TODO %?\nCLOCK: %U\n " :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\nCLOCK: %a\n " :clock-resume t)
        ))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(setq org-agenda-files (list "~/Documents/gtd.org")
      org-agenda-compact-blocks t
      org-agenda-sticky t
      org-agenda-start-on-weekday 0
      org-agenda-span 'day
      org-agenda-include-diary nil
      org-agenda-window-setup 'current-window  ; make org-agenda open up in the current window
      org-agenda-sorting-strategy
      '((agenda habit-down time-up user-defined-up effort-up category-keep)
        (todo category-up effort-up)
        (tags category-up effort-up)
        (search category-up)))



;;;;;;;;;;
;; Link ;;
;;;;;;;;;;

(require 'ol)
; Org links library
; file:///usr/share/emacs/29.1/lisp/org/ol.el.gz
(define-key global-map (kbd "C-c l") 'org-store-link)

(require 'org-cliplink)
; Insert org-mode links from clipboard
; https://github.com/rexim/org-cliplink
(global-set-key (kbd "C-c y") 'org-cliplink)
(setq org-cliplink-max-length 120)  ; cuts any title that exceeds the limit



;;;;;;;;;;;;;;;
;; Org Clock ;;
;;;;;;;;;;;;;;;

(require 'org-clock)
; The time clocking code for Org mode
; file:///usr/share/emacs/29.1/lisp/org/org-clock.el.gz

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist   t
      org-clock-in-resume t)

;; org-clock preferences
(setq org-clock-into-drawer t  ; Save clock data and notes in the LOGBOOK drawer
      org-log-into-drawer   t  ; Save state changes in the LOGBOOK drawer
      org-clock-out-remove-zero-time-clocks t)  ; Removes clocked tasks with 0:00 duration

(require 'org-duration)
; Library handling duration
; file:///usr/share/emacs/29.1/lisp/org/org-duration.el.gz
(setq org-duration-format  ; Show clock sums as hours and minutes, not "n days" etc.
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))



;;;;;;;;;;;;;;;;
;; Org Refile ;;
;;;;;;;;;;;;;;;;

(require 'org-refile)
; Refile Org Subtrees
; file:///usr/share/emacs/29.1/lisp/org/org-refile.el.gz

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)



;;;;;;;;;;;
;; Babel ;;
;;;;;;;;;;;

(require 'org-src)
; Source code examples in Org
; file:///usr/share/emacs/29.1/lisp/org/org-src.el.gz
(setq org-edit-src-content-indentation 0)

(require 'ob)
; Working with Code Blocks in Org
; file:///usr/share/emacs/29.1/lisp/org/ob.el.gz

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((C . t)
      (C++ . t)
      (ditaa . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (go . t)
      (js .t)
      (latex . t)
      (octave . t)
      (php . t)
      (python . t)
      (rust . t)
      (sagemath . t)
      (shell . t)
      (sql . t)))))

(require 'ob-core)
; Working with Code Blocks
; file:///usr/share/emacs/29.1/lisp/org/ob-core.el.gz
(setq org-confirm-babel-evaluate nil) ;; Do not confirm before evaluation

(require 'ob-latex)
; Babel Functions for LaTeX
; file:///usr/share/emacs/29.1/lisp/org/ob-latex.el.gz
; (setq org-babel-latex-pdf-svg-process "dvisvgm --pdf %f -o %O")

(require 'ob-ditaa)
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

(require 'ob-sagemath)
; org-babel integration with SageMath
; https://github.com/sagemath/ob-sagemath
(set-variable 'sage-shell:use-prompt-toolkit nil)  ; for Ipython >=7 (sage-shell-mode.el) (same below)
(set-variable 'sage-shell:use-simple-prompt  t)
(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)



;;;;;;;;;;;;;;;;;;;
;; LaTeX preview ;;
;;;;;;;;;;;;;;;;;;;

(require 'org)
; Outline-based notes management and organizer
; file:///usr/share/emacs/29.1/lisp/org/org.el.gz

; adjust the compiler of imagemagick to XeLaTeX and use it as default process to
; convert LaTeX fragments to image files
(let ((pos (assoc 'imagemagick org-preview-latex-process-alist)))
  (plist-put (cdr pos) :latex-compiler '("xelatex -interaction nonstopmode -output-directory %o %f")))
(setq org-preview-latex-default-process 'imagemagick)  ; origin: dvipng

(require 'org-fragtog)
; Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
; https://github.com/io12/org-fragtog
(add-hook 'org-mode-hook 'org-fragtog-mode) ; for latex live previous

(require 'xenops)
; An editing environment for LaTeX mathematical documents
; https://github.com/dandavison/xenops



;;;;;;;;;;;;
;; Export ;;
;;;;;;;;;;;;

(require 'ox)
; Export Framework for Org Mode
; file:///usr/share/emacs/29.1/lisp/org/ox.el.gz
(setq org-export-with-tags nil  ; dont export headlines with tags
      org-export-coding-system 'utf-8
      org-html-validation-link nil
      org-export-with-broken-links t  ; who cares about annoying broken link errors..
      )

(require 'ox-latex)
; LaTeX Back-End for Org Export Engine
; file:///usr/share/emacs/29.1/lisp/org/ox-latex.el.gz
(setq org-latex-compiler "xelatex"                    ; origin: "pdflatex"
      org-latex-image-default-width ".5\\linewidth")  ; origin: ".9\\linewidth"

(add-to-list 'org-latex-classes
             '("ox-latex-scrarticle"
               "\\documentclass[a4paper, headsepline, footsepline]{scrarticle}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-html-htmlize-output-type 'css)


(provide 'init-org)
;;; init-org.el ends here
