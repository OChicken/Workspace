;;; blog.el --- generate and deploy settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(package-initialize)
(require 'ox-html)
(require 'ox-publish)

; How to remove message `Indentation setup for shell type sh`
; https://emacs.stackexchange.com/q/52846
(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (let ((inhibit-message t))
                (apply orig-fun args))))

;; Set html layout
(setq org-html-head      (with-temp-buffer (insert-file-contents "static/head.html")      (buffer-string))
      org-html-preamble  (with-temp-buffer (insert-file-contents "static/preamble.html")  (buffer-string))
      org-html-postamble (with-temp-buffer (insert-file-contents "static/postamble.html") (buffer-string)))
(add-to-list 'org-html-mathjax-options '(path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"))

;; Set sub-dirs
(defvar blog-dirs '("static" "research" "projects" "footprint" "private"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Choose `generate' or `deploy' ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar blog-arg3 (nth 3 command-line-args))
(defvar blog-force-generate nil)

; Does elisp have a way to jump to (goto) labels in the code, ala common lisp's go?
; https://emacs.stackexchange.com/a/68622
(cl-tagbody

(cond
 ((not (or (string= blog-arg3 "g") (string= blog-arg3 "d") (string= blog-arg3 "clean")))
  (if (not (file-directory-p blog-arg3))
      (progn
        (message "Your input does not corresponds to a dir")
        (kill-emacs))
    (go blog-tag-generate-dir)))
 ((string= blog-arg3 "g")
  (defvar blog-arg4 (nth 4 command-line-args))
  (if (string= blog-arg4 "t")
      (setq blog-force-generate t)
    (message "To force generate all blog html files, the last arg should be t."))
  (go blog-tag-generate-all))
 ((string= blog-arg3 "d")
  (go blog-tag-deploy))
 ((string= blog-arg3 "clean")
  (go blog-tag-clean)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the project specified by the input dir ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
blog-tag-generate-dir

(defvar blog-dir blog-arg3)
(setq org-publish-project-alist
      (list (list blog-dir
                  :base-directory blog-dir
                  :publishing-directory blog-dir
                  :recursive t
                  :publishing-function 'org-html-publish-to-html)))

;; generate html (publish locally)
(org-publish-project blog-dir)

(go blog-tag-done)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate all blog dirs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
blog-tag-generate-all

;; root-dir "."
(setq org-publish-project-alist
      (list (list "home"
                  :base-directory "."
                  :publishing-directory "."
                  :recursive nil
                  :publishing-function 'org-html-publish-to-html
                  :exclude ".*"
                  :include '("index.org"))))

;; add blog-dirs to generate
(dolist (dir blog-dirs)
  (add-to-list 'org-publish-project-alist
               (list dir
                     :base-directory dir
                     :publishing-directory dir
                     :recursive t
                     :publishing-function 'org-html-publish-to-html) t))

;; generate html (publish locally)
(add-to-list 'org-publish-project-alist
             (list "generate" :components (mapcar 'car org-publish-project-alist)) t)
(org-publish-project "generate" blog-force-generate)

(go blog-tag-done)



;;;;;;;;;;;;
;; Deploy ;;
;;;;;;;;;;;;
blog-tag-deploy

(defvar blog-remote-path "/ssh:OChicken@OChicken.net:/var/www/html/")

;; root-dir "."
(setq org-publish-project-alist
      (list (list "home"
                  :base-directory "."
                  :publishing-directory blog-remote-path
                  :recursive nil
                  :publishing-function 'org-publish-attachment
                  :exclude ".*"
                  :include '("index.html"))))

;; add blog-dirs to deploy
(dolist (dir blog-dirs)
  (add-to-list 'org-publish-project-alist
               (list dir
                     :base-directory dir
                     :publishing-directory (concat blog-remote-path dir)
                     :recursive t
                     :base-extension ".*"
                     :exclude ".org"  ; upload all files in blog-dirs except for the org files
                     :publishing-function 'org-publish-attachment) t))

;; deploy attachment (publish to remote)
(add-to-list 'org-publish-project-alist
             (list "deploy" :components (mapcar 'car org-publish-project-alist)) t)
(org-publish-project "deploy")

(go blog-tag-done)



;;;;;;;;;;;
;; Clean ;;
;;;;;;;;;;;
blog-tag-clean

(dolist (dir (cdr blog-dirs))
  (when (file-directory-p dir)
    (dolist (file (directory-files-recursively dir "\\.html\\'"))
      (message "rm %s" file)
      (delete-file file))))

(go blog-tag-done)



;;;;;;;;;
;; End ;;
;;;;;;;;;

blog-tag-done (message "done")
)

(provide 'blog)
;;; blog.el ends here
