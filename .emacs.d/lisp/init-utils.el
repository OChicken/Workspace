;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'which-key)
; Displays available keybindings in popup
; https://github.com/justbur/emacs-which-key
(add-hook 'after-init-hook 'which-key-mode)
(which-key-setup-side-window-right)
(setq which-key-idle-delay 5
      which-key-popup-type 'side-window)
(diminish 'which-key-mode)

(require 'list-unicode-display)
; Search for and list unicode characters in Emacs
; https://github.com/purcell/list-unicode-display

(defun copy-file-path ()
  "Copy the path of the opened buffer to the kill ring."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (kill-new (abbreviate-file-name file-path))
        (message "Buffer is not associated with a file."))))

(defun show-file-path ()
  "Copy the path of the opened buffer to the kill ring."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (message (abbreviate-file-name file-path))
        (message "Buffer is not associated with a file."))))

(defun kill-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Dired buffer(s)." count))))

(defun kill-magit-buffers ()
  "Kill all Magit buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'magit-diff-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'magit-process-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'magit-revision-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Magit buffer(s)." count))))

(defun kill-log-buffers ()
  "Kill all log/error buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'fundamental-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'emacs-lisp-compilation-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'xref--xref-buffer-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'package-menu-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'flycheck-error-list-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'flycheck-error-message-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'TeX-output-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'TeX-special-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'compilation-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'completion-list-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'grep-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'occur-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'ob-sagemath-error-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'diff-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Output Log buffer(s)." count))))

(defun kill-gnus-buffers ()
  "Kill buffers of Gnus."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'message-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'gnus-summary-mode)
          (setq count (1+ count))
          (kill-buffer buffer))
        (when (equal major-mode 'gnus-article-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Gnus buffer(s)." count))))

(defun kill-el-gz-buffers ()
  "Kill all buffers with filenames ending in '.el.gz'."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (and (buffer-file-name)
                   (string-match "\\.el\\.gz\\'" (buffer-file-name)))
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i *.el.gz file(s)." count))))

(provide 'init-utils)
;;; init-utils.el ends here
