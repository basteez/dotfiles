;;; core-keybindings.el --- Key bindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Essential key bindings for better Emacs workflow.

;;; Code:

;; Better window navigation
(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "M-o") 'other-window)

;; Better buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; File operations
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Text manipulation
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Search and replace
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace)

;; Better commenting
(global-set-key (kbd "M-;") 'comment-dwim)

;; Line operations
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

;; Function to move to beginning of line or indentation
(defun beginning-of-line-or-indentation ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

;; Quick access to configuration
(global-set-key (kbd "C-c e") 'find-user-init-file)

(defun find-user-init-file ()
  "Find and open the user's init.el file."
  (interactive)
  (find-file user-init-file))

;; Quick quit
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)

(provide 'core-keybindings)

;;; core-keybindings.el ends here
