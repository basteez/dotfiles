;;; core-editing.el --- Editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for better text editing experience.

;;; Code:

;; Better completion
(setq-default
 completion-cycle-threshold 3
 tab-always-indent 'complete
 completion-ignore-case t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t)

;; Hippie expand configuration
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Better whitespace handling
(setq-default
 require-final-newline t
 delete-trailing-lines t)

;; Auto-cleanup whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Better indentation
(setq-default
 indent-tabs-mode nil
 tab-width 4
 standard-indent 4)

;; Show whitespace in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Auto-pair configuration
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\' . ?\')
        (?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})))

;; Better word wrapping
(setq-default
 word-wrap t
 truncate-lines nil
 truncate-partial-width-windows nil)

;; Automatically wrap text in text modes
(add-hook 'text-mode-hook 'visual-line-mode)

;; Better undo/redo
(setq undo-limit 800000
      undo-strong-limit 12000000
      undo-outer-limit 120000000)

;; Smart beginning of line
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; Better kill-line behavior
(defun smart-kill-line ()
  "Kill line but preserve indentation of empty lines."
  (interactive)
  (if (and (eolp) (not (bolp)))
      (delete-horizontal-space)
    (kill-line)))

;; Bind smart functions
(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key (kbd "C-k") 'smart-kill-line)

(provide 'core-editing)

;;; core-editing.el ends here
