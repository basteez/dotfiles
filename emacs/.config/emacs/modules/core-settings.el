;;; core-settings.el --- Core Emacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic Emacs settings and behavior configuration.

;;; Code:

;; Basic settings
(setq-default
 ;; Disable startup screen
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 
 ;; Better defaults
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 
 ;; Encoding
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix
 
 ;; Editing behavior
 tab-width 4
 indent-tabs-mode nil
 fill-column 80
 sentence-end-double-space nil
 
 ;; Better scrolling
 scroll-conservatively 10000
 scroll-preserve-screen-position t
 scroll-margin 5
 
 ;; Ring behavior
 kill-ring-max 200
 history-length 1000)

;; Enable useful modes
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)

;; Set yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show matching parentheses
(show-paren-mode 1)

;; Enable line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable column numbers
(column-number-mode 1)

;; Automatically pair parentheses
(electric-pair-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Better unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Recent files
(setq recentf-max-saved-items 100)

(provide 'core-settings)

;;; core-settings.el ends here
