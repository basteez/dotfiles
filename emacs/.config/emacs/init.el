(tool-bar-mode -1)              ; Hide the outdated icons
(scroll-bar-mode -1)            ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t)  ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI

;; straight.el | package manager
(defvar bootstrap-version)
(let ((bootstrap-file
    (expand-file-name
      "straight/repos/straight.el/bootstrap.el"
      (or (bound-and-true-p straight-base-dir)
        user-emacs-directory)))
    (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

;; set sane defaults
(use-package emacs
  :init
  ;; disable initial scratch message
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message ""))
  ;; let answer just 'y' or 'n' in confirmation dialogs
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; make everything use UTF-8
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  ;; use spaces but configure tab-with for modes that use tabs
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  ;; set correct keybindings for macOS
  (when (eq system-type 'darwin)
		(setq mac-command-modifier 'super)
		(setq mac-option-modifier 'meta)
		(setq mac-control-modifier 'control))
  ;; enable relative line numbers
  (defun ab/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers))

;; evil mode
(use-package evil
  :demand ; No lazy loading
  :config
  (evil-mode 1))

;; load theme (doom-themes)
(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-challenger-deep t))

;; add a nicer modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; install nerd icons
(use-package nerd-icons)

;; org
(use-package org)


;; org-roam
(use-package org-roam
  :straight t
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-db-autosync-enable t))


;; Show the help buffer after startup
(add-hook 'after-init-hook 'help-quick)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
