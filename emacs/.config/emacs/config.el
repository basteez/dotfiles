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
  ;; use spaces but configure tab-width for modes that use tabs
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

(use-package evil
  :demand
  :config
  (evil-mode 1))

(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-challenger-deep t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons)

(use-package org)

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
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-db-autosync-enable t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(add-hook 'after-init-hook 'help-quick)
