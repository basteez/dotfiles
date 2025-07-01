;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the main entry point for the Emacs configuration.
;; It sets up the module system and loads all configuration modules.

;;; Code:

;; Set up package management early
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap use-package if not available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load configuration modules
(require 'core-settings)
(require 'core-keybindings)
(require 'core-ui)
(require 'core-editing)

;;; init.el ends here
