;;; core-ui.el --- UI and appearance configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Emacs user interface and visual appearance.

;;; Code:

;; Remove UI clutter
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Frame settings
(setq-default
 frame-title-format '("Emacs - " (:eval (buffer-name)))
 icon-title-format frame-title-format
 frame-resize-pixelwise t)

;; Window settings
(setq-default
 window-resize-pixelwise t
 split-width-threshold 120
 split-height-threshold 80)

;; Font settings (adjust according to your preference)
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height 120))

;; Theme (using built-in themes for minimal setup)
(load-theme 'modus-vivendi t)

;; Better mode line
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   mode-line-position
   (vc-mode vc-mode)
   "  "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

;; Cursor settings
(setq-default
 cursor-type 'box
 blink-cursor-mode nil)

;; Better visual feedback
(setq-default
 visible-bell t
 ring-bell-function 'ignore)

;; Line spacing
(setq-default line-spacing 0.1)

(provide 'core-ui)

;;; core-ui.el ends here
