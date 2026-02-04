;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; ORG ROAM
(setq org-roam-directory "~/Documents/org/org-roam/")
(setq org-roam-db-autosync-enable t)
(setq org-enable-roam-ui t)

;;
;; LSP CONFIGURATION - Auto-enable and optimize
;;

;; Enable LSP for all +lsp flagged languages (deferred for performance)
(setq lsp-auto-guess-root t)

;; LSP Performance optimizations
(after! lsp-mode
  (setq lsp-idle-delay 0.1
        lsp-log-io nil
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 5000
        lsp-headerline-breadcrumb-enable t
        lsp-modeline-diagnostics-enable t
        lsp-modeline-code-actions-enable t
        lsp-lens-enable t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-semantic-tokens-enable t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-snippet t
        lsp-eldoc-enable-hover t))

;; LSP-UI Configuration
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 20
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t))

;; Flycheck (linter) integration with LSP
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change)))

;; LSP diagnostics via flycheck
(setq lsp-diagnostics-provider :flycheck)

;; Java LSP Configuration (lsp-java / jdtls)
(setq lsp-java-workspace-dir (expand-file-name "~/.cache/java-workspace")
      lsp-java-workspace-cache-dir (expand-file-name "~/.cache/java-workspace/.cache")
      lsp-java-server-install-dir (expand-file-name "~/.local/share/eclipse.jdt.ls"))

(after! lsp-java
  (setq lsp-java-completion-enabled t
        lsp-java-completion-guess-method-arguments t
        lsp-java-completion-overwrite t
        lsp-java-completion-favorite-static-members
        '("org.junit.Assert.*"
          "org.junit.jupiter.api.Assertions.*"
          "java.util.Objects.requireNonNull"
          "java.util.Objects.requireNonNullElse")
        lsp-java-autobuild-enabled t
        lsp-java-save-actions-organize-imports t
        lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-references-code-lens-enabled t
        lsp-java-implementations-code-lens-enabled t
        lsp-java-format-on-type-enabled t
        lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Xmx2G" "-Xms100m")))

;; DAP (Debugger) Configuration
(after! dap-mode
  (setq dap-auto-configure-mode t)
  (require 'dap-node)
  (require 'dap-python)
  (require 'dap-go)
  (require 'dap-lldb)
  (require 'dap-java)  ; Java debugging
  (dap-auto-configure-mode 1))

;; Tree-sitter configuration
(after! tree-sitter
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Corfu completion (modern completion framework)
(after! corfu
  (setq corfu-auto t
        corfu-auto-delay 0.0      ; No delay - instant popup
        corfu-auto-prefix 1       ; Complete after 1 char
        corfu-cycle t
        corfu-preselect 'first    ; Preselect first candidate
        corfu-scroll-margin 5
        corfu-quit-no-match 'separator)
  ;; Enable corfu in minibuffer for M-x etc
  (global-corfu-mode))

;; Ensure LSP completion works with corfu
(after! lsp-mode
  ;; Force LSP to use capf (corfu-compatible)
  (setq lsp-completion-provider :capf)
  ;; Enable completion
  (setq lsp-completion-enable t)
  ;; Make sure lsp-completion-mode is active
  (add-hook 'lsp-mode-hook #'lsp-completion-mode))

;; Format on save only when LSP is active (handled by Doom's format module)
;; The (format +onsave) flag in init.el handles this properly

;; Direnv support (for project-specific environments)
(after! direnv
  (direnv-mode))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; BANNER
(defun my-weebery-is-always-greater ()
  (let* ((banner '("███████ ███    ███  █████   ██████ ███████ "
                   "██      ████  ████ ██   ██ ██      ██      "
                   "█████   ██ ████ ██ ███████ ██      ███████ "
                   "██      ██  ██  ██ ██   ██ ██           ██ "
                   "███████ ██      ██ ██   ██  ██████ ███████ "
                   ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'my-weebery-is-always-greater)



;; ORG-ROAM-UI
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; ORG-BULLETS
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; COPILOT
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
