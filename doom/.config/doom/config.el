;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load "~/.config/doom/private.el" t)
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


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
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

;; ORG ROAM
(setq org-roam-directory "~/Documents/org/org-roam/")
(setq org-roam-db-autosync-enable t)
(setq org-enable-roam-ui t)

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

;; CLAUDE CODE
(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))  ; exposes xref/project as MCP tools

;; SLACK
;; ---------------------------------------------------------------------------
;; How to get the Slack token and cookie
;;
;; Slack no longer accepts the old `xoxs-' user tokens. You need a pair of
;; credentials: a short-lived `xoxc-' token and a matching `d' cookie. Both
;; must be extracted manually from a logged-in Slack session in the browser.
;;
;; 1. Get the token (xoxc-)
;;    Open Slack in your browser (e.g. https://yourteam.slack.com) and log in.
;;    Open DevTools (F12) -> Console and run this one-liner:
;;
;;    JSON.parse(localStorage.localConfig_v2)
;;      .teams[document.location.pathname.match(/^\/client\/([TE][A-Z0-9]+)/)[1]]
;;      .token
;;
;;    It returns a string like "xoxc-...". Copy it.
;;
;; 2. Get the cookie (d)
;;    The `d' cookie is HttpOnly, so it can't be read from JavaScript. Grab it
;;    by hand:
;;      - DevTools -> Application tab (Firefox: Storage)
;;      - Cookies -> pick the https://app.slack.com domain (or yourteam.slack.com)
;;      - Find the cookie named `d' and copy its value (a long "xoxd-..." string)
;;
;; Store both in ~/.authinfo.gpg as:
;;   machine yourteam.slack.com login you@example.com password xoxc-...
;;   machine yourteam.slack.com login you@example.com^cookie password xoxd-...
;;
;; Tokens and cookies expire periodically; re-extract them when Slack stops
;; authenticating. `M-x slack-refresh-token' walks you through it interactively.
;; ---------------------------------------------------------------------------
(use-package slack
  :bind (("C-c S K" . slack-stop)
         ("C-c S c" . slack-select-rooms)
         ("C-c S u" . slack-select-unread-rooms)
         ("C-c S U" . slack-user-select)
         ("C-c S s" . slack-search-from-messages)
         ("C-c S J" . slack-jump-to-browser)
         ("C-c S j" . slack-jump-to-app)
         ("C-c S e" . slack-insert-emoji)
         ("C-c S E" . slack-message-edit)
         ("C-c S r" . slack-message-add-reaction)
         ("C-c S t" . slack-thread-show-or-create)
         ("C-c S g" . slack-message-redisplay)
         ("C-c S G" . slack-conversations-list-update-quick)
         ("C-c S q" . slack-quote-and-reply)
         ("C-c S Q" . slack-quote-and-reply-with-link)
         (:map slack-mode-map
               (("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-thread-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)
                ("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)))
         (:map slack-message-compose-buffer-mode-map
               (("C-c '" . slack-message-send-from-buffer)))
         )
  :custom
  (slack-extra-subscribed-channels (mapcar 'intern (list "some-channel")))
  :config
  (slack-register-team
   :name my/slack-team
   :token (auth-source-pick-first-password
           :host (concat my/slack-team ".slack.com")
           :user my/slack-email)
   :cookie (auth-source-pick-first-password
            :host (concat my/slack-team ".slack.com")
            :user (concat my/slack-email "^cookie"))
   :full-and-display-names t
   :default t
   :subscribed-channels nil))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
