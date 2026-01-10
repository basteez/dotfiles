# Doom Emacs Configuration 🚀

A modern and functional Doom Emacs configuration, optimized for multi-language development with automatic LSP support.

## 📋 Supported Languages

This configuration supports the following languages with automatic LSP:

- **TypeScript/JavaScript** - Full LSP support with tree-sitter
- **Python** - Pyright LSP
- **Rust** - rust-analyzer with tree-sitter
- **Go** - gopls LSP
- **Java** - Eclipse JDT Language Server with tree-sitter
- **JSON** - LSP support
- **YAML** - LSP support
- **TOML** - LSP support
- **Markdown** - Enhanced markdown with grip
- **Docker** - Dockerfile LSP
- **HTML/CSS/Web** - Full web development support
- **Shell/Bash** - Built-in support

## ✨ Key Features

### 🎯 LSP Out-of-the-Box

- **Auto-activation**: LSP servers start automatically when you open a file
- **Auto-installation**: lsp-mode automatically installs servers when needed
- **Performance optimized**: Balanced configuration between speed and features
- **LSP-UI**: Modern interface similar to VSCode

### 🛠️ Modern Features

- **Tree-sitter**: Advanced parsing and syntax highlighting
- **Copilot**: Integrated AI code completion
- **Format on save**: Automatic formatting when saving
- **Auto-imports**: Automatic import organization
- **Debugger (DAP)**: Integrated debugging for Node.js, Python, Go, Rust
- **Docker support**: Editing and LSP for Dockerfile
- **Direnv**: Project-specific environment support
- **Integrated Git**: Magit with forge support

### 🎨 UI/UX

- Relative line numbers (like LazyVim)
- Informative modeline
- Managed popups
- Indent guides
- VC gutter for diff visualization
- Workspace management

## 📦 Installation and Setup

### 1. Sync Packages

After copying this configuration, run:

```bash
doom sync
```

### 2. Install Language Servers

Most LSP servers are automatically installed by Doom when you open a file of the corresponding language. However, you can install them manually or use the provided script:

```bash
./install-lsp.sh
```

#### Manual Installation

##### TypeScript/JavaScript

```bash
npm install -g typescript-language-server typescript
```

##### Python

```bash
# Option 1: python-lsp-server (recommended)
pip install 'python-lsp-server[all]'

# Option 2: pyright
pip install pyright
```

##### Rust

```bash
rustup component add rust-analyzer
```

##### Go

```bash
go install golang.org/x/tools/gopls@latest
```

##### JSON/YAML/Docker/HTML/CSS

```bash
npm install -g vscode-langservers-extracted yaml-language-server dockerfile-language-server-nodejs
```

##### Markdown

```bash
npm install -g unified-language-server
```

##### Copilot (optional but recommended)

```bash
npm install -g @github/copilot-language-server
```

### 3. Restart Emacs

```bash
doom sync && doom doctor
```

Open Emacs and verify everything works correctly.

## 🎓 How to Add New Languages

### Simple 3-Step Process

#### 1. Add the module in `init.el`

Find the `:lang` section and uncomment or add the language:

```elisp
:lang
;; Example for C/C++
(cc +lsp +tree-sitter)  ; C/C++ support

;; Example for Haskell
(haskell +lsp)          ; Haskell support
```

#### 2. Configure LSP auto-start in `config.el`

Add the appropriate hook in the LSP configuration section:

```elisp
(after! lsp-mode
  ;; Example for C/C++
  (add-hook 'c-mode-hook #'lsp!)
  (add-hook 'c++-mode-hook #'lsp!)

  ;; Example for Haskell
  (add-hook 'haskell-mode-hook #'lsp!))
```

#### 3. (Optional) Add extra packages in `packages.el`

Only if necessary, add additional packages:

```elisp
;; Example for a mode not included by default
(package! haskell-mode)
```

#### 4. Sync and install the LSP server

```bash
doom sync
# Install the specific LSP server (see language documentation)
```

### Common Language Examples

#### PHP

```elisp
;; init.el
(php +lsp)

;; config.el
(after! lsp-mode
  (add-hook 'php-mode-hook #'lsp!))

;; Terminal
composer require felixfbecker/language-server
composer run-script --working-dir=vendor/felixfbecker/language-server parse-stubs
```

#### C#

```elisp
;; init.el
(csharp +lsp +tree-sitter)

;; config.el
(after! lsp-mode
  (add-hook 'csharp-mode-hook #'lsp!))

;; Terminal: OmniSharp installs automatically
```

#### Elixir

```elisp
;; init.el
(elixir +lsp +tree-sitter)

;; config.el
(after! lsp-mode
  (add-hook 'elixir-mode-hook #'lsp!))

;; Terminal
git clone https://github.com/elixir-lsp/elixir-ls.git
cd elixir-ls && mix deps.get && mix compile && mix elixir_ls.release
```

## 🔧 Custom Configurations

### Disable Format-on-Save

If you don't want automatic formatting:

```elisp
;; In config.el, comment these lines:
;; (add-hook 'before-save-hook #'lsp-format-buffer t)
;; (add-hook 'before-save-hook #'lsp-organize-imports t)
```

### Change Theme

```elisp
;; In config.el
(setq doom-theme 'doom-gruvbox)  ; or doom-tokyo-night, doom-nord, etc.
```

### Configure Custom Fonts

```elisp
;; In config.el
(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "SF Pro" :size 14))
```

## 🚀 Useful Keybindings

### LSP

- `SPC c a` - Code actions
- `SPC c d` - Jump to definition
- `SPC c D` - Jump to references
- `SPC c f` - Format buffer
- `SPC c r` - Rename symbol
- `K` - Show documentation

### File Navigation

- `SPC .` - Find file
- `SPC f r` - Recent files
- `SPC /` - Search in project
- `SPC s p` - Search in project (alternative)

### Git (Magit)

- `SPC g g` - Magit status
- `SPC g d` - Git diff
- `SPC g b` - Git blame

### Project

- `SPC p p` - Switch project
- `SPC p f` - Find file in project
- `SPC p !` - Run command in project root

### Windows

- `SPC w v` - Split window vertically
- `SPC w s` - Split window horizontally
- `SPC w d` - Delete window

## 🐛 Troubleshooting

### LSP doesn't start automatically

1. Verify the LSP server is installed: `M-x lsp-doctor`
2. Check LSP logs: `M-x lsp-log-io-mode`
3. Restart the LSP server: `M-x lsp-workspace-restart`

### Copilot doesn't work

1. Verify server installation: `npm install -g @github/copilot-language-server`
2. Authenticate Copilot: `M-x copilot-login`
3. Restart Emacs

### Performance Issues

If Emacs is slow:

```elisp
;; In config.el, add:
(setq lsp-idle-delay 0.5)  ; Increase delay
(setq lsp-log-io nil)      ; Disable logging
```

## 📚 Useful Resources

- [Doom Emacs Documentation](https://docs.doomemacs.org)
- [LSP Mode](https://emacs-lsp.github.io/lsp-mode/)
- [Tree-sitter](https://tree-sitter.github.io/tree-sitter/)
- [Magit Manual](https://magit.vc/manual/)

## 🎉 Conclusion

This configuration provides a modern experience similar to LazyVim with:

- ✅ LSP working out-of-the-box
- ✅ Auto-activation for all supported languages
- ✅ Easy addition of new languages
- ✅ Modern and responsive UI
- ✅ Performance optimized

Happy coding! 🚀
