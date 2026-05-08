#!/usr/bin/env bash
# fix-doom.sh — install missing tools flagged by `doom doctor`
# Interactive: pick which language/tool groups you actually use.

set -e

# ---------- helpers ----------
say()  { printf "\n\033[1;34m==>\033[0m %s\n" "$*"; }
warn() { printf "\033[1;33m!!\033[0m %s\n" "$*"; }
ok()   { printf "\033[1;32m✔\033[0m %s\n" "$*"; }
ask()  { read -rp "$1 [y/N] " r; [[ "$r" =~ ^[Yy]$ ]]; }

have() { command -v "$1" >/dev/null 2>&1; }

# ---------- detect OS / package manager ----------
PM=""
if [[ "$OSTYPE" == "darwin"* ]]; then
    PM="brew"
elif have apt-get; then
    PM="apt"
elif have pacman; then
    PM="pacman"
elif have dnf; then
    PM="dnf"
else
    warn "Could not detect a known package manager. You'll need to install system packages manually."
fi

pkg_install() {
    case "$PM" in
        brew)   brew install "$@" ;;
        apt)    sudo apt-get install -y "$@" ;;
        pacman) sudo pacman -S --needed --noconfirm "$@" ;;
        dnf)    sudo dnf install -y "$@" ;;
        *)      warn "Skipping system packages: $*" ;;
    esac
}

say "Detected package manager: ${PM:-none}"

# ---------- 1. fix +roam2 → +roam in init.el ----------
INIT_EL="${DOOMDIR:-$HOME/.doom.d}/init.el"
[[ -f "$INIT_EL" ]] || INIT_EL="$HOME/.config/doom/init.el"

if [[ -f "$INIT_EL" ]] && grep -q '+roam2' "$INIT_EL"; then
    say "Fixing deprecated +roam2 in $INIT_EL"
    cp "$INIT_EL" "$INIT_EL.bak"
    sed -i.tmp 's/+roam2/+roam/g' "$INIT_EL" && rm -f "$INIT_EL.tmp"
    ok "Replaced +roam2 → +roam (backup at $INIT_EL.bak)"
else
    ok "No +roam2 to fix"
fi

# ---------- 2. Python tooling ----------
if ask "Install Python tooling (black, isort, pyflakes, pytest)?"; then
    if have pipx; then
        for p in black isort pyflakes pytest; do pipx install "$p" || true; done
    elif have pip3; then
        pip3 install --user black isort pyflakes pytest
    else
        warn "No pip3/pipx found — install Python first."
    fi
fi

# ---------- 3. Shell tooling ----------
if ask "Install shell tooling (shellcheck, shfmt)?"; then
    case "$PM" in
        brew)   pkg_install shellcheck shfmt ;;
        apt)    pkg_install shellcheck
                # shfmt isn't in older apt repos; fall back to go install if needed
                have shfmt || pkg_install shfmt || warn "Install shfmt manually: https://github.com/mvdan/sh" ;;
        pacman) pkg_install shellcheck shfmt ;;
        dnf)    pkg_install ShellCheck shfmt ;;
    esac
fi

# ---------- 4. Go tooling ----------
if ask "Install Go tooling (gopls, gomodifytags, gotests, gore)?"; then
    if ! have go; then
        warn "Install Go first (https://go.dev/dl/), then re-run."
    else
        go install golang.org/x/tools/gopls@latest
        go install github.com/fatih/gomodifytags@latest
        go install github.com/cweill/gotests/gotests@latest
        go install github.com/x-motemen/gore/cmd/gore@latest
        ok "Make sure \$(go env GOPATH)/bin is on your PATH"
    fi
fi

# ---------- 5. Web tooling ----------
if ask "Install web tooling (stylelint, js-beautify)?"; then
    if have npm; then
        npm install -g stylelint js-beautify
    else
        warn "Install Node/npm first."
    fi
fi

# ---------- 6. Java formatter ----------
if ask "Install clang-format (used by Doom for Java formatting)?"; then
    case "$PM" in
        brew)   pkg_install clang-format ;;
        apt)    pkg_install clang-format ;;
        pacman) pkg_install clang ;;   # clang-format ships with clang on Arch
        dnf)    pkg_install clang-tools-extra ;;
    esac
fi

# ---------- 7. Markdown preview ----------
if ask "Install grip for Markdown preview?"; then
    if have pipx; then pipx install grip
    elif have pip3; then pip3 install --user grip
    else warn "No pip3/pipx found."
    fi
fi

# ---------- 8. direnv ----------
if ask "Install direnv?"; then
    pkg_install direnv
    ok "Add 'eval \"\$(direnv hook bash)\"' (or zsh) to your shell rc."
fi

# ---------- 9. dockfmt (rarely needed) ----------
if ask "Install dockfmt? (skip unless you specifically want Dockerfile formatting)"; then
    if have go; then
        go install github.com/jessfraz/dockfmt@latest
    else
        warn "Needs Go."
    fi
fi

# ---------- 10. Native compilation note ----------
say "About native compilation:"
cat <<'EOF'
Native-comp requires reinstalling Emacs, which this script won't do automatically.
Quick reference:
  macOS:   brew install emacs-plus@30 --with-native-comp
  Ubuntu:  sudo apt install emacs   (28+ ships with native-comp)
  Arch:    sudo pacman -S emacs     (native-comp by default)
  Fedora:  sudo dnf install emacs   (native-comp by default)
After reinstalling: doom sync && doom build
EOF

# ---------- 11. resync Doom ----------
if have doom && ask "Run 'doom sync' now?"; then
    doom sync
fi

ok "Done. Run 'doom doctor' again to see remaining warnings."
