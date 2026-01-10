#!/bin/bash

# ═══════════════════════════════════════════════════════════════════════════════
# Doom Emacs LSP Server Installation Script
# ═══════════════════════════════════════════════════════════════════════════════
# This script installs all LSP servers required for the Doom Emacs configuration.
# Run with: ./install-lsp.sh [options]
#
# Options:
#   --all        Install all LSP servers
#   --minimal    Install only essential servers (TypeScript, Python, JSON, YAML)
#   --help       Show this help message
# ═══════════════════════════════════════════════════════════════════════════════

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color
BOLD='\033[1m'

# ─────────────────────────────────────────────────────────────────────────────
# Helper Functions
# ─────────────────────────────────────────────────────────────────────────────

print_header() {
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${CYAN}  🚀 Doom Emacs LSP Server Installer${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
}

print_section() {
    echo ""
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}${CYAN}  $1${NC}"
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

success() {
    echo -e "${GREEN}  ✓ $1${NC}"
}

error() {
    echo -e "${RED}  ✗ $1${NC}"
}

warning() {
    echo -e "${YELLOW}  ⚠ $1${NC}"
}

info() {
    echo -e "${BLUE}  → $1${NC}"
}

check_command() {
    if command -v "$1" &> /dev/null; then
        return 0
    else
        return 1
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Prerequisite Checks
# ─────────────────────────────────────────────────────────────────────────────

check_prerequisites() {
    print_section "Checking Prerequisites"
    
    local missing_deps=()
    
    # Check for npm
    if check_command npm; then
        success "npm is installed ($(npm --version))"
    else
        error "npm is not installed"
        missing_deps+=("npm")
    fi
    
    # Check for pip/pip3
    if check_command pip3; then
        success "pip3 is installed ($(pip3 --version | awk '{print $2}'))"
        PIP_CMD="pip3"
    elif check_command pip; then
        success "pip is installed ($(pip --version | awk '{print $2}'))"
        PIP_CMD="pip"
    else
        error "pip/pip3 is not installed"
        missing_deps+=("pip")
    fi
    
    # Check for rustup (for Rust)
    if check_command rustup; then
        success "rustup is installed"
    else
        warning "rustup is not installed (needed for Rust LSP)"
    fi
    
    # Check for go (for Go)
    if check_command go; then
        success "go is installed ($(go version | awk '{print $3}'))"
    else
        warning "go is not installed (needed for Go LSP)"
    fi
    
    if [ ${#missing_deps[@]} -gt 0 ]; then
        echo ""
        error "Missing required dependencies: ${missing_deps[*]}"
        echo -e "${YELLOW}  Please install them before running this script.${NC}"
        exit 1
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Installation Functions
# ─────────────────────────────────────────────────────────────────────────────

install_npm_packages() {
    print_section "Installing Node.js LSP Servers"
    
    local packages=(
        "typescript"
        "typescript-language-server"
        "vscode-langservers-extracted"
        "yaml-language-server"
        "dockerfile-language-server-nodejs"
        "unified-language-server"
    )
    
    for pkg in "${packages[@]}"; do
        info "Installing $pkg..."
        if npm install -g "$pkg" 2>/dev/null; then
            success "$pkg installed"
        else
            error "Failed to install $pkg"
        fi
    done
}

install_python_lsp() {
    print_section "Installing Python LSP Servers"
    
    info "Installing python-lsp-server with all extras..."
    if $PIP_CMD install 'python-lsp-server[all]' --user 2>/dev/null; then
        success "python-lsp-server installed"
    else
        warning "Failed to install python-lsp-server, trying without extras..."
        $PIP_CMD install python-lsp-server --user 2>/dev/null && success "python-lsp-server (basic) installed" || error "Failed to install python-lsp-server"
    fi
    
    info "Installing pyright..."
    if $PIP_CMD install pyright --user 2>/dev/null; then
        success "pyright installed"
    else
        error "Failed to install pyright"
    fi
}

install_rust_analyzer() {
    print_section "Installing Rust LSP (rust-analyzer)"
    
    if check_command rustup; then
        info "Installing rust-analyzer via rustup..."
        if rustup component add rust-analyzer 2>/dev/null; then
            success "rust-analyzer installed"
        else
            error "Failed to install rust-analyzer"
        fi
    else
        warning "Skipping rust-analyzer (rustup not found)"
        echo -e "${YELLOW}  Install rustup first: https://rustup.rs/${NC}"
    fi
}

install_gopls() {
    print_section "Installing Go LSP (gopls)"
    
    if check_command go; then
        info "Installing gopls..."
        if go install golang.org/x/tools/gopls@latest 2>/dev/null; then
            success "gopls installed"
        else
            error "Failed to install gopls"
        fi
    else
        warning "Skipping gopls (go not found)"
        echo -e "${YELLOW}  Install Go first: https://go.dev/dl/${NC}"
    fi
}

install_copilot() {
    print_section "Installing GitHub Copilot Language Server"
    
    info "Installing @github/copilot-language-server..."
    if npm install -g @github/copilot-language-server 2>/dev/null; then
        success "copilot-language-server installed"
        echo ""
        info "Remember to authenticate Copilot in Emacs with: M-x copilot-login"
    else
        error "Failed to install copilot-language-server"
    fi
}

# ─────────────────────────────────────────────────────────────────────────────
# Main Installation Modes
# ─────────────────────────────────────────────────────────────────────────────

install_minimal() {
    print_header
    echo -e "${CYAN}  Installing minimal LSP servers...${NC}"
    check_prerequisites
    
    print_section "Installing Essential Node.js LSP Servers"
    local packages=(
        "typescript"
        "typescript-language-server"
        "vscode-langservers-extracted"
        "yaml-language-server"
    )
    
    for pkg in "${packages[@]}"; do
        info "Installing $pkg..."
        if npm install -g "$pkg" 2>/dev/null; then
            success "$pkg installed"
        else
            error "Failed to install $pkg"
        fi
    done
    
    install_python_lsp
    
    print_summary
}

install_all() {
    print_header
    echo -e "${CYAN}  Installing all LSP servers...${NC}"
    check_prerequisites
    install_npm_packages
    install_python_lsp
    install_rust_analyzer
    install_gopls
    install_copilot
    print_summary
}

print_summary() {
    print_section "Installation Complete! 🎉"
    
    echo ""
    echo -e "${GREEN}  Next steps:${NC}"
    echo -e "${BLUE}  1. Run 'doom sync' to sync Doom Emacs packages${NC}"
    echo -e "${BLUE}  2. Run 'doom doctor' to verify everything is working${NC}"
    echo -e "${BLUE}  3. Open Emacs and start coding!${NC}"
    echo ""
    
    if check_command copilot-language-server; then
        echo -e "${YELLOW}  Note: Don't forget to authenticate Copilot with M-x copilot-login${NC}"
        echo ""
    fi
}

show_help() {
    echo ""
    echo -e "${BOLD}Doom Emacs LSP Server Installation Script${NC}"
    echo ""
    echo "Usage: $0 [option]"
    echo ""
    echo "Options:"
    echo "  --all        Install all LSP servers (TypeScript, Python, Rust, Go, etc.)"
    echo "  --minimal    Install only essential servers (TypeScript, Python, JSON, YAML)"
    echo "  --help       Show this help message"
    echo ""
    echo "If no option is provided, interactive mode will be used."
    echo ""
}

interactive_mode() {
    print_header
    
    echo -e "${CYAN}  Select installation mode:${NC}"
    echo ""
    echo "  1) All LSP servers (recommended)"
    echo "  2) Minimal (TypeScript, Python, JSON, YAML only)"
    echo "  3) Exit"
    echo ""
    read -p "  Enter your choice [1-3]: " choice
    
    case $choice in
        1) install_all ;;
        2) install_minimal ;;
        3) echo "  Bye!"; exit 0 ;;
        *) echo "  Invalid choice"; exit 1 ;;
    esac
}

# ─────────────────────────────────────────────────────────────────────────────
# Main Entry Point
# ─────────────────────────────────────────────────────────────────────────────

main() {
    case "${1:-}" in
        --all)
            install_all
            ;;
        --minimal)
            install_minimal
            ;;
        --help|-h)
            show_help
            ;;
        "")
            interactive_mode
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
}

main "$@"
