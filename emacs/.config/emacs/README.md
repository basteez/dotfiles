# Emacs Configuration

A minimal, modular Emacs configuration with essential features.

## Structure

```
emacs/.config/emacs/
├── init.el              # Main entry point
└── modules/
    ├── core-settings.el    # Basic Emacs settings
    ├── core-ui.el         # UI and appearance
    ├── core-keybindings.el # Key bindings
    └── core-editing.el     # Editing enhancements
```

## Features

### Core Settings

- UTF-8 encoding by default
- Disabled backup and auto-save files
- Better scrolling behavior
- Recent files tracking
- Auto-revert for external file changes

### UI Improvements

- Disabled menu bar, tool bar, and scroll bar
- Modus Vivendi theme (built-in dark theme)
- Line numbers in programming modes
- Highlighted current line
- Better mode line

### Key Bindings

- `M-o` - Switch to other window
- `C-x C-r` - Open recent files
- `C-c e` - Open init.el file
- `M-/` - Hippie expand (better completion)
- `C-w` - Backward kill word

### Editing Enhancements

- Electric pair mode (auto-close brackets)
- Smart beginning of line movement
- Trailing whitespace cleanup
- Better completion settings
- Visual line mode for text files

## Usage

This configuration will be automatically loaded when Emacs starts. The modular structure makes it easy to:

1. Add new modules in the `modules/` directory
2. Modify individual aspects without affecting others
3. Enable/disable features by commenting out require statements in `init.el`

## Extending

To add new modules:

1. Create a new `.el` file in the `modules/` directory
2. Add `(provide 'module-name)` at the end
3. Add `(require 'module-name)` to `init.el`
