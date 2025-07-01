#!/usr/bin/env bash

# Test script for Emacs configuration
# This script tests if the configuration loads without errors

echo "Testing Emacs configuration..."

cd "$(dirname "$0")"

# Test basic loading
if emacs --batch --eval "(progn (load-file \"init.el\") (message \"SUCCESS: Configuration loaded\"))" 2>/dev/null; then
    echo "✓ Configuration loads successfully"
else
    echo "✗ Configuration failed to load"
    exit 1
fi

# Test module loading
modules=("core-settings" "core-ui" "core-keybindings" "core-editing")

for module in "${modules[@]}"; do
    if emacs --batch --eval "(progn (add-to-list 'load-path \"$(pwd)/modules\") (require '$module) (message \"$module loaded\"))" 2>/dev/null; then
        echo "✓ Module $module loads successfully"
    else
        echo "✗ Module $module failed to load"
        exit 1
    fi
done

echo "All tests passed! Configuration is working correctly."
