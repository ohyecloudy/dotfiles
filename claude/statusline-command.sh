#!/bin/bash

# Read JSON input
input=$(cat)

# Extract current directory from JSON (without jq)
cwd=$(echo "$input" | grep -o '"current_dir":"[^"]*"' | sed 's/"current_dir":"//;s/"$//')

# Get git branch info (skip optional locks for performance)
git_branch=""
if [ -n "$cwd" ] && git -C "$cwd" rev-parse --git-dir > /dev/null 2>&1; then
    branch=$(git -C "$cwd" --no-optional-locks branch --show-current 2>/dev/null)
    if [ -n "$branch" ]; then
        git_branch=" ($branch)"
    fi
fi

# Define colors using printf
BROWNISH_YELLOW=$(printf '\033[33m')
CYAN=$(printf '\033[36m')
NO_COLOR=$(printf '\033[0m')

# Build status line
printf "%s%s%s%s%s" \
    "$BROWNISH_YELLOW" "$cwd" \
    "$CYAN" "$git_branch" \
    "$NO_COLOR"
