#!/bin/bash

directory=$1

# Check if directory argument is provided
if [ -z "$directory" ]; then
    echo "Usage: $0 <directory>"
    exit 1
fi

if [ ! -d "$directory" ]; then
    echo >&2 "failed: '$directory' is not a directory"
    exit 1
fi

if ! command -v ffmpeg > /dev/null; then
    echo >&2 "failed: cannot find ffmpeg"
    exit 1
fi

if ! ffmpeg -hide_banner -filters 2>/dev/null | grep -q drawtext; then
    echo >&2 "failed: ffmpeg has no 'drawtext' filter (needs a build with --enable-libfreetype)"
    echo >&2 "hint: brew tap homebrew-ffmpeg/ffmpeg && brew install homebrew-ffmpeg/ffmpeg/ffmpeg"
    exit 1
fi

output_dir="$directory/numbered"

# List target files in the directory
echo "The following files will be numbered (top-right corner) in the directory '$directory':"
find "$directory" -maxdepth 1 -type f \( -iname "*.mp4" -o -iname "*.mov" -o -iname "*.mkv" -o -iname "*.avi" -o -iname "*.webm" -o -iname "*.m4v" \) | sort

# Ask for user confirmation
read -p "Do you want to proceed? (y/n): " confirm

if [[ "$confirm" != "y" ]]; then
    echo "Aborting..."
    exit 0
fi

mkdir -p "$output_dir"

converted=()
skipped=()

n=1
while IFS= read -r -d '' file; do
    filename=$(basename "$file")
    output="$output_dir/$filename"

    if [ -f "$output" ]; then
        echo "Skipping $file (already numbered: $output)"
        skipped+=("$file")
        n=$((n + 1))
        continue
    fi

    echo "Numbering $file -> $output (n=$n)"
    ffmpeg -nostdin -i "$file" -vf "drawtext=text='$n':x=w-tw-40:y=40:fontsize=h/10:fontcolor=white:borderw=8:bordercolor=black" "$output"
    converted+=("$file")
    n=$((n + 1))
done < <(find "$directory" -maxdepth 1 -type f \( -iname "*.mp4" -o -iname "*.mov" -o -iname "*.mkv" -o -iname "*.avi" -o -iname "*.webm" -o -iname "*.m4v" \) -print0 | sort -z)

echo
echo "Numbered files:"
if [ "${#converted[@]}" -eq 0 ]; then
    echo "  (none)"
else
    printf '  %s\n' "${converted[@]}"
fi

echo "Skipped files:"
if [ "${#skipped[@]}" -eq 0 ]; then
    echo "  (none)"
else
    printf '  %s\n' "${skipped[@]}"
fi
