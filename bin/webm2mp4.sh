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

# List target WEBM files in the directory
echo "The following WEBM files will be converted in the directory '$directory':"
find "$directory" -name "*.webm"

# Ask for user confirmation
read -p "Do you want to proceed? (y/n): " confirm

if [[ "$confirm" != "y" ]]; then
    echo "Aborting..."
    exit 0
fi

while IFS= read -r -d '' file; do
    output="${file%.webm}.mp4"

    if [ -f "$output" ]; then
        echo "Skipping $file (already converted: $output)"
        continue
    fi

    echo "Converting $file -> $output"
    ffmpeg -nostdin -i "$file" -c:v libx264 -c:a aac "$output"
done < <(find "$directory" -name "*.webm" -print0)
