#!/bin/bash

# Set default values
default_maxsize=300  # Default maxsize value (in KB)
default_quality=70  # Default quality value
max_attempts=3  # Maximum number of attempts

directory=$1
quality=${2:-$default_quality}
maxsize_kb=${default_maxsize}

# Convert KB to bytes (1KB = 1024 bytes)
maxsize=$(($maxsize_kb * 1024))

# Check if directory argument is provided
if [ -z "$directory" ]; then
    echo "Usage: $0 <directory> [quality]"
    exit 1
fi

if ! command -v mogrify > /dev/null; then
    echo >&2 "failed: cannot find mogrify"
    exit 1
fi

if ! command -v convert > /dev/null; then
    echo >&2 "failed: cannot find convert"
    exit 1
fi

if ! command -v identify > /dev/null; then
    echo >&2 "failed: cannot find identify"
    exit 1
fi

# List target JPG files in the directory
echo "The following JPG files will be processed in the directory '$directory':"
find "$directory" -name "*.jpg"

# Ask for user confirmation
read -p "Do you want to proceed? (y/n): " confirm

if [[ "$confirm" != "y" ]]; then
    echo "Aborting..."
    exit 0
fi

# Detect the operating system and set the appropriate stat command
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    stat_cmd="stat -f%z"
else
    # Linux
    stat_cmd="stat --format=%s"
fi

# Initialize a list for files that are still larger than maxsize
oversized_files=()

# Check the file size and compress if it exceeds maxsize
for img in *.jpg; do
    width=$(identify -format "%w" "$img")  # Get the image width using ImageMagick's identify command
    if [ "$width" -gt 1600 ]; then
        echo "Resizing $img (original width: $width pixels)"
        mogrify -resize 1600\> "$img"  # Resize the image to a width of 1600 pixels, maintaining aspect ratio
    else
        echo "$img width is already under 1600 pixels"
    fi

    filesize=$($stat_cmd "$img")  # Get the file size

    if [ "$filesize" -gt "$maxsize" ]; then
        echo "Compressing $img (original size: $filesize bytes)"
        magick "$img" -quality "$quality" "$img"  # Save with the specified quality
        new_filesize=$($stat_cmd "$img")  # Get the new file size

        # Check if the new size is still larger than maxsize
        if [ "$new_filesize" -gt "$maxsize" ]; then
            oversized_files+=("$img")
        fi
    else
        echo "$img is already under the specified $maxsize"
    fi
done

# Print the list of oversized files
if [ ${#oversized_files[@]} -gt 0 ]; then
    echo "The following images are still larger than $maxsize bytes:"
    for file in "${oversized_files[@]}"; do
        echo "$file"
    done
else
    echo "All images are within the specified max size."
fi
