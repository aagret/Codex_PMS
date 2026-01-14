#!/bin/bash

# Script to move useless files to ../useless/ directory
# Preserves the original folder structure

BASE_DIR="/Volumes/Data/Alexandre/PMS"
USELESS_DIR="/Volumes/Data/Alexandre/useless"

# Create useless directory if it doesn't exist
mkdir -p "$USELESS_DIR"

# Function to move file preserving directory structure
move_file() {
    local file="$1"
    local relative_path="${file#$BASE_DIR/}"
    local target_dir="$USELESS_DIR/$(dirname "$relative_path")"
    
    mkdir -p "$target_dir"
    mv "$file" "$target_dir/"
    echo "Moved: $relative_path"
}

echo "Starting to move useless files..."

# 1. Remove entire RStudio cache directories
if [ -d "$BASE_DIR/R/.Rproj.user" ]; then
    mkdir -p "$USELESS_DIR/R"
    mv "$BASE_DIR/R/.Rproj.user" "$USELESS_DIR/R/"
    echo "Moved: R/.Rproj.user/"
fi

# 2. Move .Rhistory files
find "$BASE_DIR" -type f -name ".Rhistory" | while read file; do
    move_file "$file"
done

# 3. Move duplicate R scripts in root
for file in formatPositions.R loadFunctions.R SQL.R; do
    if [ -f "$BASE_DIR/$file" ]; then
        move_file "$BASE_DIR/$file"
    fi
done

# 4. Move unused/commented functions in R/source/
for file in getCitePositions.R loadRequiredPackages.R readQuintetFiles.R uploadPsql.R calcFxCross.R extractDate.R extractDates.R formatQuintetFile.R.old; do
    if [ -f "$BASE_DIR/R/source/$file" ]; then
        move_file "$BASE_DIR/R/source/$file"
    fi
done

# 5. Move entire R/dev directory
if [ -d "$BASE_DIR/R/dev" ]; then
    mkdir -p "$USELESS_DIR/R"
    mv "$BASE_DIR/R/dev" "$USELESS_DIR/R/"
    echo "Moved: R/dev/"
fi

# 6. Move unused R scripts
if [ -f "$BASE_DIR/R/extractSecurities.R" ]; then
    move_file "$BASE_DIR/R/extractSecurities.R"
fi

# 7. Move all BankExtractions directory
if [ -d "$BASE_DIR/BankExtractions" ]; then
    mv "$BASE_DIR/BankExtractions" "$USELESS_DIR/"
    echo "Moved: BankExtractions/"
fi

# 8. Move Excel/Office temporary files
find "$BASE_DIR" -type f \( -name "~$*.xlsx" -o -name "~$*.docx" -o -name "Thumbs.db" \) | while read file; do
    move_file "$file"
done

# 9. Move temp files
for file in tempControl.csv tempControl.xlsx; do
    if [ -f "$BASE_DIR/$file" ]; then
        move_file "$BASE_DIR/$file"
    fi
done

# 10. Move webScrap.py
if [ -f "$BASE_DIR/webScrap.py" ]; then
    move_file "$BASE_DIR/webScrap.py"
fi

echo ""
echo "Done! All useless files moved to: $USELESS_DIR"
echo "You can review the moved files and delete the useless directory when ready."
