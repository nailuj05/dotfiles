#!/bin/bash

# === Configuration ===
REMOTE="models.truphysics:/06 - Blender Library"
LOCAL="$HOME/work/truphysics/blender/library"
LOGFILE="$HOME/work/truphysics/blender/rclone_bisync_preview.log"

# === Dry Run ===
echo "🔍 Previewing bisync changes (dry-run)..."
rclone bisync "$REMOTE" "$LOCAL" --dry-run --verbose --exclude-from "$LOCAL/.rclone-ignore" | tee "$LOGFILE"

# === Confirmation Prompt ===
echo
read -p "⚠️ Do you want to proceed with the actual sync? (yes/no/discard): " CONFIRM

if [[ "$CONFIRM" =~ ^[Yy][Ee][Ss]$ ]]; then
    echo "✅ Proceeding with real bisync..."
    rclone bisync "$REMOTE" "$LOCAL" --exclude-from "$LOCAL/.rclone-ignore"
    echo "🎉 Sync complete."
elif [[ "$CONFIRM" =~ ^[Dd][Ii][Ss][Cc][Aa][Rr][Dd]$ ]]; then
    echo "✅ Discarding..."
    rclone sync "$REMOTE" "$LOCAL" --exclude-from "$LOCAL/.rclone-ignore"
    echo "🎉 Discarded."
else
    echo "❌ Sync aborted."
fi
