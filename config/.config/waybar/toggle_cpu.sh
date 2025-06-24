#!/bin/bash

# Get current governor
current_governor=$(cpupower frequency-info | grep -m 1 "governor" | awk '{print $4}')

# Check available governors
available_governors=$(cpupower frequency-info | grep "available cpufreq governors" | awk -F ': ' '{print $2}')

# Debugging output (can be removed later)
echo "Current governor: $current_governor"
echo "Available governors: $available_governors"

# Toggle between performance and powersave if available
if [[ "$available_governors" == *"performance"* && "$current_governor" == "performance" ]]; then
    echo "Switching to powersave..."
    sudo cpupower frequency-set -g powersave
elif [[ "$available_governors" == *"powersave"* && "$current_governor" == "powersave" ]]; then
    echo "Switching to performance..."
    sudo cpupower frequency-set -g performance
else
    echo "Error: One of the governors is not available or the current governor is not recognized."
    exit 1
fi
