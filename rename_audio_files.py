#!/usr/bin/env python3
"""
Script to rename audio files based on Trial and CarrierType from trials.csv
and update the CSV to reference the new names.
"""

import csv
import os
import shutil
from pathlib import Path

# Configuration
CSV_PATH = "data/trials.csv"
AUDIO_DIR = "data/audio"
OUTPUT_CSV = "data/trials_renamed.csv"

def generate_clean_filename(trial, carrier_type):
    """Generate a clean filename from Trial and CarrierType."""
    # Remove special characters and spaces, convert to lowercase
    trial_clean = str(trial).replace(" ", "_").replace("/", "-")
    carrier_clean = str(carrier_type).replace(" ", "_").replace("/", "-")
    
    # Create filename: trial_carriertype.wav
    filename = f"{trial_clean}_{carrier_clean}.wav"
    return filename

def main():
    # Read the CSV
    if not os.path.exists(CSV_PATH):
        print(f"Error: {CSV_PATH} not found")
        return
    
    if not os.path.exists(AUDIO_DIR):
        print(f"Error: {AUDIO_DIR} directory not found")
        return
    
    rows = []
    with open(CSV_PATH, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
    
    if not rows:
        print("No rows found in CSV")
        return
    
    # Check for required columns
    if 'Trial' not in rows[0] or 'CarrierType' not in rows[0]:
        print("Error: CSV must have 'Trial' and 'CarrierType' columns")
        print(f"Available columns: {list(rows[0].keys())}")
        return
    
    # Generate mapping
    print("=" * 60)
    print("AUDIO FILE RENAMING MAPPING")
    print("=" * 60)
    
    mapping = []
    
    for row in rows:
        trial = row.get('Trial', '')
        carrier_type = row.get('CarrierType', '')
        old_filename = row.get('TargetWordAudioFile', '')
        
        new_filename = generate_clean_filename(trial, carrier_type)
        
        mapping.append({
            'old': old_filename,
            'new': new_filename,
            'exists': os.path.exists(os.path.join(AUDIO_DIR, old_filename))
        })
        
        exists_str = "✓" if mapping[-1]['exists'] else "✗ (FILE NOT FOUND)"
        print(f"{len(mapping):3d}. {old_filename:50s} → {new_filename}")
        print(f"     {exists_str}")
    
    print("\n" + "=" * 60)
    print("NEXT STEPS:")
    print("=" * 60)
    print("\n1. Review the mapping above")
    print("2. If it looks correct, do ONE of the following:\n")
    print("   OPTION A - Manual rename:")
    print("   - Copy the 'new' names and rename files in data/audio/")
    print("   - Then run this script again with --update flag\n")
    print("   OPTION B - Automatic rename:")
    print("   - Uncomment the rename_files() call below in main()\n")
    
    # Check if --update flag is provided (for manual renames)
    import sys
    if '--update' in sys.argv:
        print("Updating CSV with new filenames...")
        update_csv(rows, mapping)
        print(f"✓ Updated CSV saved to {OUTPUT_CSV}")
        print("Replace data/trials.csv with the new file if satisfied")
        return
    
    # UNCOMMENT THE NEXT LINE TO ACTUALLY RENAME FILES:
    rename_files(mapping)

def update_csv(rows, mapping):
    """Update CSV with new filenames."""
    for i, row in enumerate(rows):
        row['TargetWordAudioFile'] = mapping[i]['new']
    
    # Write updated CSV
    if rows:
        fieldnames = list(rows[0].keys())
        with open(OUTPUT_CSV, 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(rows)

def rename_files(mapping):
    """Actually rename the audio files. UNCOMMENT TO USE."""
    print("\nRenaming files...")
    for item in mapping:
        old_path = os.path.join(AUDIO_DIR, item['old'])
        new_path = os.path.join(AUDIO_DIR, item['new'])
        
        if not os.path.exists(old_path):
            print(f"✗ {item['old']} - FILE NOT FOUND")
            continue
        
        try:
            shutil.move(old_path, new_path)
            print(f"✓ {item['old']} → {item['new']}")
        except Exception as e:
            print(f"✗ {item['old']} - ERROR: {e}")
    
    print("\nUpdating CSV...")
    rows = []
    with open(CSV_PATH, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
    
    for i, row in enumerate(rows):
        row['TargetWordAudioFile'] = mapping[i]['new']
    
    if rows:
        fieldnames = list(rows[0].keys())
        with open(OUTPUT_CSV, 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(rows)
    
    print(f"✓ Updated CSV saved to {OUTPUT_CSV}")


if __name__ == "__main__":
    main()
