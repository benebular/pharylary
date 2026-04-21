import os
import re

directory = '/Volumes/cassandra/alldata/dissertation/1A/grids/3_phary-note'

def get_tiers(file_path):
    try:
        with open(file_path, 'r', encoding='utf-16', errors='ignore') as f:
            content = f.read()
        return re.findall(r'name\s*=\s*\"([^\"]+)\"', content)
    except Exception:
        return None

print(f'\nScanning: {directory}\n' + '-'*40)

files = sorted([f for f in os.listdir(directory) if f.endswith('.TextGrid')])
low_tier_files = []

if not files:
    print('No .TextGrid files found in that directory.')
else:
    for filename in files:
        tier_names = get_tiers(os.path.join(directory, filename))
        
        if tier_names is None:
            print(f'File: {filename} - [Error reading file]')
            continue
            
        tier_count = len(tier_names)
        print(f'File: {filename} ({tier_count} tiers)')
        
        # Track files with fewer than 9 tiers
        if tier_count < 9:
            low_tier_files.append((filename, tier_count))
            
        for i, name in enumerate(tier_names, 1):
            print(f'  {i}: {name}')
        print('-' * 20)

    # Summary Section
    print('\n' + '='*40)
    print('SUMMARY: FILES WITH LESS THAN 9 TIERS')
    print('='*40)
    if not low_tier_files:
        print('All files have 9 or more tiers.')
    else:
        for name, count in low_tier_files:
            print(f'[!] {name:30} | Tiers: {count}')
    print('='*40 + '\n')