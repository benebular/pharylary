import os
import glob

# Set the directory path
textgrid_dir = '/Volumes/cassandra/alldata/dissertation/vs/originals'  # <-- change this

# Find all TextGrid files
textgrid_files = glob.glob(os.path.join(textgrid_dir, '*.TextGrid'))

# Filter those ending in "_2.TextGrid"
duplicates = [os.path.basename(f) for f in textgrid_files if f.endswith('_2.TextGrid')]

# Print or save the list
print("Duplicate TextGrids ending in '_2':")
for f in duplicates:
    print(f)

# Optional: write to a text file
out_path = os.path.join(textgrid_dir, 'duplicate_textgrids.txt')
with open(out_path, 'w') as out_file:
    out_file.write('\n'.join(duplicates))

print(f"\nSaved list to {out_path}")
