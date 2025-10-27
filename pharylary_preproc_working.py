### pharylary preprocessing script
# this script takes individual outputs from VoiceSauce and different tiers from TextGrids
# to create individual entries of acoustic values at each millisecond according to the different intervals in the TextGrid tiers

import pandas as pd
import numpy as np
import os
import glob
import textgrid
import time
from tqdm.auto import tqdm
from pathlib import Path
import soundfile as sf
import unicodedata
from textgrid import TextGrid, IntervalTier, Interval

# output_dir = '/Volumes/circe/alldata/dissertation/vs/output_preproc'
# textgrid_folder = '/Volumes/circe/alldata/dissertation/vs/input_preproc'
# vs_output_dir = '/Volumes/circe/alldata/dissertation/vs/vs_output'
# fricative_output_dir = '/Volumes/circe/alldata/dissertation/vs/fricative_output'

output_dir = '/Volumes/cassandra/alldata/dissertation/vs/output_preproc'
textgrid_folder = '/Volumes/cassandra/alldata/dissertation/vs/input_preproc'
vs_output_dir = '/Volumes/cassandra/alldata/dissertation/vs/vs_output'
fricative_output_dir = '/Volumes/cassandra/alldata/dissertation/vs/fricative_output'

os.chdir(output_dir)

# vs_output = pd.read_csv(os.path.join(output_dir, 'output.txt'), sep='\t')
# tg = textgrid.TextGrid.fromFile(os.path.join(input_dir, 'Y0395_expt_1_1_1.TextGrid'))
# point_tier = tg.getFirst('comment')

# Find all .txt files in the vs_output_dir
voicesauce_files = glob.glob(os.path.join(vs_output_dir, '*.txt'))

# Initialize an empty DataFrame
voicesauce_data = pd.DataFrame()

# Loop through each file and append to voicesauce_data
for file in voicesauce_files:
    try:
        # Try utf-8 first
        temp_df = pd.read_csv(file, sep='\t', encoding='utf-8')
    except UnicodeDecodeError:
        # Fallback to latin1 if utf-8 fails
        temp_df = pd.read_csv(file, sep='\t', encoding='latin1')
    
    voicesauce_data = pd.concat([voicesauce_data, temp_df], ignore_index=True)

## remove SD0009 temporarily due to missing label
# voicesauce_data = voicesauce_data[~voicesauce_data['Filename'].str.contains('SD0009', na=False)]
# voicesauce_data = voicesauce_data[~voicesauce_data['Filename'].str.contains('SD0015', na=False)]
# voicesauce_data = voicesauce_data[~voicesauce_data['Filename'].str.contains('SD0024', na=False)]

# Save the concatenated data as a CSV
print("Saving voicesauce_data.csv...")
output_csv = os.path.join(output_dir, 'voicesauce_data.csv')
voicesauce_data.to_csv(output_csv, index=False)


### LOOP ####
# use inidividual extracted phrasenum textgrids


# Initialize an empty DataFrame to store all relevant data
# all_relevant_data = pd.DataFrame()

# List or dictionary of tiers to process
tiers_to_process = ['phonetic', 'glottis', 'V-sequence', 'word']

time_start = time.ctime()
print("Start time:", time_start)
seconds_start = time.time()

# Iterate over each file in the TextGrid folder
for filename in os.listdir(textgrid_folder):
    if filename.endswith('.TextGrid'):  # Check if the file is a TextGrid file
        filepath = os.path.join(textgrid_folder, filename)
        tg_basename = os.path.splitext(filename)[0]
        tg = textgrid.TextGrid.fromFile(filepath)
        print ('Extracting and appending %s TextGrid labels.'%(filename))
        # # Identify your point tier
        point_tier = tg.getFirst('comment')

        # Assign variables for each tier
        # print("Assigning tier variables...")
        tiers = {tier_name: tg.getFirst(tier_name) for tier_name in tiers_to_process}
        phrasenum_tier = tg.getFirst('phrasenum')

        # Build expected output filename for this TextGrid
        output_csv = os.path.join(output_dir, 'relevant_data', f"{os.path.splitext(filename)[0]}_relevant.csv")

        # Skip if CSV already exists
        if os.path.exists(output_csv):
            print(f"Skipping {filename} — output already exists at {output_csv}")
            continue

        # Subset the VoiceSauce output for the current phrasenum
        # print("Subsetting...")
        voicesauce_grid_data = voicesauce_data[voicesauce_data['Filename'].str.contains(tg_basename, na=False)]

        # Initialize an empty DataFrame to store all relevant data
        phrase_relevant_data = pd.DataFrame()
        all_relevant_data = pd.DataFrame()

        # Process each phrasenum interval
        for phrasenum_interval in phrasenum_tier:
            if phrasenum_interval.mark is not str(''):
                phrasenum_value = phrasenum_interval.mark

                # Subset the VoiceSauce output for the current phrasenum
                # print("Subsetting...")
                subset_voicesauce = voicesauce_grid_data[voicesauce_grid_data['Label'] == int(phrasenum_value)]

                # Process each tier
                # print("Processing...")
                for tier_name, tier in tiers.items():
                    for interval in tier:
                        if interval.mark != '':
                            start_time = interval.minTime *1000 # these originally have a multiplier for 1000, unsure why, but check later pipeline
                            end_time = interval.maxTime *1000

                            # Extract relevant data from the subsetted VoiceSauce output
                            relevant_data = subset_voicesauce[(subset_voicesauce['t_ms'] >= start_time) & (subset_voicesauce['t_ms'] <= end_time)].copy()

                            # Find points within the phrasenum interval
                            points_in_interval = [point.mark for point in point_tier if phrasenum_interval.minTime <= point.time <= phrasenum_interval.maxTime and point.mark]

                            # Add the point marks as a new column
                            relevant_data['comments'] = ', '.join(points_in_interval)

                            # Add a column with the interval's .mark value
                            relevant_data['interval'] = interval.mark
                            relevant_data['tier'] = tier.name

                            # Reorder the columns
                            cols = relevant_data.columns.tolist()
                            cols.insert(2, cols.pop(cols.index('interval')))
                            cols.insert(3, cols.pop(cols.index('tier')))
                            cols.insert(4, cols.pop(cols.index('comments')))
                            relevant_data = relevant_data[cols]

                            # # Save this TextGrid's relevant_data to CSV
                            # relevant_data.to_csv(output_csv, index=False)
                            # print(f"Saved {len(relevant_data)} rows to {output_csv}")

                            # Append the relevant data to the DataFrame
                            phrase_relevant_data = pd.concat([phrase_relevant_data, relevant_data], ignore_index=True)

        all_relevant_data = pd.concat([all_relevant_data, phrase_relevant_data], ignore_index=True)

        all_relevant_data.to_csv(output_csv, index=False)

time_end = time.ctime()
print("End time:", time_end)
seconds_end = time.time()
print("Time elapsed (minutes): ", (seconds_end-seconds_start)/60)


### code here that concatenates all the relevant_data

# directory containing the individual CSVs
csv_dir = os.path.join(output_dir, "relevant_data")
csv_files = glob.glob(os.path.join(csv_dir, "*.csv"))

print(f"Found {len(csv_files)} CSV files to combine.")

# -----------------------------
# 1. Load all CSVs with progress
# -----------------------------
df_list = []
for f in tqdm(csv_files, desc="Loading CSVs", unit="file"):
    try:
        df = pd.read_csv(f)
        df_list.append(df)
    except Exception as e:
        print(f"⚠️ Skipping {f} due to error: {e}")

# -----------------------------
# 2. Concatenate with progress
# -----------------------------
if df_list:
    print("Concatenating CSVs into all_relevant_data...")
    all_relevant_data = pd.DataFrame()
    # simulate progress by adding DataFrames one by one
    with tqdm(total=len(df_list), desc="Concatenating", unit="file") as pbar:
        for df in df_list:
            all_relevant_data = pd.concat([all_relevant_data, df], ignore_index=True)
            pbar.update(1)

    print(f"✅ Combined {len(df_list)} files — total rows: {len(all_relevant_data):,}")

    # Save the combined dataset
    combined_path = os.path.join(output_dir, "all_relevant_data.csv")
    all_relevant_data.to_csv(combined_path, index=False)
    print(f"Saved combined dataset to {combined_path}")

else:
    all_relevant_data = pd.DataFrame()
    print("No CSV files were loaded; all_relevant_data is empty.")

# # Initialize an empty DataFrame to store all relevant data
# all_relevant_data = pd.DataFrame()

# # List or dictionary of tiers to process
# tiers_to_process = ['phonetic', 'glottis', 'V-sequence', 'word']

# time_start = time.ctime()
# print("Start time:", time_start)
# seconds_start = time.time()

# # Iterate over each file in the TextGrid folder
# for filename in os.listdir(textgrid_folder):
#     if filename.endswith('.TextGrid'):  # Check if the file is a TextGrid file
#         filepath = os.path.join(textgrid_folder, filename)
#         tg = textgrid.TextGrid.fromFile(filepath)
#         print ('Extracting and appending %s TextGrid labels.'%(filename))
#         # # Identify your point tier
#         point_tier = tg.getFirst('comment')

#         # Assign variables for each tier
#         # print("Assigning tier variables...")
#         tiers = {tier_name: tg.getFirst(tier_name) for tier_name in tiers_to_process}
#         phrasenum_tier = tg.getFirst('phrasenum')

#         # Build expected output filename for this TextGrid
#         output_csv = os.path.join(output_dir, f"{os.path.splitext(filename)[0]}_relevant.csv")

#         # Skip if CSV already exists
#         if os.path.exists(output_csv):
#             print(f"Skipping {filename} — output already exists at {output_csv}")
#             continue

#         # Process each phrasenum interval
#         for phrasenum_interval in phrasenum_tier:
#             if phrasenum_interval.mark is not str(''):
#                 phrasenum_value = phrasenum_interval.mark

#                 # Subset the VoiceSauce output for the current phrasenum
#                 # print("Subsetting...")
#                 subset_voicesauce = voicesauce_data[voicesauce_data['Label'] == int(phrasenum_value)]

#                 # Process each tier
#                 # print("Processing...")
#                 for tier_name, tier in tiers.items():
#                     for interval in tier:
#                         if interval.mark != '':
#                             start_time = interval.minTime *1000 # these originally have a multiplier for 1000, unsure why, but check later pipeline
#                             end_time = interval.maxTime *1000

#                             # Extract relevant data from the subsetted VoiceSauce output
#                             relevant_data = subset_voicesauce[(subset_voicesauce['t_ms'] >= start_time) & (subset_voicesauce['t_ms'] <= end_time)].copy()

# 							# Find points within the phrasenum interval
#                             points_in_interval = [point.mark for point in point_tier if phrasenum_interval.minTime <= point.time <= phrasenum_interval.maxTime and point.mark]

# 							# Add the point marks as a new column
#                             relevant_data['comments'] = ', '.join(points_in_interval)

# 							# Add a column with the interval's .mark value
#                             relevant_data['interval'] = interval.mark
#                             relevant_data['tier'] = tier.name

#                             # Reorder the columns
#                             cols = relevant_data.columns.tolist()
#                             cols.insert(2, cols.pop(cols.index('interval')))
#                             cols.insert(3, cols.pop(cols.index('tier')))
#                             cols.insert(4, cols.pop(cols.index('comments')))
#                             relevant_data = relevant_data[cols]

#                             # # Save this TextGrid's relevant_data to CSV
#                             # relevant_data.to_csv(output_csv, index=False)
#                             # print(f"Saved {len(relevant_data)} rows to {output_csv}")

#                             # Append the relevant data to the DataFrame
#                             all_relevant_data = pd.concat([all_relevant_data, relevant_data], ignore_index=True)

# all_relevant_data.to_csv('/Volumes/circe/alldata/dissertation/vs/output_preproc/all_relevant_data.csv', index=False)

# time_end = time.ctime()
# print("End time:", time_end)
# seconds_end = time.time()
# print("Time elapsed (minutes): ", (seconds_end-seconds_start)/60)


#### MORE PREPROC #####

print('Processing all data for min, max, proportion, etc.')
all_relevant_data = pd.read_csv(os.path.join(output_dir, "all_relevant_data.csv"), encoding='utf-8')

# Drop the 'Label' column
all_relevant_data.drop(['Label','Unnamed: 72'], axis=1, inplace=True)

# Split the 'Filename' column into separate parts
filename_parts = all_relevant_data['Filename'].str.split('_', expand=True)

# Assign new columns based on the split parts
# Assuming the format is consistent and the desired parts are at specific indices
all_relevant_data['participant'] = filename_parts[0]
all_relevant_data['session'] = filename_parts[2]
all_relevant_data['phrase'] = filename_parts[3]
# all_relevant_data['phrase'] = filename_parts[4].str.extract(r'(\d+)(?=\.mat)')


# Determine the position of the 'comments' column
comments_col_idx = all_relevant_data.columns.get_loc('interval') + 2

# Reorder columns to place the new columns after 'comments'
cols = all_relevant_data.columns.tolist()
participant_idx = cols.index('participant')
session_idx = cols.index('session')
phrase_num_idx = cols.index('phrase')

# Move 'participant', 'session', and 'phrase_num' after 'comments'
cols.insert(comments_col_idx, cols.pop(participant_idx))
cols.insert(comments_col_idx + 1, cols.pop(session_idx))
cols.insert(comments_col_idx + 2, cols.pop(phrase_num_idx))

# Reassign reordered columns to DataFrame
all_relevant_data = all_relevant_data[cols]

## find min and max for each interval
# tier_min_max = all_relevant_data.groupby(['phrase', 'tier', 'interval'])['t_ms'].agg([min, max]).reset_index()
# energy_min_max = all_relevant_data.groupby(['phrase','tier','interval'])['Energy'].agg([min,max]).reset_index()

# tier_min_max = (
#     all_relevant_data
#     .groupby(['phrase', 'tier', 'interval'])['t_ms']
#     .agg(['min', 'max'])
#     .reset_index()
# )

# energy_min_max = (
#     all_relevant_data
#     .groupby(['phrase', 'tier', 'interval'])['Energy']
#     .agg(['min', 'max'])
#     .reset_index()
# )

# ## merge to df
# all_relevant_data = all_relevant_data.merge(tier_min_max, on=['phrase', 'tier', 'interval'])
# ## rename the max and min column
# all_relevant_data = all_relevant_data.rename(columns={'min':'t_min','max':'t_max'})

# ## same as above, except for RMS to normalize within participants
# all_relevant_data = all_relevant_data.merge(energy_min_max, on=['phrase', 'tier', 'interval'])
# all_relevant_data = all_relevant_data.rename(columns={'min':'energy_min','max':'energy_max'})

# Ensure numeric (optional but helps if there are stray strings)
all_relevant_data['t_ms']  = pd.to_numeric(all_relevant_data['t_ms'], errors='coerce')
all_relevant_data['Energy'] = pd.to_numeric(all_relevant_data['Energy'], errors='coerce')

g = all_relevant_data.groupby(['phrase', 'tier', 'interval'])

# t_ms min/max per (phrase, tier, interval)
all_relevant_data['t_min'] = g['t_ms'].transform('min')
all_relevant_data['t_max'] = g['t_ms'].transform('max')

# Energy min/max per (phrase, tier, interval)
all_relevant_data['energy_min'] = g['Energy'].transform('min')
all_relevant_data['energy_max'] = g['Energy'].transform('max')


# ### proportion column
# all_relevant_data['t_prop'] = (all_relevant_data['t_ms'] - all_relevant_data['t_min']) / (all_relevant_data['t_max'] - all_relevant_data['t_min'])
# all_relevant_data['t_prop'] = all_relevant_data['t_prop'] * 100

# all_relevant_data['energy_prop'] = (all_relevant_data['Energy'] - all_relevant_data['energy_min']) / (all_relevant_data['energy_max'] - all_relevant_data['energy_min'])
# # all_relevant_data['energy_prop'] = all_relevant_data['energy_prop'] * 100

# ### duration column
# all_relevant_data['duration'] = all_relevant_data['t_max'] - all_relevant_data['t_min']

# Group once
g = all_relevant_data.groupby(['phrase', 'tier', 'interval'], group_keys=False)

# Precompute range denominators to avoid division repetition
t_range = g['t_ms'].transform('max') - g['t_ms'].transform('min')
e_range = g['Energy'].transform('max') - g['Energy'].transform('min')

# Compute proportions (with safe division)
all_relevant_data['t_prop'] = ((all_relevant_data['t_ms'] - g['t_ms'].transform('min')) / t_range) * 100
all_relevant_data['energy_prop'] = (all_relevant_data['Energy'] - g['Energy'].transform('min')) / e_range

# Compute duration per group
all_relevant_data['duration'] = t_range

# (optional) replace inf/nan if any denominators were zero
all_relevant_data[['energy_prop']] = (
    all_relevant_data[['energy_prop']].fillna(0)
)

### add in other data from google sheet ###
print('Adding metadata...')
stim_meta = pd.read_csv('/Users/bcl/GitHub/pharylary/PharyLary Stimuli - pharylary2.csv')
stim_meta_df = stim_meta[['Phrasenum','IPA','Gloss','Gloss_2','Syllable','Segment','Type','Position','Position_2','Contrast_(IPA)','Contrast','Experiment']]
stim_meta_df = stim_meta_df.rename(columns={'Phrasenum':'phrase'})
stim_meta_df = stim_meta_df.astype({'phrase': int})
all_relevant_data = all_relevant_data.astype({'phrase': int})
all_data = pd.merge(all_relevant_data, stim_meta_df, on='phrase', how='inner')

# Output the final DataFrame
# print(all_relevant_data)
# Optionally, save to a file
# print('Saving as .csv in %s.'%(output_dir))
# all_data.to_csv('preproc_output.csv', index=False)

### NEW SECTION: Import and concatenate fricative measurement data ###

### loop to prep files into individual folders for use with multitaper.R. all of the files are made from the individual textgrids in originals


# ====== USER SETTINGS ======
data_dir   = textgrid_folder                               # folder with .TextGrid + matching .wav basenames
out_root   = os.path.join(output_dir, "fricatives")
# Choose how you pick fricative intervals:
TIER_FILTER_INDEX = 0      # e.g., 1 for your "fricatives" tier (0-based); set to None if you’ll filter by name
TIER_FILTER_NAME  = None   # or e.g., "fricatives"; set to None if using index

# Labels to extract (each goes to its own subfolder inside out_root)
labels = ["s", "sˤ", "ħ", "h", "ʕ", "ʁ", "ð", "ðħ", "sʕ","ðˤ"]

# The two tiers to copy into the per-clip TextGrid (by NAME)
COPY_TIER_NAMES = ["phonetic", "phrasenum"]
# ===========================

# Ensure per-label subfolders exist
for lab in labels:
    os.makedirs(os.path.join(out_root, lab), exist_ok=True)

labels_set = set(unicodedata.normalize("NFC", l.strip()) for l in labels)

def nfc_strip(s):
    return unicodedata.normalize("NFC", (s or "").strip())

def find_wav(base, directory):
    """Find matching WAV (case-insensitive)."""
    for ext in (".wav", ".WAV", ".Wav"):
        path = os.path.join(directory, f"{base}{ext}")
        if os.path.exists(path):
            return path
    return None

def out_name(base, label, idx=None):
    lab = label.replace("/", "_")
    return f"{base}_{lab}.wav" if idx is None else f"{base}_{lab}-{idx:02d}.wav"

def crop_and_shift_tier(src_tier: IntervalTier, start_s: float, end_s: float, new_name: str) -> IntervalTier:
    """
    Copy intervals from src_tier that overlap [start_s, end_s], crop to that window,
    and shift so the new min time = 0.0.
    """
    clip_len = float(end_s - start_s)
    new_tier = IntervalTier(name=new_name, minTime=0.0, maxTime=clip_len)

    # Some TextGrid writers leave a final "background" interval; we’ll still crop it.
    for itv in getattr(src_tier, "intervals", []):
        a, b = float(itv.minTime), float(itv.maxTime)
        if b <= start_s or a >= end_s:
            continue  # no overlap

        # overlap cropped to the clip, re-based to 0
        na = max(a, start_s) - start_s
        nb = min(b, end_s) - start_s
        if nb > na:
            new_tier.addInterval(Interval(na, nb, itv.mark))

    # Ensure bounds are consistent
    new_tier.minTime = 0.0
    new_tier.maxTime = clip_len
    return new_tier


def build_clip_textgrid(full_tg: TextGrid, start_s: float, end_s: float, copy_names: list[str]) -> TextGrid:
    """
    Build a new TextGrid with ONLY the tiers named in copy_names,
    cropped to [start_s, end_s] and shifted to start at 0.0.
    """
    clip_len = float(end_s - start_s)
    new_tg = TextGrid(minTime=0.0, maxTime=clip_len)

    # Map existing tiers by normalized name
    import unicodedata
    def nfc_strip(s):
        return unicodedata.normalize("NFC", (s or "").strip())

    name2tier = {nfc_strip(getattr(t, "name", "")): t for t in full_tg.tiers}

    for wanted in copy_names:
        key = nfc_strip(wanted)
        src = name2tier.get(key, None)

        if src is None or not hasattr(src, "intervals"):
            # Create an empty placeholder IntervalTier if missing or it’s a PointTier
            new_tg.append(IntervalTier(name=wanted, minTime=0.0, maxTime=clip_len))
        else:
            new_tg.append(crop_and_shift_tier(src, start_s, end_s, new_name=wanted))

    new_tg.minTime = 0.0
    new_tg.maxTime = clip_len
    return new_tg


# Gather TextGrids
tg_paths = sorted(glob.glob(os.path.join(data_dir, "*.TextGrid")))
if not tg_paths:
    print(f"No TextGrid files found in {data_dir}")

total_written = 0
missing_wavs = 0

for tg_path in tg_paths:
    base = os.path.splitext(os.path.basename(tg_path))[0]
    wav_path = find_wav(base, data_dir)
    if wav_path is None:
        print(f"[WARN] Skipping {base}: no matching WAV")
        missing_wavs += 1
        continue

    # Load audio
    audio, sr = sf.read(wav_path, always_2d=False)
    n_samples = len(audio) if getattr(audio, "ndim", 1) == 1 else audio.shape[0]

    # Load TextGrid
    tg = TextGrid.fromFile(tg_path)

    # Choose the fricatives tier where we'll find intervals to extract
    if TIER_FILTER_INDEX is not None:
        if 0 <= TIER_FILTER_INDEX < len(tg.tiers):
            fric_tier = tg.tiers[TIER_FILTER_INDEX]
        else:
            print(f"[WARN] {os.path.basename(tg_path)}: tier index {TIER_FILTER_INDEX} out of range (has {len(tg.tiers)}).")
            continue
    elif TIER_FILTER_NAME is not None:
        found = [t for t in tg.tiers if nfc_strip(getattr(t, "name", "")) == nfc_strip(TIER_FILTER_NAME)]
        if not found:
            print(f"[WARN] {os.path.basename(tg_path)}: tier '{TIER_FILTER_NAME}' not found.")
            continue
        fric_tier = found[0]
    else:
        # If neither set, default to first tier
        fric_tier = tg.tiers[0]

    # Collect matches per label from the chosen tier
    by_label = {lab: [] for lab in labels_set}
    intervals = getattr(fric_tier, "intervals", [])
    for itv in intervals:
        lab = nfc_strip(itv.mark)
        if lab in labels_set and itv.minTime < itv.maxTime:
            by_label[lab].append((float(itv.minTime), float(itv.maxTime)))

    # Write clips + companion TextGrids
    for lab, spans in by_label.items():
        if not spans:
            continue
        subdir = os.path.join(out_root, lab)
        for i, (start_s, end_s) in enumerate(spans, start=1):
            # sample indices (clamped)
            start_i = max(0, min(int(round(start_s * sr)), n_samples))
            end_i   = max(0, min(int(round(end_s * sr)), n_samples))
            if end_i <= start_i:
                continue

            # 1) Write audio clip
            clip = audio[start_i:end_i]
            wav_name = out_name(base, lab, None if len(spans) == 1 else i)
            wav_out  = os.path.join(subdir, wav_name)
            sf.write(wav_out, clip, sr, subtype="PCM_16")

            # 2) Build and write companion TextGrid
            clip_tg  = build_clip_textgrid(tg, start_s, end_s, COPY_TIER_NAMES)
            tg_name  = os.path.splitext(wav_name)[0] + ".TextGrid"
            tg_out   = os.path.join(subdir, tg_name)
            clip_tg.write(tg_out)

            print(f"✔ {lab}: {wav_name} & {tg_name}  {(end_i - start_i)/sr:.3f}s")
            total_written += 1

print("\nSummary")
print("=======")
print(f"TextGrids processed: {len(tg_paths)}")
print(f"Missing WAVs: {missing_wavs}")
print(f"Total clips written: {total_written}")
print(f"Output root: {out_root}")


# ---- CONFIG ----
csv_dir = os.path.join(output_dir, "fricatives")  # <- folder containing the per-TextGrid CSVs
# If your CSVs are in output_dir directly, set: csv_dir = output_dir

# ---- Helper: robust parser for stim filenames ----
def parse_stim(stim: str):
    """
    Expected format: SD0001_expt_1_100_1_ð.wav
      - participant = first token before first '_':           'SD0001'
      - phrase      = 4th token (index 3):                    '100'
      - Segment     = last token before extension:            'ð'
    Returns (participant, phrase, Segment) as strings, or (None, None, None) if malformed.
    """
    if not isinstance(stim, str):
        return None, None, None
    base = os.path.basename(stim)
    if base.lower().endswith(".wav"):
        base = base[:-4]  # drop .wav
    parts = base.split("_")
    if len(parts) < 5:
        return None, None, None
    participant = parts[0]
    phrase = parts[3]                 # 0:part, 1:expt, 2:1, 3:phrase, 4:trial, -1:Segment
    segment = parts[-1]
    return str(participant), str(phrase), str(segment)

# ---- Build one metadata table from all CSVs ----
meta_rows = []
csv_files = glob.glob(os.path.join(csv_dir, "*.csv"))
print(f"Found {len(csv_files)} CSV(s) in {csv_dir}")

for fpath in csv_files:
    try:
        df = pd.read_csv(fpath)
    except Exception as e:
        print(f"Skipping {fpath}: {e}")
        continue

    if "stim" not in df.columns:
        print(f"Skipping {fpath}: missing 'stim' column")
        continue

    # Iterate rows in this CSV and extract keys + payload
    for _, row in df.iterrows():
        participant, phrase, segment = parse_stim(row["stim"])
        if participant is None:
            continue

        # Carry all other columns from this row (except 'stim') into the payload
        payload = row.drop(labels=["stim"]).to_dict()

        meta_rows.append({
            "participant": participant,
            "phrase": phrase,
            "interval": segment,
            **payload
        })

# Assemble metadata DataFrame; ensure one row per (participant, phrase, Segment)
metadata = pd.DataFrame(meta_rows)
if metadata.empty:
    print("No metadata rows parsed; leaving all_data unchanged.")
else:
    # If any duplicates slipped in, keep last
    metadata = metadata.drop_duplicates(subset=["participant", "phrase", "interval"], keep="last")

    # ---- Align dtypes to match all_data for a clean merge ----
    # Convert keys to string on both sides (adjust if you prefer ints for 'phrase')
    for col in ["participant", "phrase", "interval"]:
        if col in metadata:
            metadata[col] = metadata[col].astype(str)
        if col in all_data:
            all_data[col] = all_data[col].astype(str)

    # ---- Merge onto all_data ----
    # Left-merge so every row in all_data is retained; metadata columns are appended
    all_data = all_data.merge(metadata, on=["participant", "phrase", "interval"], how="left")

    print(f"Merged metadata onto all_data. New shape: {all_data.shape}")

# Output the final DataFrame
# print(all_relevant_data)
# Optionally, save to a file
print('Saving as .csv in %s.'%(output_dir))
all_data.to_csv('preproc_output.csv', index=False)


# ### old code using the extractions from the dicanio script
# # Step 1: Locate all text files in the fricative_output_dir ending with '_logfile_s'
# fricative_files = glob.glob(os.path.join(fricative_output_dir, '*.txt'))

# # Step 2: Read and concatenate these files into a single DataFrame
# fricative_data = pd.DataFrame()  # Initialize an empty DataFrame

# for file in fricative_files:
#     # Read each file
#     temp_df = pd.read_csv(file, sep='\t')  # Assuming tab-separated files
#     fricative_data = pd.concat([fricative_data, temp_df], ignore_index=False)

# # Step 3: Output the concatenated DataFrame for verification
# print(f"Concatenated fricative data contains {len(fricative_data)} rows and {len(fricative_data.columns)} columns.")

# # Subset the data based on matching phrasenum values
# phrasenum_values = [1, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 37, 39, 41, 43, 45, 47, 49, 50, 53, 55, 57, 59, 60, 62, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 134, 135, 136, 137, 138, 139, 141, 142, 143, 145, 146, 149, 150, 156, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172]

# # Convert phrasenum_values to string to match the object type in fricative_data
# phrasenum_values = list(map(str, phrasenum_values))

# # Filter rows based on phrasenum_values
# fricative_data = fricative_data[fricative_data['phrasenum'].astype(str).isin(phrasenum_values)]

# # Output the subset DataFrame for verification
# print(f"Subset fricative data contains {len(fricative_data)} rows after filtering by phrasenum.")

# # Print the count of total phrasenum values by label
# label_phrasenum_counts = fricative_data.groupby('label')['phrasenum'].nunique()
# print("Counts of unique phrasenum values by label:")
# print(label_phrasenum_counts)

# #### ADD HERE SECTION for matching the above fricatives with the metadata.
# ### each row in the fricative thing should already be a target phoneme, then export

# # Load the metadata CSV file
# metadata_path = os.path.join('/Users/bcl/GitHub/pharylary/PharyLary Stimuli - pharylary2.csv')  # Update the path to your metadata file
# metadata = pd.read_csv(metadata_path)
# metadata = metadata[['Phrasenum','IPA','Gloss','Gloss_2','Syllable','Segment','Type','Position','Position_2','Contrast_(IPA)','Contrast','Experiment']]

# # Ensure the Phrasenum column in metadata is of the same type as the phrasenum column in fricative_data
# metadata['Phrasenum'] = metadata['Phrasenum'].astype(str)
# fricative_data['phrasenum'] = fricative_data['phrasenum'].astype(str)

# # Merge fricative_data with metadata on phrasenum/Phrasenum
# fricative_data = pd.merge(
#     fricative_data,
#     metadata,
#     how='left',  # Use 'left' join to preserve the shape of fricative_data
#     left_on='phrasenum',
#     right_on='Phrasenum'
# )

# # Output the resulting DataFrame for verification
# print(f"fricative_data now contains {len(fricative_data)} rows and {len(fricative_data.columns)} columns after merging with metadata.")

# ### Save the final subset data ###
# fricative_data.to_csv(os.path.join(output_dir, 'subset_fricative_data.csv'), index=False)


#### matches for means ####
##### data for the intervals as extracted from overlap with tier 3 because there's no unique labels in tier 1.
##### this makes it so the data is just the target phonemes, not all of them


### new method

# t0 = time.time()

# # Ensure comparable dtypes
# all_data = all_data.copy()
# all_data['phrase'] = all_data['phrase'].astype(str)
# all_data['t_ms']   = pd.to_numeric(all_data['t_ms'], errors='coerce')

# # Split once
# intervals = all_data.loc[all_data['tier'].eq('phonetic')].set_index(['phrase','t_ms'])
# vseq_idx  = (
#     all_data
#     .loc[all_data['tier'].eq('V-sequence'), ['phrase','t_ms']]
#     .drop_duplicates()
#     .set_index(['phrase','t_ms'])
#     .index
# )

# # Keep only intervals whose (phrase, t_ms) are present in V-sequence
# matching_data2 = intervals.loc[intervals.index.isin(vseq_idx)].reset_index()

# print(f"Done in {(time.time()-t0):.2f}s — rows: {len(matching_data2):,}")

t0 = time.time()

# Ensure comparable dtypes (same as your snippet)
all_data = all_data.copy()
all_data['phrase'] = all_data['phrase'].astype(str)
all_data['t_ms']   = pd.to_numeric(all_data['t_ms'], errors='coerce')

# Build the set of (phrase, t_ms) pairs from V-sequence
vseq_idx = pd.MultiIndex.from_frame(
    all_data.loc[all_data['tier'].eq('V-sequence'), ['phrase', 't_ms']].drop_duplicates()
)

# Identify phonetic rows and their (phrase, t_ms)
mask_phonetic = all_data['tier'].eq('phonetic')
phon_idx = pd.MultiIndex.from_frame(all_data.loc[mask_phonetic, ['phrase', 't_ms']])

# Compute match mask for phonetic rows
match_mask = phon_idx.isin(vseq_idx)

# Create/assign the dummy column (0/1) across the full DataFrame
all_data['target_match'] = 0
all_data.loc[mask_phonetic, 'target_match'] = match_mask.astype(int)

print(f"Done in {(time.time()-t0):.2f}s — matches: {all_data['target_match'].sum():,}")



## slicing the data below takes about 5 minutes to run because iterrows() is slow ####
#Initialize an empty DataFrame to store matching rows
# time_start = time.ctime()
# print("Start time:", time_start)
# seconds_start = time.time()

# print('Getting matching data for means...')
# matching_data = pd.DataFrame()

# # Iterate through each unique phrase
# for phrase in all_data['phrase'].unique():

#     # Filter data for the current phrase
#     phrase_data = all_data[all_data['phrase'] == phrase]

#     # Separate interval and V-sequence data within this phrase
#     interval_data = phrase_data[phrase_data['tier'] == 'phonetic']
#     v_sequence_data = phrase_data[phrase_data['tier'] == 'V-sequence']

#     # Iterate through the interval data
#     for index, interval_row in interval_data.iterrows():
#         # Check if this interval t_ms matches any V-sequence t_ms in the same phrase
#         if any(interval_row['t_ms'] == v_sequence_row.t_ms for v_sequence_row in v_sequence_data.itertuples()):
#             # Append matching row to the matching_data DataFrame
#             print('Appending match for %s phrase %s...'%(interval_row['participant'], phrase))
#             matching_data = matching_data.append(interval_row)


# time_start = time.ctime()
# print("Start time:", time_start)
# seconds_start = time.time()

# print('Getting matching data for means...')
# matching_data = pd.DataFrame()

# # Initialize a list to collect matching rows
# matching_rows = []

# # Iterate through each unique phrase
# for phrase in all_data['phrase'].unique():
#     print('Appending match for %s phrase %s...' % (interval_row['participant'], phrase))
#     # Filter data for the current phrase
#     phrase_data = all_data[all_data['phrase'] == phrase]

#     # Separate interval and V-sequence data within this phrase
#     interval_data = phrase_data[phrase_data['tier'] == 'phonetic']
#     v_sequence_data = phrase_data[phrase_data['tier'] == 'V-sequence']

#     # Create a set of t_ms values from V-sequence data for faster lookup
#     v_sequence_t_ms_set = set(v_sequence_data['t_ms'])

#     # Iterate through the interval data
#     for index, interval_row in interval_data.iterrows():
#         # Check if this interval t_ms matches any V-sequence t_ms in the same phrase
#         if interval_row['t_ms'] in v_sequence_t_ms_set:
#             # Collect the matching row
#             matching_rows.append(interval_row)

# # Convert the list of matching rows to a DataFrame
# matching_rows_df = pd.DataFrame(matching_rows)

# # Use pd.concat to append all matching rows to the matching_data DataFrame at once
# matching_data = pd.concat([matching_data, matching_rows_df], ignore_index=True)


# time_end = time.ctime()
# print("End time:", time_end)
# seconds_end = time.time()
# print("Time elapsed (minutes): ", (seconds_end-seconds_start)/60)

print('Saving matches for means as .csv in %s.'%(output_dir))
# all_data.to_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', index=False)
all_data.to_csv(os.path.join(output_dir, "preproc_matchesformeans.csv"), index=False)
# matching_data = pd.read_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', encoding='utf8')
