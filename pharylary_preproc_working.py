# ### pharylary preprocessing script
# # this script takes individual outputs from VoiceSauce and different tiers from TextGrids
# # to create individual entries of acoustic values at each millisecond according to the different intervals in the TextGrid tiers

# import pandas as pd
# import numpy as np
# import os
# import glob
# import textgrid
# import time
# from tqdm.auto import tqdm
# from pathlib import Path
# import soundfile as sf
# import unicodedata
# from textgrid import TextGrid, IntervalTier, Interval
# import math
# from scipy import linalg
# import matplotlib.pyplot as plt

# output_dir = '/Volumes/circe/alldata/dissertation/vs/output_preproc'
# report_dir = output_dir + '/reports'
# textgrid_folder = '/Volumes/circe/alldata/dissertation/vs/input_preproc'
# vs_output_dir = '/Volumes/circe/alldata/dissertation/vs/vs_output'
# fricative_output_dir = '/Volumes/circe/alldata/dissertation/vs/fricative_output'

# # output_dir = '/Volumes/cassandra/alldata/dissertation/vs/output_preproc'
# # report_dir = output_dir + '/reports'
# # textgrid_folder = '/Volumes/cassandra/alldata/dissertation/vs/input_preproc'
# # vs_output_dir = '/Volumes/cassandra/alldata/dissertation/vs/vs_output'
# # fricative_output_dir = '/Volumes/cassandra/alldata/dissertation/vs/fricative_output'

# os.chdir(output_dir)

# # vs_output = pd.read_csv(os.path.join(output_dir, 'output.txt'), sep='\t')
# # tg = textgrid.TextGrid.fromFile(os.path.join(input_dir, 'Y0395_expt_1_1_1.TextGrid'))
# # point_tier = tg.getFirst('comment')

# # Find all .txt files in the vs_output_dir
# voicesauce_files = glob.glob(os.path.join(vs_output_dir, '*.txt'))

# # Initialize an empty DataFrame
# voicesauce_data = pd.DataFrame()

# # Loop through each file and append to voicesauce_data
# for file in voicesauce_files:
#     try:
#         # Try utf-8 first
#         temp_df = pd.read_csv(file, sep='\t', encoding='utf-8')
#     except UnicodeDecodeError:
#         # Fallback to latin1 if utf-8 fails
#         temp_df = pd.read_csv(file, sep='\t', encoding='latin1')
    
#     voicesauce_data = pd.concat([voicesauce_data, temp_df], ignore_index=True)

# ## remove SD0009 temporarily due to missing label
# # voicesauce_data = voicesauce_data[~voicesauce_data['Filename'].str.contains('SD0009', na=False)]
# # voicesauce_data = voicesauce_data[~voicesauce_data['Filename'].str.contains('SD0015', na=False)]
# # voicesauce_data = voicesauce_data[~voicesauce_data['Filename'].str.contains('SD0024', na=False)]

# print('There are %s subjects in this VS output.'%(len(voicesauce_data['Filename'].str.split('_').str[0].unique())))

# # Save the concatenated data as a CSV
# print("Saving voicesauce_data.csv...")
# output_csv = os.path.join(output_dir, 'voicesauce_data.csv')
# voicesauce_data.to_csv(output_csv, index=False)




# ### LOOP ####
# # use inidividual extracted phrasenum textgrids


# # Initialize an empty DataFrame to store all relevant data
# # all_relevant_data = pd.DataFrame()

# # List or dictionary of tiers to process
# tiers_to_process = ['phonetic', 'glottis', 'V-sequence', 'word']

# time_start = time.ctime()
# print("Start time:", time_start)
# seconds_start = time.time()

# # Iterate over each file in the TextGrid folder
# for filename in os.listdir(textgrid_folder):
#     if filename.endswith('.TextGrid'):  # Check if the file is a TextGrid file
#         filepath = os.path.join(textgrid_folder, filename)
#         tg_basename = os.path.splitext(filename)[0]
#         tg = textgrid.TextGrid.fromFile(filepath)
#         print ('Extracting and appending %s TextGrid labels.'%(filename))
#         # # Identify your point tier
#         point_tier = tg.getFirst('comment')

#         # Assign variables for each tier
#         # print("Assigning tier variables...")
#         tiers = {tier_name: tg.getFirst(tier_name) for tier_name in tiers_to_process}
#         phrasenum_tier = tg.getFirst('phrasenum')

#         # Build expected output filename for this TextGrid
#         output_csv = os.path.join(output_dir, 'relevant_data', f"{os.path.splitext(filename)[0]}_relevant.csv")

#         # Skip if CSV already exists
#         if os.path.exists(output_csv):
#             print(f"Skipping {filename} — output already exists at {output_csv}")
#             continue

#         # Subset the VoiceSauce output for the current phrasenum
#         # print("Subsetting %s for %s...")
#         voicesauce_grid_data = voicesauce_data[voicesauce_data['Filename'].str.contains(tg_basename, na=False)]

#         # Initialize an empty DataFrame to store all relevant data
#         phrase_relevant_data = pd.DataFrame()
#         all_relevant_data = pd.DataFrame()

#         # Process each phrasenum interval
#         for phrasenum_interval in phrasenum_tier:
#             if phrasenum_interval.mark is not str(''):
#                 phrasenum_value = phrasenum_interval.mark

#                 # Subset the VoiceSauce output for the current phrasenum
#                 print("Subsetting %s..."%phrasenum_value)
#                 subset_voicesauce = voicesauce_grid_data[voicesauce_grid_data['Label'] == int(phrasenum_value)]

#                 # Process each tier
#                 # print("Processing...")
#                 for tier_name, tier in tiers.items():
#                     for interval in tier:
#                         if interval.mark != '':
#                             print("Subsetting %s for %s..."%(interval,tier))
#                             start_time = interval.minTime *1000 # these originally have a multiplier for 1000, unsure why, but check later pipeline
#                             end_time = interval.maxTime *1000

#                             # Extract relevant data from the subsetted VoiceSauce output
#                             relevant_data = subset_voicesauce[(subset_voicesauce['t_ms'] >= start_time) & (subset_voicesauce['t_ms'] <= end_time)].copy()

#                             # Find points within the phrasenum interval
#                             points_in_interval = [point.mark for point in point_tier if phrasenum_interval.minTime <= point.time <= phrasenum_interval.maxTime and point.mark]

#                             # Add the point marks as a new column
#                             relevant_data['comments'] = ', '.join(points_in_interval)

#                             # Add a column with the interval's .mark value
#                             relevant_data['interval'] = interval.mark
#                             relevant_data['tier'] = tier.name

#                             # Reorder the columns
#                             cols = relevant_data.columns.tolist()
#                             cols.insert(2, cols.pop(cols.index('interval')))
#                             cols.insert(3, cols.pop(cols.index('tier')))
#                             cols.insert(4, cols.pop(cols.index('comments')))
#                             relevant_data = relevant_data[cols]

#                             # print(relevant_data)

#                             # # Save this TextGrid's relevant_data to CSV
#                             # relevant_data.to_csv(output_csv, index=False)
#                             # print(f"Saved {len(relevant_data)} rows to {output_csv}")

#                             # Append the relevant data to the DataFrame
#                             phrase_relevant_data = pd.concat([phrase_relevant_data, relevant_data], ignore_index=True)

#         all_relevant_data = pd.concat([all_relevant_data, phrase_relevant_data], ignore_index=True)

#         all_relevant_data.to_csv(output_csv, index=False)

# time_end = time.ctime()
# print("End time:", time_end)
# seconds_end = time.time()
# print("Time elapsed (minutes): ", (seconds_end-seconds_start)/60)


# ### code here that concatenates all the relevant_data

# # directory containing the individual CSVs
# csv_dir = os.path.join(output_dir, "relevant_data")
# csv_files = glob.glob(os.path.join(csv_dir, "*.csv"))

# print(f"Found {len(csv_files)} CSV files to combine.")

# # -----------------------------
# # 1. Load all CSVs with progress
# # -----------------------------
# df_list = []
# for f in tqdm(csv_files, desc="Loading CSVs", unit="file"):
#     try:
#         df = pd.read_csv(f)
#         df_list.append(df)
#     except Exception as e:
#         print(f"⚠️ Skipping {f} due to error: {e}")

# # -----------------------------
# # 2. Concatenate with progress
# # -----------------------------
# if df_list:
#     print("Concatenating CSVs into all_relevant_data...")
#     all_relevant_data = pd.DataFrame()
#     # simulate progress by adding DataFrames one by one
#     with tqdm(total=len(df_list), desc="Concatenating", unit="file") as pbar:
#         for df in df_list:
#             all_relevant_data = pd.concat([all_relevant_data, df], ignore_index=True)
#             pbar.update(1)

#     print(f"✅ Combined {len(df_list)} files — total rows: {len(all_relevant_data):,}")

#     # Save the combined dataset
#     combined_path = os.path.join(output_dir, "all_relevant_data.csv")
#     all_relevant_data.to_csv(combined_path, index=False)
#     print(f"Saved combined dataset to {combined_path}")

# else:
#     all_relevant_data = pd.DataFrame()
#     print("No CSV files were loaded; all_relevant_data is empty.")

# # # Initialize an empty DataFrame to store all relevant data
# # all_relevant_data = pd.DataFrame()

# # # List or dictionary of tiers to process
# # tiers_to_process = ['phonetic', 'glottis', 'V-sequence', 'word']

# # time_start = time.ctime()
# # print("Start time:", time_start)
# # seconds_start = time.time()

# # # Iterate over each file in the TextGrid folder
# # for filename in os.listdir(textgrid_folder):
# #     if filename.endswith('.TextGrid'):  # Check if the file is a TextGrid file
# #         filepath = os.path.join(textgrid_folder, filename)
# #         tg = textgrid.TextGrid.fromFile(filepath)
# #         print ('Extracting and appending %s TextGrid labels.'%(filename))
# #         # # Identify your point tier
# #         point_tier = tg.getFirst('comment')

# #         # Assign variables for each tier
# #         # print("Assigning tier variables...")
# #         tiers = {tier_name: tg.getFirst(tier_name) for tier_name in tiers_to_process}
# #         phrasenum_tier = tg.getFirst('phrasenum')

# #         # Build expected output filename for this TextGrid
# #         output_csv = os.path.join(output_dir, f"{os.path.splitext(filename)[0]}_relevant.csv")

# #         # Skip if CSV already exists
# #         if os.path.exists(output_csv):
# #             print(f"Skipping {filename} — output already exists at {output_csv}")
# #             continue

# #         # Process each phrasenum interval
# #         for phrasenum_interval in phrasenum_tier:
# #             if phrasenum_interval.mark is not str(''):
# #                 phrasenum_value = phrasenum_interval.mark

# #                 # Subset the VoiceSauce output for the current phrasenum
# #                 # print("Subsetting...")
# #                 subset_voicesauce = voicesauce_data[voicesauce_data['Label'] == int(phrasenum_value)]

# #                 # Process each tier
# #                 # print("Processing...")
# #                 for tier_name, tier in tiers.items():
# #                     for interval in tier:
# #                         if interval.mark != '':
# #                             start_time = interval.minTime *1000 # these originally have a multiplier for 1000, unsure why, but check later pipeline
# #                             end_time = interval.maxTime *1000

# #                             # Extract relevant data from the subsetted VoiceSauce output
# #                             relevant_data = subset_voicesauce[(subset_voicesauce['t_ms'] >= start_time) & (subset_voicesauce['t_ms'] <= end_time)].copy()

# # 							# Find points within the phrasenum interval
# #                             points_in_interval = [point.mark for point in point_tier if phrasenum_interval.minTime <= point.time <= phrasenum_interval.maxTime and point.mark]

# # 							# Add the point marks as a new column
# #                             relevant_data['comments'] = ', '.join(points_in_interval)

# # 							# Add a column with the interval's .mark value
# #                             relevant_data['interval'] = interval.mark
# #                             relevant_data['tier'] = tier.name

# #                             # Reorder the columns
# #                             cols = relevant_data.columns.tolist()
# #                             cols.insert(2, cols.pop(cols.index('interval')))
# #                             cols.insert(3, cols.pop(cols.index('tier')))
# #                             cols.insert(4, cols.pop(cols.index('comments')))
# #                             relevant_data = relevant_data[cols]

# #                             # # Save this TextGrid's relevant_data to CSV
# #                             # relevant_data.to_csv(output_csv, index=False)
# #                             # print(f"Saved {len(relevant_data)} rows to {output_csv}")

# #                             # Append the relevant data to the DataFrame
# #                             all_relevant_data = pd.concat([all_relevant_data, relevant_data], ignore_index=True)

# # all_relevant_data.to_csv('/Volumes/circe/alldata/dissertation/vs/output_preproc/all_relevant_data.csv', index=False)

# # time_end = time.ctime()
# # print("End time:", time_end)
# # seconds_end = time.time()
# # print("Time elapsed (minutes): ", (seconds_end-seconds_start)/60)


# #### MORE PREPROC #####

# print('Processing all data for min, max, proportion, etc.')
# # all_relevant_data = pd.read_csv(os.path.join(output_dir, "all_relevant_data.csv"), encoding='utf-8')

# # Drop the 'Label' column
# all_relevant_data.drop(['Label','Unnamed: 72'], axis=1, inplace=True)

# # Split the 'Filename' column into separate parts
# filename_parts = all_relevant_data['Filename'].str.split('_', expand=True)

# # Assign new columns based on the split parts
# # Assuming the format is consistent and the desired parts are at specific indices
# all_relevant_data['participant'] = filename_parts[0]
# all_relevant_data['session'] = filename_parts[2]
# all_relevant_data['phrase'] = filename_parts[3]
# # all_relevant_data['phrase'] = filename_parts[4].str.extract(r'(\d+)(?=\.mat)')


# # Determine the position of the 'comments' column
# comments_col_idx = all_relevant_data.columns.get_loc('interval') + 2

# # Reorder columns to place the new columns after 'comments'
# cols = all_relevant_data.columns.tolist()
# participant_idx = cols.index('participant')
# session_idx = cols.index('session')
# phrase_num_idx = cols.index('phrase')

# # Move 'participant', 'session', and 'phrase_num' after 'comments'
# cols.insert(comments_col_idx, cols.pop(participant_idx))
# cols.insert(comments_col_idx + 1, cols.pop(session_idx))
# cols.insert(comments_col_idx + 2, cols.pop(phrase_num_idx))

# # Reassign reordered columns to DataFrame
# all_relevant_data = all_relevant_data[cols]

# # Ensure numeric (optional but helps if there are stray strings)
# all_relevant_data['t_ms']  = pd.to_numeric(all_relevant_data['t_ms'], errors='coerce')
# all_relevant_data['Energy'] = pd.to_numeric(all_relevant_data['Energy'], errors='coerce')

# g = all_relevant_data.groupby(['phrase', 'tier', 'interval'])

# # t_ms min/max per (phrase, tier, interval)
# all_relevant_data['t_min'] = g['t_ms'].transform('min')
# all_relevant_data['t_max'] = g['t_ms'].transform('max')

# # Energy min/max per (phrase, tier, interval)
# all_relevant_data['energy_min'] = g['Energy'].transform('min')
# all_relevant_data['energy_max'] = g['Energy'].transform('max')

# # Group once
# g = all_relevant_data.groupby(['phrase', 'tier', 'interval'], group_keys=False)

# # Precompute range denominators to avoid division repetition
# t_range = g['t_ms'].transform('max') - g['t_ms'].transform('min')
# e_range = g['Energy'].transform('max') - g['Energy'].transform('min')

# # Compute proportions (with safe division)
# all_relevant_data['t_prop'] = ((all_relevant_data['t_ms'] - g['t_ms'].transform('min')) / t_range) * 100
# all_relevant_data['energy_prop'] = (all_relevant_data['Energy'] - g['Energy'].transform('min')) / e_range

# # Compute duration per group
# all_relevant_data['duration'] = t_range

# # (optional) replace inf/nan if any denominators were zero
# all_relevant_data[['energy_prop']] = (
#     all_relevant_data[['energy_prop']].fillna(0)
# )

# ### add in other data from google sheet ###
# print('Adding metadata...')
# stim_meta = pd.read_csv('/Users/bcl/GitHub/pharylary/PharyLary Stimuli - pharylary2.csv')
# stim_meta_df = stim_meta[['Phrasenum','IPA','Gloss','Gloss_2','Syllable','Segment','Type','Position','Position_2','Contrast_(IPA)','Contrast','Experiment']]
# stim_meta_df = stim_meta_df.rename(columns={'Phrasenum':'phrase'})
# stim_meta_df = stim_meta_df.astype({'phrase': int})
# all_relevant_data = all_relevant_data.astype({'phrase': int})
# all_data = pd.merge(all_relevant_data, stim_meta_df, on='phrase', how='inner')

# record_data = pd.read_csv('/Users/bcl/GitHub/pharylary/PharyLary Participant Record.csv')
# record_data_df = record_data[['Participant','Experiment']]
# record_data_df = record_data_df.rename(columns={'Experiment':'Carrier','Participant':'participant'})
# all_data = pd.merge(all_data, record_data_df, on='participant', how='inner')

# # Output the final DataFrame
# # print(all_relevant_data)
# # Optionally, save to a file
# # print('Saving as .csv in %s.'%(output_dir))
# # all_data.to_csv('preproc_output.csv', index=False)

# ### NEW SECTION: Import and concatenate fricative measurement data ###

# ### loop to prep files into individual folders for use with multitaper.R. all of the files are made from the individual textgrids in originals


# # ====== USER SETTINGS ======
# data_dir   = textgrid_folder                               # folder with .TextGrid + matching .wav basenames
# out_root   = os.path.join(output_dir, "fricatives")
# # Choose how you pick fricative intervals:
# TIER_FILTER_INDEX = 0      # e.g., 1 for your "fricatives" tier (0-based); set to None if you’ll filter by name
# TIER_FILTER_NAME  = None   # or e.g., "fricatives"; set to None if using index

# # Labels to extract (each goes to its own subfolder inside out_root)
# labels = ["s", "sˤ", "ħ", "h", "ʕ", "ʁ", "ð", "ðħ", "sʕ","ðˤ"]

# # The two tiers to copy into the per-clip TextGrid (by NAME)
# COPY_TIER_NAMES = ["phonetic", "phrasenum"]
# # ===========================

# # Ensure per-label subfolders exist
# for lab in labels:
#     os.makedirs(os.path.join(out_root, lab), exist_ok=True)

# labels_set = set(unicodedata.normalize("NFC", l.strip()) for l in labels)

# def nfc_strip(s):
#     return unicodedata.normalize("NFC", (s or "").strip())

# def find_wav(base, directory):
#     """Find matching WAV (case-insensitive)."""
#     for ext in (".wav", ".WAV", ".Wav"):
#         path = os.path.join(directory, f"{base}{ext}")
#         if os.path.exists(path):
#             return path
#     return None

# def out_name(base, label, idx=None):
#     lab = label.replace("/", "_")
#     return f"{base}_{lab}.wav" if idx is None else f"{base}_{lab}-{idx:02d}.wav"

# def crop_and_shift_tier(src_tier: IntervalTier, start_s: float, end_s: float, new_name: str) -> IntervalTier:
#     """
#     Copy intervals from src_tier that overlap [start_s, end_s], crop to that window,
#     and shift so the new min time = 0.0.
#     """
#     clip_len = float(end_s - start_s)
#     new_tier = IntervalTier(name=new_name, minTime=0.0, maxTime=clip_len)

#     # Some TextGrid writers leave a final "background" interval; we’ll still crop it.
#     for itv in getattr(src_tier, "intervals", []):
#         a, b = float(itv.minTime), float(itv.maxTime)
#         if b <= start_s or a >= end_s:
#             continue  # no overlap

#         # overlap cropped to the clip, re-based to 0
#         na = max(a, start_s) - start_s
#         nb = min(b, end_s) - start_s
#         if nb > na:
#             new_tier.addInterval(Interval(na, nb, itv.mark))

#     # Ensure bounds are consistent
#     new_tier.minTime = 0.0
#     new_tier.maxTime = clip_len
#     return new_tier


# def build_clip_textgrid(full_tg: TextGrid, start_s: float, end_s: float, copy_names: list[str]) -> TextGrid:
#     """
#     Build a new TextGrid with ONLY the tiers named in copy_names,
#     cropped to [start_s, end_s] and shifted to start at 0.0.
#     """
#     clip_len = float(end_s - start_s)
#     new_tg = TextGrid(minTime=0.0, maxTime=clip_len)

#     # Map existing tiers by normalized name
#     import unicodedata
#     def nfc_strip(s):
#         return unicodedata.normalize("NFC", (s or "").strip())

#     name2tier = {nfc_strip(getattr(t, "name", "")): t for t in full_tg.tiers}

#     for wanted in copy_names:
#         key = nfc_strip(wanted)
#         src = name2tier.get(key, None)

#         if src is None or not hasattr(src, "intervals"):
#             # Create an empty placeholder IntervalTier if missing or it’s a PointTier
#             new_tg.append(IntervalTier(name=wanted, minTime=0.0, maxTime=clip_len))
#         else:
#             new_tg.append(crop_and_shift_tier(src, start_s, end_s, new_name=wanted))

#     new_tg.minTime = 0.0
#     new_tg.maxTime = clip_len
#     return new_tg


# # Gather TextGrids
# tg_paths = sorted(glob.glob(os.path.join(data_dir, "*.TextGrid")))
# if not tg_paths:
#     print(f"No TextGrid files found in {data_dir}")

# total_written = 0
# missing_wavs = 0

# for tg_path in tg_paths:
#     base = os.path.splitext(os.path.basename(tg_path))[0]
#     wav_path = find_wav(base, data_dir)
#     if wav_path is None:
#         print(f"[WARN] Skipping {base}: no matching WAV")
#         missing_wavs += 1
#         continue

#     # Load audio
#     audio, sr = sf.read(wav_path, always_2d=False)
#     n_samples = len(audio) if getattr(audio, "ndim", 1) == 1 else audio.shape[0]

#     # Load TextGrid
#     tg = TextGrid.fromFile(tg_path)

#     # Choose the fricatives tier where we'll find intervals to extract
#     if TIER_FILTER_INDEX is not None:
#         if 0 <= TIER_FILTER_INDEX < len(tg.tiers):
#             fric_tier = tg.tiers[TIER_FILTER_INDEX]
#         else:
#             print(f"[WARN] {os.path.basename(tg_path)}: tier index {TIER_FILTER_INDEX} out of range (has {len(tg.tiers)}).")
#             continue
#     elif TIER_FILTER_NAME is not None:
#         found = [t for t in tg.tiers if nfc_strip(getattr(t, "name", "")) == nfc_strip(TIER_FILTER_NAME)]
#         if not found:
#             print(f"[WARN] {os.path.basename(tg_path)}: tier '{TIER_FILTER_NAME}' not found.")
#             continue
#         fric_tier = found[0]
#     else:
#         # If neither set, default to first tier
#         fric_tier = tg.tiers[0]

#     # Collect matches per label from the chosen tier
#     by_label = {lab: [] for lab in labels_set}
#     intervals = getattr(fric_tier, "intervals", [])
#     for itv in intervals:
#         lab = nfc_strip(itv.mark)
#         if lab in labels_set and itv.minTime < itv.maxTime:
#             by_label[lab].append((float(itv.minTime), float(itv.maxTime)))

#     # Write clips + companion TextGrids
#     for lab, spans in by_label.items():
#         if not spans:
#             continue
#         subdir = os.path.join(out_root, lab)
#         for i, (start_s, end_s) in enumerate(spans, start=1):
#             # sample indices (clamped)
#             start_i = max(0, min(int(round(start_s * sr)), n_samples))
#             end_i   = max(0, min(int(round(end_s * sr)), n_samples))
#             if end_i <= start_i:
#                 continue

#             # 1) Write audio clip
#             clip = audio[start_i:end_i]
#             wav_name = out_name(base, lab, None if len(spans) == 1 else i)
#             wav_out  = os.path.join(subdir, wav_name)
#             sf.write(wav_out, clip, sr, subtype="PCM_16")

#             # 2) Build and write companion TextGrid
#             clip_tg  = build_clip_textgrid(tg, start_s, end_s, COPY_TIER_NAMES)
#             tg_name  = os.path.splitext(wav_name)[0] + ".TextGrid"
#             tg_out   = os.path.join(subdir, tg_name)
#             clip_tg.write(tg_out)

#             print(f"✔ {lab}: {wav_name} & {tg_name}  {(end_i - start_i)/sr:.3f}s")
#             total_written += 1

# print("\nSummary")
# print("=======")
# print(f"TextGrids processed: {len(tg_paths)}")
# print(f"Missing WAVs: {missing_wavs}")
# print(f"Total clips written: {total_written}")
# print(f"Output root: {out_root}")

#################################################
#                                               #
#       GO TO MULTITAPER MODIFIED SCRIPT        #
#                                               #
#################################################

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

# Flag rows where "comments" contains "ignore" (case-insensitive)
all_data['ignore_flag'] = (
    all_data['comments']
    .astype(str)                     # ensure it's string
    .str.contains('ignore', case=False, na=False)
    .astype(int)                     # convert True/False → 1/0
)

print(f"Flagged {all_data['ignore_flag'].sum()} rows containing 'ignore'.")


# Output the final DataFrame
# print(all_data)
# Optionally, save to a file
print('Saving as .csv in %s.'%(output_dir))
all_data.to_csv('preproc_output.csv', index=False)


# Make sure phrase values are numeric
all_data['phrase'] = pd.to_numeric(all_data['phrase'], errors='coerce')

# Build report of missing phrases per participant
missing_report = []

for participant, df_part in all_data.groupby('participant'):
    phrases = sorted(df_part['phrase'].dropna().unique())
    if len(phrases) == 0:
        continue  # skip participants with no phrase data
    
    # Find the range of possible phrases for this participant
    min_phrase = int(np.nanmin(phrases))
    max_phrase = int(np.nanmax(phrases))
    expected = set(range(min_phrase, max_phrase + 1))
    actual = set(phrases)
    
    missing = sorted(expected - actual)
    
    missing_report.append({
        'participant': participant,
        'min_phrase': min_phrase,
        'max_phrase': max_phrase,
        'total_expected': len(expected),
        'total_present': len(actual),
        'total_missing': len(missing),
        'missing_phrases': missing
    })

# Convert report to DataFrame
missing_report_df = pd.DataFrame(missing_report)

# Save the report to CSV
output_csv = os.path.join(report_dir, 'missing_phrases_by_participant.csv')
missing_report_df.to_csv(output_csv, index=False)

print(f"Saved missing-phrase report to: {output_csv}")

## requires having all_data loaded from preproc

# Filter for phonetic-tier rows and keep only relevant columns
phonetic_df = all_data.loc[
    all_data['tier'] == 'phonetic',
    ['participant', 'phrase', 'interval', 'Segment']
].dropna()

# Group by interval, phrase, and Segment, collecting unique participants
interval_participants = (
    phonetic_df
    .groupby(['interval', 'phrase', 'Segment'])['participant']
    .unique()
    .reset_index()
)

# Sort for readability
interval_participants = (
    interval_participants
    .sort_values(['interval', 'phrase', 'Segment'])
    .reset_index(drop=True)
)

# Define output path
output_csv = os.path.join(report_dir, 'phonetic_intervals_by_phrase_segment_participant.csv')

# Save to CSV
interval_participants.to_csv(output_csv, index=False)

print(f"Saved phonetic interval labels by phrase, segment, and participant to: {output_csv}")



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

print('Saving matches for means as .csv in %s.'%(output_dir))
# all_data.to_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', index=False)
all_data.to_csv(os.path.join(output_dir, "preproc_matchesformeans.csv"), index=False)
# matching_data = pd.read_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', encoding='utf8')

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

# print('Saving matches for means as .csv in %s.'%(output_dir))
# # all_data.to_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', index=False)
# all_data.to_csv(os.path.join(output_dir, "preproc_matchesformeans.csv"), index=False)
# # matching_data = pd.read_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', encoding='utf8')


### pharylary preprocessing script (flat version, parallel)
# Same pipeline as the original, top to bottom, as flat as possible. The only functions are the two
# per-TextGrid workers (cells 3 and 8), which exist so joblib can run them in parallel. Each "# %%" block is a cell you can run and inspect on its own.
# Lines marked "# FIX" are where this differs from the original script.
#
# Steps:
#   1. load VoiceSauce output
#   2. loop over TextGrids -> one CSV per TextGrid (one row per VS frame, labelled by interval)
#   3. combine the per-TextGrid CSVs
#   4. per-token stats (t_prop, energy_prop, duration, ...)
#   5. merge stimulus + participant metadata
#   6. (optional) compare against the old pipeline's all_relevant_data.csv
#   7. clip fricatives for multitaper.R

# %% 0. IMPORTS + CONFIG
import os
import re
import glob
import time
import traceback
import unicodedata
from collections import Counter

import numpy as np
import pandas as pd
import soundfile as sf
import textgrid
from textgrid import TextGrid, IntervalTier, Interval
from joblib import Parallel, delayed     # needs joblib >= 1.3
from tqdm.auto import tqdm

base_dir = '/Volumes/circe/alldata/dissertation/vs'
# base_dir = '/Volumes/cassandra/alldata/dissertation/vs'

output_dir = base_dir + '/output_preproc'
textgrid_folder = base_dir + '/input_preproc'
vs_output_dir = base_dir + '/vs_output'

SUFFIX = '_v2'     # FIX: new outputs get a suffix so the old files stay untouched for comparison
relevant_dir = os.path.join(output_dir, 'relevant_data' + SUFFIX)
report_dir = os.path.join(output_dir, 'reports')
fricatives_out = os.path.join(output_dir, 'fricatives')   # same as the original

stim_csv = '/Users/bcl/GitHub/pharylary/PharyLary Stimuli - pharylary2.csv'
record_csv = '/Users/bcl/GitHub/pharylary/PharyLary Participant Record.csv'

N_JOBS = 6                         # worker processes for cells 3 and 8; 1 = serial. If more workers
                                   # doesn't speed things up you're I/O-bound on the network volume
OVERWRITE = False                  # True = redo every TextGrid even if its CSV already exists
tiers_to_process = ['phonetic', 'glottis', 'V-sequence', 'word']
exclude_participants = []          # e.g. ['SD0009', 'SD0015', 'SD0024'] (replaces the commented-out lines)

# FIX: stem = VS 'Filename' with this extension removed; it must EXACTLY equal the TextGrid basename
vs_ext_regex = r'\.(mat|wav|txt|textgrid)$'

# What does the VS 'Label' column hold?
#   False = phone labels from the VS run (e.g. 'a', 'ʔ', 'ħ') -> a phrase's frames are selected by TIME
#   True  = the phrasenum (the original script's assumption)  -> frames are selected by Label == phrasenum
vs_label_is_phrasenum = False

# position of each field when the stem is split on '_' (same as the original)
participant_idx, session_idx, phrase_idx = 0, 2, 3

# a one-line helper
nfc = lambda s: unicodedata.normalize('NFC', (s or '').strip())

assert os.path.isdir(base_dir), f'{base_dir} not found - is the network volume mounted?'
os.makedirs(output_dir, exist_ok=True)
os.makedirs(report_dir, exist_ok=True)
os.makedirs(relevant_dir, exist_ok=True)


# %% 1. LOAD VOICESAUCE OUTPUT
voicesauce_files = sorted(glob.glob(os.path.join(vs_output_dir, '*.txt')))
print(f'{len(voicesauce_files)} VoiceSauce files')

vs_list = []                                    # FIX: collect in a list, concat once (not once per file)
for file in tqdm(voicesauce_files, desc='Reading VS files'):
    try:
        temp_df = pd.read_csv(file, sep='\t', encoding='utf-8', low_memory=False)
    except UnicodeDecodeError:
        temp_df = pd.read_csv(file, sep='\t', encoding='latin1', low_memory=False)
    # FIX: drop 'Unnamed: N' columns (trailing tabs) here, instead of hard-coding 'Unnamed: 72' later
    temp_df = temp_df.loc[:, ~temp_df.columns.astype(str).str.startswith('Unnamed')]
    vs_list.append(temp_df)
voicesauce_data = pd.concat(vs_list, ignore_index=True)

# FIX: make sure the columns we compare/compute on are numeric
# (Label is only numeric when it holds the phrasenum; in your current VS output it holds phone labels)
raw_head = voicesauce_data[['Filename', 'Label', 't_ms', 'Energy']].head()    # kept for the diagnostic below
raw_dtypes = voicesauce_data[['Label', 't_ms', 'Energy']].dtypes
numeric_cols = ['t_ms', 'Energy'] + (['Label'] if vs_label_is_phrasenum else [])
required_cols = ['t_ms'] + (['Label'] if vs_label_is_phrasenum else [])
for col in numeric_cols:
    voicesauce_data[col] = pd.to_numeric(voicesauce_data[col], errors='coerce')
n_nan = voicesauce_data[required_cols].isna().sum()
print('NaN after numeric conversion:'); print(n_nan)
if n_nan.max() > 0.5 * len(voicesauce_data):         # a whole column failed to convert: stop and show why
    print('Raw dtypes:'); print(raw_dtypes)
    print('Raw values BEFORE conversion:'); print(raw_head)
    raise ValueError(f'{required_cols} is mostly non-numeric after conversion - see the raw values printed above')
n_bad = voicesauce_data[required_cols].isna().any(axis=1).sum()
print(f'Dropping {n_bad:,} rows with non-numeric {required_cols}')
voicesauce_data = voicesauce_data.dropna(subset=required_cols)

# FIX: 'stem' is the key used to match VS rows to TextGrids (exact match, not substring/regex)
voicesauce_data['stem'] = (voicesauce_data['Filename'].astype(str).str.strip()
                           .str.replace(vs_ext_regex, '', regex=True, flags=re.I))

vs_participants = voicesauce_data['stem'].str.split('_').str[participant_idx]
print('There are %s subjects in this VS output.' % vs_participants.nunique())
voicesauce_data = voicesauce_data[~vs_participants.isin(exclude_participants)]

print(voicesauce_data.shape)
print(voicesauce_data[['Filename', 'stem', 'Label', 't_ms', 'Energy']].head())

print('Saving voicesauce_data.csv...')
voicesauce_data.to_csv(os.path.join(output_dir, 'voicesauce_data.csv'), index=False)


# %% 1b. CHECK THE VS TIME BASE ON ONE FILE
# Is t_ms measured from the start of the FILE (same clock as the TextGrid) or restarted per labelled segment?
# Compare t_first / t_last below with that file's phonetic-tier boundaries (x 1000) in Praat.
one = voicesauce_data[voicesauce_data['stem'] == voicesauce_data['stem'].iloc[0]]
runs = (one['Label'] != one['Label'].shift()).cumsum()
print(one['stem'].iloc[0])
print(one.groupby(runs).agg(Label=('Label', 'first'), t_first=('t_ms', 'min'), t_last=('t_ms', 'max'),
                            n_frames=('t_ms', 'size')).head(15))


# %% 2. SPLIT VS DATA BY TEXTGRID (once, up front)
# FIX: the original ran voicesauce_data['Filename'].str.contains(tg_basename) for every TextGrid:
# that is a full-table regex scan each time, and it also matches e.g. '..._1' inside '..._12'.
vs_groups = {stem: grp for stem, grp in voicesauce_data.groupby('stem')}

tg_filenames = sorted(f for f in os.listdir(textgrid_folder)
                      if f.endswith('.TextGrid') and not f.startswith('.'))   # FIX: skip '._' files
tg_stems = {os.path.splitext(f)[0] for f in tg_filenames}

print(f'{len(tg_filenames)} TextGrids, {len(vs_groups)} VS stems')
print('TextGrids with no VS data:', sorted(tg_stems - set(vs_groups))[:10])
print('VS stems with no TextGrid:', sorted(set(vs_groups) - tg_stems)[:10])


# %% 3. LOOP OVER TEXTGRIDS -> one CSV each (PARALLEL)
# The body of the old per-TextGrid loop is now the function process_textgrid() below, so joblib can
# send each TextGrid to a worker process. The logic inside is identical to the serial version
# (`continue` became `return`). The skip checks and VS matching stay in the plain loop after it,
# so workers only receive the files that actually need work.

def process_textgrid(filename, voicesauce_grid_data, output_csv):
    tg_basename = os.path.splitext(filename)[0]
    try:                                                    # one bad file must not kill the whole run
        tg = textgrid.TextGrid.fromFile(os.path.join(textgrid_folder, filename))
        point_tier = tg.getFirst('comment')
        phrasenum_tier = tg.getFirst('phrasenum')
        tiers = {tier_name: tg.getFirst(tier_name) for tier_name in tiers_to_process}

        # FIX: getFirst returns None for a missing tier, which used to crash the loop
        missing = [n for n, t in tiers.items() if t is None]
        if phrasenum_tier is None:
            missing.append('phrasenum')
        if missing:
            return (tg_basename, 'missing_tiers', 0, ', '.join(missing))
        comment_points = [(pt.time, pt.mark) for pt in point_tier if pt.mark] if point_tier is not None else []

        file_pieces = []                                    # FIX: list + one concat (not concat per interval)
        for phrasenum_interval in phrasenum_tier:
            if phrasenum_interval.mark.strip() == '':       # FIX: was `is not str('')`
                continue
            phr_start = round(phrasenum_interval.minTime * 1000, 6)
            phr_end = round(phrasenum_interval.maxTime * 1000, 6)
            if vs_label_is_phrasenum:       # VS Label = phrasenum (the original script's assumption)
                subset_voicesauce = voicesauce_grid_data[voicesauce_grid_data['Label'] == int(phrasenum_interval.mark)]
            else:                           # VS Label = phone labels: select this phrase's frames by TIME instead
                subset_voicesauce = voicesauce_grid_data[(voicesauce_grid_data['t_ms'] >= phr_start) &
                                                         (voicesauce_grid_data['t_ms'] < phr_end)]

            # comment points inside this phrasenum interval
            points_in_interval = ', '.join(m for tm, m in comment_points
                                           if phrasenum_interval.minTime <= tm <= phrasenum_interval.maxTime)

            for tier_name, tier in tiers.items():
                for interval_idx, interval in enumerate(tier):    # FIX: keep the interval's position in its tier
                    if interval.mark.strip() == '':
                        continue
                    start_time = round(interval.minTime * 1000, 6)   # FIX: round away float error at boundaries
                    end_time = round(interval.maxTime * 1000, 6)

                    # FIX: half-open window [start, end) so a frame on a shared boundary is in ONE interval
                    relevant_data = subset_voicesauce[(subset_voicesauce['t_ms'] >= start_time) &
                                                      (subset_voicesauce['t_ms'] < end_time)].copy()
                    if relevant_data.empty:
                        continue

                    relevant_data['comments'] = points_in_interval
                    relevant_data['interval'] = interval.mark
                    relevant_data['tier'] = tier_name
                    relevant_data['interval_idx'] = interval_idx
                    relevant_data['int_start_ms'] = start_time           # FIX: true TextGrid bounds
                    relevant_data['int_end_ms'] = end_time
                    # FIX: unique ID for one interval in one file (used for the groupby in step 5)
                    relevant_data['token_id'] = f'{tg_basename}|{tier_name}|{interval_idx}'
                    file_pieces.append(relevant_data)

        if not file_pieces:
            # FIX: write NOTHING (an empty CSV would be skipped forever on reruns)
            return (tg_basename, 'no_rows', 0, '')

        pd.concat(file_pieces, ignore_index=True).to_csv(output_csv, index=False)
        return (tg_basename, 'ok', sum(len(p) for p in file_pieces), '')
    except Exception:
        return (tg_basename, 'error', 0, traceback.format_exc(limit=3))


# --- decide which TextGrids need processing (plain serial loop, cheap) ---
log = []
tasks = []
for filename in tg_filenames:
    tg_basename = os.path.splitext(filename)[0]
    output_csv = os.path.join(relevant_dir, f'{tg_basename}_relevant.csv')

    # FIX: skip check happens BEFORE parsing the TextGrid
    if os.path.exists(output_csv) and not OVERWRITE:
        log.append((tg_basename, 'skipped_exists', 0, ''))
        continue
    if tg_basename not in vs_groups:
        log.append((tg_basename, 'no_vs_match', 0, ''))
        continue
    tasks.append((filename, vs_groups[tg_basename], output_csv))     # FIX: exact-stem VS subset
print(f'{len(tasks)} TextGrids to process, {len(log)} skipped/unmatched')

# --- run (N_JOBS = 1 runs serially in this process, which is easiest for debugging) ---
time_start = time.time()
print('Start time:', time.ctime())
results = Parallel(n_jobs=N_JOBS, return_as='generator')(delayed(process_textgrid)(*t) for t in tasks)
for r in tqdm(results, total=len(tasks), desc='TextGrids'):
    log.append(r)
print('End time:', time.ctime())
print('Time elapsed (minutes): ', (time.time() - time_start) / 60)

log_df = pd.DataFrame(log, columns=['stem', 'status', 'n_rows', 'detail'])
log_df.to_csv(os.path.join(report_dir, 'relevant_data_log.csv'), index=False)
print(log_df['status'].value_counts())
# show WHY files were not written: one example per problem status, with the full detail text
print(f'\n{(log_df["status"] == "ok").sum()} CSVs written to {relevant_dir}')
for status, grp in log_df[~log_df['status'].isin(['ok', 'skipped_exists'])].groupby('status'):
    print(f'\n===== {status}: {len(grp)} files, e.g. {grp["stem"].head(3).tolist()}')
    print(grp['detail'].iloc[0] or '(no detail for this status)')


# %% 3b. CHECK ONE FILE BY EYE
check_stem = next((s for s, st, *_ in log if st == 'ok'), None)       # first file that was written
assert check_stem is not None, 'no CSVs were written - see the log above'
check = pd.read_csv(os.path.join(relevant_dir, f'{check_stem}_relevant.csv'))
print(check[['stem', 'tier', 'interval', 'interval_idx', 'int_start_ms', 'int_end_ms', 't_ms']].head(10))
print(check.groupby(['tier', 'interval_idx', 'interval']).agg(
    n=('t_ms', 'size'), t_first=('t_ms', 'min'), t_last=('t_ms', 'max'),
    start=('int_start_ms', 'first'), end=('int_end_ms', 'first')).head(15))
print('duplicate frames within a tier:', check.duplicated(['tier', 't_ms']).sum())   # should be 0


# %% 4. COMBINE THE PER-TEXTGRID CSVs
csv_files = sorted(glob.glob(os.path.join(relevant_dir, '*_relevant.csv')))
print(f'Found {len(csv_files)} CSV files to combine.')

df_list = []
for f in tqdm(csv_files, desc='Loading CSVs'):
    try:
        df_list.append(pd.read_csv(f, low_memory=False))
    except Exception as e:
        print(f'Skipping {f} due to error: {e}')

all_relevant_data = pd.concat(df_list, ignore_index=True)    # FIX: one concat (was quadratic)
print(f'Combined {len(df_list)} files, total rows: {len(all_relevant_data):,}')
all_relevant_data.to_csv(os.path.join(output_dir, f'all_relevant_data{SUFFIX}.csv'), index=False)


# %% 5. PER-TOKEN STATS (min, max, proportion, duration)
print('Processing all data for min, max, proportion, etc.')

# FIX: drop any 'Unnamed' columns instead of hard-coding 'Unnamed: 72'
all_relevant_data = all_relevant_data.drop(
    columns=[c for c in all_relevant_data.columns if str(c).startswith('Unnamed')])

# split the stem into parts (stem has no file extension, so the last part is clean)
filename_parts = all_relevant_data['stem'].str.split('_', expand=True)
all_relevant_data['participant'] = filename_parts[participant_idx]
all_relevant_data['session'] = filename_parts[session_idx]
all_relevant_data['phrase'] = pd.to_numeric(filename_parts[phrase_idx], errors='coerce')

if vs_label_is_phrasenum:
    # FIX: the phrase from the filename should equal the VS Label we filtered on; check before dropping Label
    mismatch = all_relevant_data['phrase'] != all_relevant_data['Label']
    print(f'Rows where phrase (from filename) != VS Label: {mismatch.sum():,}')
    print(all_relevant_data.loc[mismatch, 'stem'].unique()[:10])
    all_relevant_data = all_relevant_data.drop(columns=['Label'])
else:
    # VS Label = phone label from the VS run: keep it as vs_label and use it to verify the time alignment.
    # In 'phonetic'-tier rows it should equal the interval label (100% = VS and TextGrid share one clock).
    all_relevant_data = all_relevant_data.rename(columns={'Label': 'vs_label'})
    ph = all_relevant_data[all_relevant_data['tier'] == 'phonetic']
    agree = ph['vs_label'].map(nfc) == ph['interval'].map(nfc)
    print(f'phonetic-tier rows where vs_label == interval label: {agree.mean():.1%}  ({(~agree).sum():,} differ)')
    print(ph.loc[~agree, ['stem', 'interval', 'vs_label', 't_ms']].head(10))

all_relevant_data['t_ms'] = pd.to_numeric(all_relevant_data['t_ms'], errors='coerce')
all_relevant_data['Energy'] = pd.to_numeric(all_relevant_data['Energy'], errors='coerce')

# FIX: group by TOKEN (one interval in one file). The original grouped by
# ['phrase', 'tier', 'interval'], which pooled every participant/repetition (and repeated labels).
g = all_relevant_data.groupby('token_id')

all_relevant_data['t_min'] = g['t_ms'].transform('min')
all_relevant_data['t_max'] = g['t_ms'].transform('max')
all_relevant_data['energy_min'] = g['Energy'].transform('min')
all_relevant_data['energy_max'] = g['Energy'].transform('max')

t_range = all_relevant_data['t_max'] - all_relevant_data['t_min']
e_range = all_relevant_data['energy_max'] - all_relevant_data['energy_min']

# t_prop: 0-100 from first to last VS frame of the token (same definition as the original).
# To use the TextGrid boundaries instead:
#   (t_ms - int_start_ms) / (int_end_ms - int_start_ms) * 100
all_relevant_data['t_prop'] = (all_relevant_data['t_ms'] - all_relevant_data['t_min']) / t_range * 100
all_relevant_data['energy_prop'] = (all_relevant_data['Energy'] - all_relevant_data['energy_min']) / e_range
# FIX: single-frame / flat-energy tokens give inf/NaN: keep them as NaN (the original turned energy_prop into 0)
all_relevant_data[['t_prop', 'energy_prop']] = all_relevant_data[['t_prop', 'energy_prop']].replace([np.inf, -np.inf], np.nan)

# FIX: duration from the TextGrid (the original used max-min of the frames, which is short by ~1 frame)
all_relevant_data['duration'] = all_relevant_data['int_end_ms'] - all_relevant_data['int_start_ms']

# checks
print(all_relevant_data.groupby('token_id')['t_prop'].agg(['min', 'max']).describe())
print('tokens with NaN energy_prop:', all_relevant_data.groupby('token_id')['energy_prop'].apply(lambda s: s.isna().all()).sum())
print(all_relevant_data[['token_id', 'interval', 't_ms', 't_prop', 'energy_prop', 'duration']].head(10))


# %% 6. ADD METADATA
print('Adding metadata...')

stim_meta = pd.read_csv(stim_csv)
stim_meta_df = stim_meta[['Phrasenum', 'IPA', 'Gloss', 'Gloss_2', 'Syllable', 'Segment', 'Type', 'Position',
                          'Position_2', 'Contrast_(IPA)', 'Contrast', 'Experiment']]
stim_meta_df = stim_meta_df.rename(columns={'Phrasenum': 'phrase'})
# FIX: blank rows in the Google sheet would crash astype(int)
stim_meta_df['phrase'] = pd.to_numeric(stim_meta_df['phrase'], errors='coerce')
stim_meta_df = stim_meta_df.dropna(subset=['phrase']).astype({'phrase': int})
# FIX: duplicate Phrasenum rows would silently multiply the data
dups = stim_meta_df.loc[stim_meta_df['phrase'].duplicated(keep=False), 'phrase'].unique()
assert len(dups) == 0, f'Duplicate Phrasenum in stimulus sheet: {dups}'

bad_phrase = all_relevant_data['phrase'].isna()
print(f'Rows with non-numeric phrase (dropped): {bad_phrase.sum():,}')
all_relevant_data = all_relevant_data[~bad_phrase].astype({'phrase': int})

n_before = len(all_relevant_data)
# FIX: left join + validate + indicator, so rows lost to the metadata are reported (original: silent inner join)
all_data = pd.merge(all_relevant_data, stim_meta_df, on='phrase', how='left', validate='m:1', indicator='_stim')
print('Phrases missing from stimulus sheet:', sorted(all_data.loc[all_data['_stim'] == 'left_only', 'phrase'].unique()))
all_data = all_data[all_data['_stim'] == 'both'].drop(columns='_stim')      # same rows as the original inner join

record_data = pd.read_csv(record_csv)
record_data_df = record_data[['Participant', 'Experiment']]
record_data_df = record_data_df.rename(columns={'Experiment': 'Carrier', 'Participant': 'participant'})
record_data_df['participant'] = record_data_df['participant'].astype(str).str.strip()
all_data['participant'] = all_data['participant'].astype(str).str.strip()
all_data = pd.merge(all_data, record_data_df, on='participant', how='left', validate='m:1', indicator='_rec')
print('Participants missing from record:', sorted(all_data.loc[all_data['_rec'] == 'left_only', 'participant'].unique()))
all_data = all_data[all_data['_rec'] == 'both'].drop(columns='_rec')

print(f'Rows: {n_before:,} before merges, {len(all_data):,} after.')

# column order: identifiers first
lead = ['Filename', 'stem', 'participant', 'session', 'phrase', 'tier', 'interval', 'interval_idx', 'token_id',
        'comments', 'vs_label', 'duration', 'int_start_ms', 'int_end_ms', 't_ms', 't_prop', 'energy_prop',
        't_min', 't_max', 'energy_min', 'energy_max']
lead = [c for c in lead if c in all_data.columns]
all_data = all_data[lead + [c for c in all_data.columns if c not in lead]]

print('Saving preproc_output...')
all_data.to_csv(os.path.join(output_dir, f'preproc_output{SUFFIX}.csv'), index=False)   # FIX: this was commented out
print(all_data.head())


# %% 7. (OPTIONAL) COMPARE WITH THE OLD PIPELINE'S all_relevant_data.csv
# Expect: new rows <= old rows. Differences come from boundary frames that were duplicated
# and from substring matches (e.g. '..._1' also pulling '..._12') in the old extraction.
old_path = os.path.join(output_dir, 'all_relevant_data.csv')
if os.path.exists(old_path):
    old = pd.read_csv(old_path, low_memory=False, usecols=['Filename', 'tier', 'interval'])
    new = pd.read_csv(os.path.join(output_dir, f'all_relevant_data{SUFFIX}.csv'),
                      low_memory=False, usecols=['Filename', 'tier', 'interval'])
    print(f'old rows: {len(old):,}   new rows: {len(new):,}')
    cmp = (old.groupby(['Filename', 'tier', 'interval']).size().rename('old').to_frame()
           .join(new.groupby(['Filename', 'tier', 'interval']).size().rename('new'), how='outer').fillna(0))
    cmp['diff'] = cmp['new'] - cmp['old']
    print(cmp['diff'].describe())
    print(cmp[cmp['diff'] != 0].sort_values('diff').head(20))
else:
    print('No old all_relevant_data.csv found, skipping.')


# %% 8. FRICATIVE CLIPS (for multitaper.R) (PARALLEL)
data_dir = textgrid_folder
out_root = fricatives_out

labels = ["s", "sˤ", "ħ", "h", "ʕ", "ʁ", "ð", "ðħ", "sʕ", "ðˤ"]
copy_tier_names = ["phonetic", "phrasenum"]
fric_tier_name = None      # FIX: set this to your fricative tier's NAME (None falls back to the index)
fric_tier_index = 0
overwrite_clips = False

labels_set = {nfc(l) for l in labels}
for lab in labels:
    print(repr(lab), [f'U+{ord(c):04X}' for c in nfc(lab)])      # check codepoints match your TextGrids
    os.makedirs(os.path.join(out_root, lab), exist_ok=True)


# One TextGrid + its wav -> clips + companion TextGrids. Body is the old serial loop (`continue` -> `return`).
def clip_fricatives(tg_filename):
    base = os.path.splitext(tg_filename)[0]
    unmatched = Counter()
    try:
        tg_path = os.path.join(data_dir, tg_filename)

        wav_path = None
        for ext in ('.wav', '.WAV', '.Wav'):
            if os.path.exists(os.path.join(data_dir, base + ext)):
                wav_path = os.path.join(data_dir, base + ext)
                break
        if wav_path is None:
            return dict(base=base, status='no_wav', written=0, unmatched=unmatched, detail='')

        audio, sr = sf.read(wav_path, always_2d=False)
        n_samples = len(audio) if getattr(audio, 'ndim', 1) == 1 else audio.shape[0]
        tg = TextGrid.fromFile(tg_path)

        # tier that holds the fricative labels (FIX: by name if given)
        if fric_tier_name is not None:
            found = [t for t in tg.tiers if nfc(getattr(t, 'name', '')) == nfc(fric_tier_name)]
            fric_tier = found[0] if found else None
        else:
            fric_tier = tg.tiers[fric_tier_index] if fric_tier_index < len(tg.tiers) else None
        if fric_tier is None:
            return dict(base=base, status='no_fricative_tier', written=0, unmatched=unmatched, detail='')

        # collect (start, end) per target label; count labels that did not match (to catch codepoint typos)
        by_label = {lab: [] for lab in labels_set}
        for itv in getattr(fric_tier, 'intervals', []):
            lab = nfc(itv.mark)
            if lab in labels_set and itv.minTime < itv.maxTime:
                by_label[lab].append((float(itv.minTime), float(itv.maxTime)))
            elif lab:
                unmatched[lab] += 1

        name2tier = {nfc(getattr(t, 'name', '')): t for t in tg.tiers}    # FIX: built once per file

        written = 0
        for lab, spans in by_label.items():
            subdir = os.path.join(out_root, lab)
            for i, (start_s, end_s) in enumerate(spans, start=1):
                start_i = max(0, min(int(round(start_s * sr)), n_samples))
                end_i = max(0, min(int(round(end_s * sr)), n_samples))
                if end_i <= start_i:
                    continue

                wav_name = f'{base}_{lab}.wav' if len(spans) == 1 else f'{base}_{lab}-{i:02d}.wav'
                wav_out = os.path.join(subdir, wav_name)
                tg_out = os.path.join(subdir, os.path.splitext(wav_name)[0] + '.TextGrid')
                if os.path.exists(wav_out) and os.path.exists(tg_out) and not overwrite_clips:
                    continue                                              # FIX: restartable

                # 1) audio clip
                sf.write(wav_out, audio[start_i:end_i], sr, subtype='PCM_16')

                # 2) companion TextGrid: copy the wanted tiers, cropped to the clip and shifted to start at 0
                clip_len = float(end_s - start_s)
                clip_tg = TextGrid(minTime=0.0, maxTime=clip_len)
                for wanted in copy_tier_names:
                    new_tier = IntervalTier(name=wanted, minTime=0.0, maxTime=clip_len)
                    src = name2tier.get(nfc(wanted))
                    if src is not None and hasattr(src, 'intervals'):
                        for itv in src.intervals:
                            a, b = float(itv.minTime), float(itv.maxTime)
                            if b <= start_s or a >= end_s:
                                continue
                            na, nb = max(a, start_s) - start_s, min(b, end_s) - start_s
                            if nb - na > 1e-9:
                                new_tier.addInterval(Interval(na, nb, itv.mark))
                    clip_tg.append(new_tier)
                clip_tg.write(tg_out)

                written += 1
        return dict(base=base, status='ok', written=written, unmatched=unmatched, detail='')
    except Exception:
        return dict(base=base, status='error', written=0, unmatched=unmatched,
                    detail=traceback.format_exc(limit=3))


# --- run ---
results = Parallel(n_jobs=N_JOBS, return_as='generator')(delayed(clip_fricatives)(f) for f in tg_filenames)
fric_results = list(tqdm(results, total=len(tg_filenames), desc='Clipping fricatives'))

fric_log = pd.DataFrame([{k: v for k, v in r.items() if k != 'unmatched'} for r in fric_results])
fric_log.to_csv(os.path.join(report_dir, 'fricative_log.csv'), index=False)
unmatched = Counter()
for r in fric_results:
    unmatched.update(r['unmatched'])

print('\nSummary')
print('=======')
print(f'TextGrids processed: {len(tg_filenames)}')
print(fric_log['status'].value_counts())
print(f'Total clips written: {fric_log["written"].sum()}')
print(f'Output root: {out_root}')
print(fric_log[~fric_log['status'].isin(['ok'])].head(20))
print('Most common non-target labels in the fricative tier (look for sˤ vs sʕ style codepoint mismatches):')
for lab, n in unmatched.most_common(25):
    print(f'  {lab!r:12} x{n:<5} {[f"U+{ord(c):04X}" for c in lab]}')

### pharylary preprocessing: BACK HALF (fricative metadata, target flags, reports)
# Runs on its own, in a fresh session (new terminal). Cell 9 does the imports, config and loads:
#   - preproc_output<SUFFIX>.csv   written by cell 6 of the main script (all_data, before fricative metadata)
#   - fricatives/*.csv             written by multitaper.R (read in cell 9c)
# Run order: main script cells 1-6 and 8 -> multitaper.R -> this file.
# Lines marked "# FIX" are where this differs from your previous back half.
# The long commented-out "old code using the extractions from the dicanio script" block is left out;
# paste it back at the end if you want to keep it.

#################################################
#                                               #
#       GO TO MULTITAPER MODIFIED SCRIPT        #
#                                               #
#################################################

# %% 9. SETUP + LOAD (everything this half needs, so it can run in a new terminal)
import os
import re
import glob
import time
import unicodedata

import numpy as np
import pandas as pd

# keep these identical to the main script's config
base_dir = '/Volumes/circe/alldata/dissertation/vs'
# base_dir = '/Volumes/cassandra/alldata/dissertation/vs'
SUFFIX = '_v2'

output_dir = base_dir + '/output_preproc'
report_dir = os.path.join(output_dir, 'reports')
csv_dir = os.path.join(output_dir, 'fricatives')   # folder with the multitaper.R CSVs (one row per clip, 'stim' column)
os.makedirs(report_dir, exist_ok=True)

# A phonetic token counts as THE TARGET if at least this fraction of its frames lie inside a labelled
# V-sequence interval of the same file. 1.0 = must be fully inside; 0.5 = majority inside.
TARGET_MIN_FRAC = 0.5

# one-line helper (same as the main script)
nfc = lambda s: unicodedata.normalize('NFC', (s or '').strip())

# --- load all_data as saved by cell 6 of the main script ---
all_data_path = os.path.join(output_dir, f'preproc_output{SUFFIX}.csv')
assert os.path.exists(all_data_path), f'{all_data_path} not found - run cells 1-6 of the main script first'
# keep_default_na=False: only empty cells become NaN, so a phone/word label like 'NA' or 'null' is not turned into NaN
all_data = pd.read_csv(all_data_path, low_memory=False, keep_default_na=False, na_values=[''])
print(f'Loaded {all_data_path}: {all_data.shape}')

needed = ['stem', 'participant', 'phrase', 'tier', 'interval', 'interval_idx', 'token_id', 'comments', 't_ms', 'Segment']
missing_cols = [c for c in needed if c not in all_data.columns]
assert not missing_cols, f'all_data is missing columns {missing_cols} - was it made by the current main script?'
# this file must be the PRE-fricative one; this half never overwrites it (see 9d/9e)
assert 'fric_stim' not in all_data.columns and 'target_token' not in all_data.columns, (
    f'{all_data_path} already contains back-half columns (an older back half overwrote it). '
    'Rerun cell 6 of the main script to regenerate it.')


# %% 9a. FLAG THE TARGET SEGMENT (phonetic tokens that sit inside a labelled V-sequence interval)
t0 = time.time()
all_data = all_data.copy()
all_data['t_ms'] = pd.to_numeric(all_data['t_ms'], errors='coerce')

# FIX: frames inside the V-sequence tier are keyed by FILE (stem) + time. The old key was (phrase, t_ms),
# which let one participant's V-sequence frames mark another participant's phonetic frames as targets.
# (V-sequence rows only exist for labelled intervals, because empty intervals are skipped in cell 3.)
vseq_idx = pd.MultiIndex.from_frame(
    all_data.loc[all_data['tier'].eq('V-sequence'), ['stem', 't_ms']].drop_duplicates())

mask_phonetic = all_data['tier'].eq('phonetic')
phon_idx = pd.MultiIndex.from_frame(all_data.loc[mask_phonetic, ['stem', 't_ms']])

# frame level (same meaning as your old target_match, now within a file)
all_data['target_match'] = 0
all_data.loc[mask_phonetic, 'target_match'] = phon_idx.isin(vseq_idx).astype(int)

# FIX: token level. A repeated segment gets its own token_id, so the copy outside the V-sequence
# interval is excluded and the copy inside it is kept.
all_data['target_frac'] = np.nan
all_data.loc[mask_phonetic, 'target_frac'] = (
    all_data.loc[mask_phonetic].groupby('token_id')['target_match'].transform('mean'))
all_data['target_token'] = (all_data['target_frac'] >= TARGET_MIN_FRAC).astype(int)   # NaN (non-phonetic) -> 0

# checks
ph_tok = all_data[all_data['tier'].eq('phonetic')].drop_duplicates('token_id')
f = ph_tok['target_frac']
print(f"Done in {time.time() - t0:.1f}s. Phonetic tokens: {len(ph_tok):,}")
print(f"  fully inside a V-sequence interval: {(f == 1).sum():,} | partly inside: {((f > 0) & (f < 1)).sum():,} "
      f"| outside: {(f == 0).sum():,}")
print(f"  flagged as target (frac >= {TARGET_MIN_FRAC}): {int((ph_tok['target_token'] == 1).sum()):,}")
print('Examples of partly-inside tokens (check boundaries / TARGET_MIN_FRAC):')
print(ph_tok.loc[(f > 0) & (f < 1), ['token_id', 'interval', 'target_frac']].head(10))
rep = ph_tok[ph_tok['target_token'] == 1].groupby(['stem', 'interval']).size()
print(f'Files where the same label is a target more than once: {(rep > 1).sum()}')


# %% 9b. NUMBER REPEATED SEGMENTS WITHIN A FILE (needed to match clips like '..._s-02.wav')
# nfc on the unique labels only (fast); labels are compared in NFC, like the clip file names are.
nfc_map = {v: nfc(v) for v in all_data['interval'].astype(str).unique()}
all_data['interval_nfc'] = all_data['interval'].astype(str).map(nfc_map)

# occurrence = 1, 2, ... for the same label within one file's phonetic tier, in TextGrid order
occ = (all_data.loc[all_data['tier'].eq('phonetic'), ['stem', 'interval_nfc', 'interval_idx']]
       .drop_duplicates().sort_values('interval_idx'))
occ['occurrence'] = occ.groupby(['stem', 'interval_nfc']).cumcount() + 1
occ['occurrence'] = occ['occurrence'].astype(float)
all_data = all_data.merge(occ, on=['stem', 'interval_nfc', 'interval_idx'], how='left', validate='m:1')
print(all_data.loc[all_data['tier'].eq('phonetic'), 'occurrence'].value_counts().sort_index())


# %% 9c. MERGE THE MULTITAPER (FRICATIVE) MEASUREMENTS ONTO THE MATCHING PHONETIC TOKEN
csv_files = sorted(glob.glob(os.path.join(csv_dir, '*.csv')))
print(f'Found {len(csv_files)} CSV(s) in {csv_dir}')
fric_list = []
for fpath in csv_files:
    try:
        df = pd.read_csv(fpath)
    except Exception as e:
        print(f'Skipping {fpath}: {e}')
        continue
    if 'stim' not in df.columns:
        print(f"Skipping {fpath}: missing 'stim' column")
        continue
    fric_list.append(df)
assert fric_list, 'No multitaper CSVs loaded - run multitaper.R first (or skip this cell).'
fric = pd.concat(fric_list, ignore_index=True)

# stim looks like SD0001_expt_1_100_1_ð.wav (stem = SD0001_expt_1_100_1), or SD0001_expt_1_100_1_ð-02.wav when the label occurs
# more than once in that TextGrid (cell 8 numbers those clips -01, -02, ...)
stim_base = fric['stim'].astype(str).map(os.path.basename).str.replace(r'\.wav$', '', regex=True, flags=re.I)
parts = stim_base.str.split('_')
ok = parts.str.len() >= 5
print(f'{int((~ok).sum())} rows with malformed stim names dropped')
fric, parts = fric[ok].copy(), parts[ok]

# FIX: join on the FILE (stem = everything before the segment) instead of (participant, phrase).
# Two files with the same participant and phrase (e.g. ..._1_100_1 and ..._1_100_2) used to collide.
fric['stem'] = parts.str[:-1].str.join('_')
seg = parts.str[-1].str.extract(r'^(.*?)(?:-(\d+))?$')                  # 'ð-02' -> label 'ð', occurrence '02'
fric['interval_nfc'] = seg[0].map(nfc)                                    # FIX: compare labels in NFC
fric['occurrence'] = pd.to_numeric(seg[1], errors='coerce').fillna(1).astype(float)   # FIX: '-02' no longer breaks the label

keys = ['stem', 'interval_nfc', 'occurrence']
dups = fric.duplicated(keys, keep=False)
print(f'{int(dups.sum())} clip rows share a (stem, segment, occurrence) key; keeping the last of each')
metadata = fric.rename(columns={'stim': 'fric_stim'}).drop_duplicates(subset=keys, keep='last')

# FIX: columns present in both tables (e.g. 'duration') keep the all_data version; the multitaper copy gets '_fric'
shared = [c for c in metadata.columns if c in all_data.columns and c not in keys]
print('Columns in both tables (multitaper copies get a _fric suffix):', shared)

n_rows = len(all_data)
all_data = all_data.merge(metadata, on=keys, how='left', suffixes=('', '_fric'), validate='m:1')
assert len(all_data) == n_rows, 'merge changed the number of rows'
print(f'Merged metadata onto all_data. New shape: {all_data.shape}')

# checks: where did the clips land?
tok = all_data[all_data['tier'].eq('phonetic')].drop_duplicates('token_id')
has_fric = tok['fric_stim'].notna()
is_target = tok['target_token'] == 1
print(f'Target tokens: {int(is_target.sum()):,} | with a multitaper row: {int((is_target & has_fric).sum()):,} '
      f'| WITHOUT one: {int((is_target & ~has_fric).sum()):,}')
print(f'Multitaper rows attached to a NON-target repeat (kept in all_data, excluded from the targets-only file): '
      f'{int((~is_target & has_fric).sum()):,}')
chk = metadata.merge(tok[keys].drop_duplicates(), on=keys, how='left', indicator=True)
print(f"Multitaper rows that match no phonetic token at all: {int((chk['_merge'] == 'left_only').sum()):,}")
print(chk.loc[chk['_merge'] == 'left_only', 'fric_stim'].head(10).tolist())
print('Target tokens without a multitaper row (first 10):')
print(tok.loc[is_target & ~has_fric, 'token_id'].head(10).tolist())


# %% 9d. IGNORE FLAG + SAVE + REPORTS
# Flag rows where "comments" contains "ignore" (case-insensitive)
all_data['ignore_flag'] = (
    all_data['comments']
    .astype(str)
    .str.contains('ignore', case=False, na=False)
    .astype(int)
)
print(f"Flagged {all_data['ignore_flag'].sum()} rows containing 'ignore'.")

# FIX: the old 'preproc_output.csv' save is gone. It wrote to the working directory (os.chdir(output_dir) no longer
# exists), and writing preproc_output<SUFFIX>.csv here would overwrite the file cell 9 reloads. Everything is
# saved in 9e instead (preproc_matchesformeans = all rows incl. ignore_flag; preproc_targets_only = targets).

# Make sure phrase values are numeric
all_data['phrase'] = pd.to_numeric(all_data['phrase'], errors='coerce')

# Build report of missing phrases per participant
missing_report = []
for participant, df_part in all_data.groupby('participant'):
    phrases = sorted(df_part['phrase'].dropna().unique())
    if len(phrases) == 0:
        continue
    min_phrase = int(np.nanmin(phrases))
    max_phrase = int(np.nanmax(phrases))
    expected = set(range(min_phrase, max_phrase + 1))
    actual = set(phrases)
    missing = sorted(expected - actual)
    missing_report.append({
        'participant': participant,
        'min_phrase': min_phrase,
        'max_phrase': max_phrase,
        'total_expected': len(expected),
        'total_present': len(actual),
        'total_missing': len(missing),
        'missing_phrases': missing
    })
missing_report_df = pd.DataFrame(missing_report)
output_csv = os.path.join(report_dir, 'missing_phrases_by_participant.csv')
missing_report_df.to_csv(output_csv, index=False)
print(f"Saved missing-phrase report to: {output_csv}")

# Filter for phonetic-tier rows and keep only relevant columns
phonetic_df = all_data.loc[
    all_data['tier'] == 'phonetic',
    ['participant', 'phrase', 'interval', 'Segment']
].dropna()

# Group by interval, phrase, and Segment, collecting unique participants
interval_participants = (
    phonetic_df
    .groupby(['interval', 'phrase', 'Segment'])['participant']
    .unique()
    .reset_index()
    .sort_values(['interval', 'phrase', 'Segment'])
    .reset_index(drop=True)
)
output_csv = os.path.join(report_dir, 'phonetic_intervals_by_phrase_segment_participant.csv')
interval_participants.to_csv(output_csv, index=False)
print(f"Saved phonetic interval labels by phrase, segment, and participant to: {output_csv}")


# %% 9e. SAVE: everything (flagged) and target segments only
print('Saving matches for means as .csv in %s.' % output_dir)
all_data.to_csv(os.path.join(output_dir, f'preproc_matchesformeans{SUFFIX}.csv'), index=False)

targets = all_data[all_data['target_token'] == 1]
targets.to_csv(os.path.join(output_dir, f'preproc_targets_only{SUFFIX}.csv'), index=False)
print(f'Targets only: {len(targets):,} rows, {targets["token_id"].nunique():,} tokens')
