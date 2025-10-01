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

output_dir = '/Volumes/circe/alldata/dissertation/vs/output_preproc'
textgrid_folder = '/Volumes/circe/alldata/dissertation/vs/input_preproc'
vs_output_dir = '/Volumes/circe/alldata/dissertation/vs/vs_output'
fricative_output_dir = '/Volumes/circe/alldata/dissertation/vs/fricative_output'

# output_dir = '/Volumes/cassandra/alldata/dissertation/vs/output_preproc'
# textgrid_folder = '/Volumes/cassandra/alldata/dissertation/vs/input_preproc'
# vs_output_dir = '/Volumes/cassandra/alldata/dissertation/vs/vs_output'
# fricative_output_dir = '/Volumes/cassandra/alldata/dissertation/vs/fricative_output'

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

# Initialize an empty DataFrame to store all relevant data
all_relevant_data = pd.DataFrame()

# List or dictionary of tiers to process
tiers_to_process = ['phonetic', 'glottis', 'V-sequence', 'word']

# Identify your point tier
# point_tier = tg.getFirst('comment')




# time_start = time.ctime()
# print("Start time:", time_start)
# seconds_start = time.time()

# for filename in os.listdir(textgrid_folder):
#     if not filename.endswith('.TextGrid'):
#         continue

#     # Build expected output filename for this TextGrid
#     output_csv = os.path.join(output_dir, f"{os.path.splitext(filename)[0]}_relevant.csv")

#     # Skip if CSV already exists
#     if os.path.exists(output_csv):
#         print(f"Skipping {filename} — output already exists at {output_csv}")
#         continue

#     filepath = os.path.join(textgrid_folder, filename)
#     tg = textgrid.TextGrid.fromFile(filepath)
#     print(f'Extracting and appending {filename} TextGrid labels.')

#     point_tier = tg.getFirst('comment')
#     tiers = {tier_name: tg.getFirst(tier_name) for tier_name in tiers_to_process}
#     phrasenum_tier = tg.getFirst('phrasenum')

#     valid_phr_intervals = [pi for pi in phrasenum_tier if (pi.mark or '').strip() != '']
#     pbar = tqdm(total=len(valid_phr_intervals), desc=f"{filename}", unit="phrase", leave=True)

#     # Collect relevant rows for this one TextGrid
#     tg_relevant_data = pd.DataFrame()

#     for phrasenum_interval in phrasenum_tier:
#         if phrasenum_interval.mark is not str(''):
#             phrasenum_value = phrasenum_interval.mark

#             # Subset the VoiceSauce output for the current phrasenum
#             # print("Subsetting...")
#             subset_voicesauce = voicesauce_data[voicesauce_data['Label'] == int(phrasenum_value)]

#             for tier_name, tier in tiers.items():
#                 for interval in tier:
#                     if interval.mark != '':

#                         start_time = interval.minTime * 1000
#                         end_time   = interval.maxTime * 1000

#                         relevant_data = subset_voicesauce[
#                             (subset_voicesauce['t_ms'] >= start_time) &
#                             (subset_voicesauce['t_ms'] <= end_time)
#                         ].copy()

#                         points_in_interval = [
#                             point.mark for point in point_tier
#                             if phrasenum_interval.minTime <= point.time <= phrasenum_interval.maxTime and point.mark
#                         ]

#                         relevant_data['comments'] = ', '.join(points_in_interval)
#                         relevant_data['interval'] = interval.mark
#                         relevant_data['tier']     = tier.name

#                         cols = relevant_data.columns.tolist()
#                         cols.insert(2, cols.pop(cols.index('interval')))
#                         cols.insert(3, cols.pop(cols.index('tier')))
#                         cols.insert(4, cols.pop(cols.index('comments')))
#                         relevant_data = relevant_data[cols]

#                         tg_relevant_data = pd.concat([tg_relevant_data, relevant_data], ignore_index=True)

#             pbar.update(1)

#         pbar.close()

#     # Save this TextGrid's relevant_data to CSV
#     tg_relevant_data.to_csv(output_csv, index=False)
#     print(f"Saved {len(tg_relevant_data)} rows to {output_csv}")

#     # (Optional) append to master dataframe if you want everything combined
#     all_relevant_data = pd.concat([all_relevant_data, tg_relevant_data], ignore_index=True)

# time_end = time.ctime()
# print("End time:", time_end)
# seconds_end = time.time()
# print("Time elapsed (minutes): ", (seconds_end - seconds_start) / 60)
# print('Processing all data for min, max, proportion, etc.')


# from tqdm.auto import tqdm

# time_start = time.ctime()
# print("Start time:", time_start)
# seconds_start = time.time()

# for filename in os.listdir(textgrid_folder):
#     if not filename.endswith('.TextGrid'):
#         continue

#     filepath = os.path.join(textgrid_folder, filename)
#     tg = textgrid.TextGrid.fromFile(filepath)
#     print(f'Extracting and appending {filename} TextGrid labels.')

#     point_tier = tg.getFirst('comment')
#     tiers = {tier_name: tg.getFirst(tier_name) for tier_name in tiers_to_process}
#     phrasenum_tier = tg.getFirst('phrasenum')

#     # progress counted by phrasenum intervals
#     valid_phr_intervals = [pi for pi in phrasenum_tier if (pi.mark or '').strip() != '']
#     pbar = tqdm(total=len(valid_phr_intervals), desc=f"{filename}", unit="phrase", leave=True)

#     for phrasenum_interval in phrasenum_tier:
#         pmark = (phrasenum_interval.mark or '').strip()
#         if pmark == '':
#             continue

#         phrasenum_value = int(pmark)
#         subset_voicesauce = voicesauce_data[voicesauce_data['Label'] == phrasenum_value]

#         for tier_name, tier in tiers.items():
#             for interval in tier:
#                 if (interval.mark or '').strip() == '':
#                     continue

#                 start_time = interval.minTime * 1000
#                 end_time   = interval.maxTime * 1000

#                 relevant_data = subset_voicesauce[
#                     (subset_voicesauce['t_ms'] >= start_time) &
#                     (subset_voicesauce['t_ms'] <= end_time)
#                 ].copy()

#                 points_in_interval = [
#                     point.mark for point in point_tier
#                     if phrasenum_interval.minTime <= point.time <= phrasenum_interval.maxTime and point.mark
#                 ]

#                 relevant_data['comments'] = ', '.join(points_in_interval)
#                 relevant_data['interval'] = interval.mark
#                 relevant_data['tier']     = tier.name

#                 cols = relevant_data.columns.tolist()
#                 cols.insert(2, cols.pop(cols.index('interval')))
#                 cols.insert(3, cols.pop(cols.index('tier')))
#                 cols.insert(4, cols.pop(cols.index('comments')))
#                 relevant_data = relevant_data[cols]

#                 all_relevant_data = pd.concat([all_relevant_data, relevant_data], ignore_index=True)

#         # one update per phrasenum
#         pbar.update(1)

#     pbar.close()

# time_end = time.ctime()
# print("End time:", time_end)
# seconds_end = time.time()
# print("Time elapsed (minutes): ", (seconds_end - seconds_start) / 60)
# print('Processing all data for min, max, proportion, etc.')

time_start = time.ctime()
print("Start time:", time_start)
seconds_start = time.time()

# Iterate over each file in the TextGrid folder
for filename in os.listdir(textgrid_folder):
    if filename.endswith('.TextGrid'):  # Check if the file is a TextGrid file
        filepath = os.path.join(textgrid_folder, filename)
        tg = textgrid.TextGrid.fromFile(filepath)
        print ('Extracting and appending %s TextGrid labels.'%(filename))
        # # Identify your point tier
        point_tier = tg.getFirst('comment')

        # Assign variables for each tier
        # print("Assigning tier variables...")
        tiers = {tier_name: tg.getFirst(tier_name) for tier_name in tiers_to_process}
        phrasenum_tier = tg.getFirst('phrasenum')

        # Build expected output filename for this TextGrid
        output_csv = os.path.join(output_dir, f"{os.path.splitext(filename)[0]}_relevant.csv")

        # Skip if CSV already exists
        if os.path.exists(output_csv):
            print(f"Skipping {filename} — output already exists at {output_csv}")
            continue

        # Process each phrasenum interval
        for phrasenum_interval in phrasenum_tier:
            if phrasenum_interval.mark is not str(''):
                phrasenum_value = phrasenum_interval.mark

                # Subset the VoiceSauce output for the current phrasenum
                # print("Subsetting...")
                subset_voicesauce = voicesauce_data[voicesauce_data['Label'] == int(phrasenum_value)]

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
                            all_relevant_data = pd.concat([all_relevant_data, relevant_data], ignore_index=True)


time_end = time.ctime()
print("End time:", time_end)
seconds_end = time.time()
print("Time elapsed (minutes): ", (seconds_end-seconds_start)/60)



print('Processing all data for min, max, proportion, etc.')

# Drop the 'Label' column
all_relevant_data.drop(['Label','Unnamed: 72'], axis=1, inplace=True)

# Split the 'Filename' column into separate parts
filename_parts = all_relevant_data['Filename'].str.split('_', expand=True)

# Assign new columns based on the split parts
# Assuming the format is consistent and the desired parts are at specific indices
all_relevant_data['participant'] = filename_parts[0]
all_relevant_data['session'] = filename_parts[2]
all_relevant_data['phrase'] = filename_parts[4]

# Determine the position of the 'comments' column
comments_col_idx = all_relevant_data.columns.get_loc('interval') - 1

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
tier_min_max = all_relevant_data.groupby(['phrase', 'tier', 'interval'])['t_ms'].agg([min, max]).reset_index()
energy_min_max = all_relevant_data.groupby(['phrase','tier','interval'])['Energy'].agg([min,max]).reset_index()

## merge to df
all_relevant_data = all_relevant_data.merge(tier_min_max, on=['phrase', 'tier', 'interval'])
## rename the max and min column
all_relevant_data = all_relevant_data.rename(columns={'min':'t_min','max':'t_max'})

## same as above, except for RMS to normalize within participants
all_relevant_data = all_relevant_data.merge(energy_min_max, on=['phrase', 'tier', 'interval'])
all_relevant_data = all_relevant_data.rename(columns={'min':'energy_min','max':'energy_max'})

### proportion column
all_relevant_data['t_prop'] = (all_relevant_data['t_ms'] - all_relevant_data['t_min']) / (all_relevant_data['t_max'] - all_relevant_data['t_min'])
all_relevant_data['t_prop'] = all_relevant_data['t_prop'] * 100

all_relevant_data['energy_prop'] = (all_relevant_data['Energy'] - all_relevant_data['energy_min']) / (all_relevant_data['energy_max'] - all_relevant_data['energy_min'])
# all_relevant_data['energy_prop'] = all_relevant_data['energy_prop'] * 100

### duration column
all_relevant_data['duration'] = all_relevant_data['t_max'] - all_relevant_data['t_min']

### add in other data from google sheet ###
print('Adding metadata...')
stim_meta = pd.read_csv('/Users/bcl/GitHub/pharylary/PharyLary Stimuli - Yes.csv')
stim_meta_df = stim_meta[['Phrasenum','IPA','Gloss','Gloss_2','Syllable','Segment','Type','Position','Position_2','Contrast_(IPA)','Contrast','Experiment']]
stim_meta_df = stim_meta_df.rename(columns={'Phrasenum':'phrase'})
stim_meta_df = stim_meta_df.astype({'phrase': int})
all_relevant_data = all_relevant_data.astype({'phrase': int})
all_data = pd.merge(all_relevant_data, stim_meta_df, on='phrase', how='inner')


# Output the final DataFrame
# print(all_relevant_data)
# Optionally, save to a file
print('Saving as .csv in %s.'%(output_dir))
all_data.to_csv('preproc_output.csv', index=False)

### NEW SECTION: Import and concatenate fricative measurement data ###
# Step 1: Locate all text files in the fricative_output_dir ending with '_logfile_s'
fricative_files = glob.glob(os.path.join(fricative_output_dir, '*.txt'))

# Step 2: Read and concatenate these files into a single DataFrame
fricative_data = pd.DataFrame()  # Initialize an empty DataFrame

for file in fricative_files:
    # Read each file
    temp_df = pd.read_csv(file, sep='\t')  # Assuming tab-separated files
    fricative_data = pd.concat([fricative_data, temp_df], ignore_index=False)

# Step 3: Output the concatenated DataFrame for verification
print(f"Concatenated fricative data contains {len(fricative_data)} rows and {len(fricative_data.columns)} columns.")

# Subset the data based on matching phrasenum values
phrasenum_values = [1, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 37, 39, 41, 43, 45, 47, 49, 50, 53, 55, 57, 59, 60, 62, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 134, 135, 136, 137, 138, 139, 141, 142, 143, 145, 146, 149, 150, 156, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172]

# Convert phrasenum_values to string to match the object type in fricative_data
phrasenum_values = list(map(str, phrasenum_values))

# Filter rows based on phrasenum_values
fricative_data = fricative_data[fricative_data['phrasenum'].astype(str).isin(phrasenum_values)]

# Output the subset DataFrame for verification
print(f"Subset fricative data contains {len(fricative_data)} rows after filtering by phrasenum.")

# Print the count of total phrasenum values by label
label_phrasenum_counts = fricative_data.groupby('label')['phrasenum'].nunique()
print("Counts of unique phrasenum values by label:")
print(label_phrasenum_counts)

#### ADD HERE SECTION for matching the above fricatives with the metadata.
### each row in the fricative thing should already be a target phoneme, then export

# Load the metadata CSV file
metadata_path = os.path.join('/Users/bcl/GitHub/pharylary/PharyLary Stimuli - pharylary2.csv')  # Update the path to your metadata file
metadata = pd.read_csv(metadata_path)
metadata = metadata[['Phrasenum','IPA','Gloss','Gloss_2','Syllable','Segment','Type','Position','Position_2','Contrast_(IPA)','Contrast','Experiment']]

# Ensure the Phrasenum column in metadata is of the same type as the phrasenum column in fricative_data
metadata['Phrasenum'] = metadata['Phrasenum'].astype(str)
fricative_data['phrasenum'] = fricative_data['phrasenum'].astype(str)

# Merge fricative_data with metadata on phrasenum/Phrasenum
fricative_data = pd.merge(
    fricative_data,
    metadata,
    how='left',  # Use 'left' join to preserve the shape of fricative_data
    left_on='phrasenum',
    right_on='Phrasenum'
)

# Output the resulting DataFrame for verification
print(f"fricative_data now contains {len(fricative_data)} rows and {len(fricative_data.columns)} columns after merging with metadata.")

### Save the final subset data ###
fricative_data.to_csv(os.path.join(output_dir, 'subset_fricative_data.csv'), index=False)


#### matches for means ####
##### data for the intervals as extracted from overlap with tier 3 because there's no unique labels in tier 1.
##### this makes it so the data is just the target phonemes, not all of them
### slicing the data below takes about 5 minutes to run because iterrows() is slow ####
#Initialize an empty DataFrame to store matching rows
time_start = time.ctime()
print("Start time:", time_start)
seconds_start = time.time()

print('Getting matching data for means...')
matching_data = pd.DataFrame()

# Iterate through each unique phrase
for phrase in all_data['phrase'].unique():

    # Filter data for the current phrase
    phrase_data = all_data[all_data['phrase'] == phrase]

    # Separate interval and V-sequence data within this phrase
    interval_data = phrase_data[phrase_data['tier'] == 'phonetic']
    v_sequence_data = phrase_data[phrase_data['tier'] == 'V-sequence']

    # Iterate through the interval data
    for index, interval_row in interval_data.iterrows():
        # Check if this interval t_ms matches any V-sequence t_ms in the same phrase
        if any(interval_row['t_ms'] == v_sequence_row.t_ms for v_sequence_row in v_sequence_data.itertuples()):
            # Append matching row to the matching_data DataFrame
            print('Appending match for %s phrase %s...'%(interval_row['participant'], phrase))
            matching_data = matching_data.append(interval_row)


time_start = time.ctime()
print("Start time:", time_start)
seconds_start = time.time()

print('Getting matching data for means...')
matching_data = pd.DataFrame()

# Initialize a list to collect matching rows
matching_rows = []

# Iterate through each unique phrase
for phrase in all_data['phrase'].unique():
    print('Appending match for %s phrase %s...' % (interval_row['participant'], phrase))
    # Filter data for the current phrase
    phrase_data = all_data[all_data['phrase'] == phrase]

    # Separate interval and V-sequence data within this phrase
    interval_data = phrase_data[phrase_data['tier'] == 'phonetic']
    v_sequence_data = phrase_data[phrase_data['tier'] == 'V-sequence']

    # Create a set of t_ms values from V-sequence data for faster lookup
    v_sequence_t_ms_set = set(v_sequence_data['t_ms'])

    # Iterate through the interval data
    for index, interval_row in interval_data.iterrows():
        # Check if this interval t_ms matches any V-sequence t_ms in the same phrase
        if interval_row['t_ms'] in v_sequence_t_ms_set:
            # Collect the matching row
            matching_rows.append(interval_row)

# Convert the list of matching rows to a DataFrame
matching_rows_df = pd.DataFrame(matching_rows)

# Use pd.concat to append all matching rows to the matching_data DataFrame at once
matching_data = pd.concat([matching_data, matching_rows_df], ignore_index=True)


time_end = time.ctime()
print("End time:", time_end)
seconds_end = time.time()
print("Time elapsed (minutes): ", (seconds_end-seconds_start)/60)

print('Saving matches for means as .csv in %s.'%(output_dir))
matching_data.to_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', index=False)
# matching_data = pd.read_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', encoding='utf8')
