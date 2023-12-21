### pharylary preprocessing script
# this script takes individual outputs from VoiceSauce and different tiers from TextGrids
# to create individual entries of acoustic values at each millisecond according to teh different intervals in the TextGrid tiers

import pandas as pd
import numpy as np
import os
import glob
import textgrid

output_dir = '/Volumes/circe/vs/output_preproc'
input_dir = '/Volumes/circe/vs/input_preproc'
vs_output_dir = '/Volumes/circe/vs/vs_output'

os.chdir(output_dir)

# vs_output = pd.read_csv(os.path.join(output_dir, 'output.txt'), sep='\t')
# tg = textgrid.TextGrid.fromFile(os.path.join(input_dir, 'Y0395_expt_1_1_1.TextGrid'))
# point_tier = tg.getFirst('comment')

# Path to the directory containing TextGrid files
textgrid_folder = input_dir

# Load the VoiceSauce output
voicesauce_data = pd.read_csv(os.path.join(vs_output_dir, 'output.csv')) # should be a text file if there were no duplicates that needed to be redone by hand

# Initialize an empty DataFrame to store all relevant data
all_relevant_data = pd.DataFrame()

# List or dictionary of tiers to process
tiers_to_process = ['phonetic', 'glottis', 'V-sequence', 'word']

# Identify your point tier
# point_tier = tg.getFirst('comment')


# Iterate over each file in the TextGrid folder
for filename in os.listdir(textgrid_folder):
    if filename.endswith('.TextGrid'):  # Check if the file is a TextGrid file
        filepath = os.path.join(textgrid_folder, filename)
        tg = textgrid.TextGrid.fromFile(filepath)
        # # Identify your point tier
        point_tier = tg.getFirst('comment')

        # Assign variables for each tier
        tiers = {tier_name: tg.getFirst(tier_name) for tier_name in tiers_to_process}
        phrasenum_tier = tg.getFirst('phrasenum')

        # Process each phrasenum interval
        for phrasenum_interval in phrasenum_tier:
            if phrasenum_interval.mark is not str(''):
                phrasenum_value = phrasenum_interval.mark

                # Subset the VoiceSauce output for the current phrasenum
                subset_voicesauce = voicesauce_data[voicesauce_data['Label'] == int(phrasenum_value)]

                # Process each tier
                for tier_name, tier in tiers.items():
                    for interval in tier:
                        if interval.mark != '':
                            start_time = interval.minTime * 1000
                            end_time = interval.maxTime * 1000

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

                            # Append the relevant data to the DataFrame
                            all_relevant_data = pd.concat([all_relevant_data, relevant_data], ignore_index=True)

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
stim_meta = pd.read_csv('/Users/bcl/GitHub/pharylary/PharyLary Stimuli - Yes.csv')
stim_meta_df = stim_meta[['Phrasenum','IPA','Gloss','Gloss 2','Syllable','Segment','Type','Position','Position 2','Contrast (IPA)','Contrast','Experiment']]
stim_meta_df = stim_meta_df.rename(columns={'Phrasenum':'phrase'})
stim_meta_df = stim_meta_df.astype({'phrase': int})
all_relevant_data = all_relevant_data.astype({'phrase': int})
all_data = pd.merge(all_relevant_data, stim_meta_df, on='phrase', how='inner')


# Output the final DataFrame
# print(all_relevant_data)
# Optionally, save to a file
all_data.to_csv('preproc_output.csv', index=False)



#### matches for means ####
### slicing the data below takes about 5 minutes to run because iterrows() is slow ####
#Initialize an empty DataFrame to store matching rows
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
            matching_data = matching_data.append(interval_row)

matching_data.to_csv('preproc_matchesformeans.csv', index=False)
# matching_data = pd.read_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', encoding='utf8')
