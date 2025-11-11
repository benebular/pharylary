import pandas as pd
import os

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
output_csv = os.path.join(output_dir, 'phonetic_intervals_by_phrase_segment_participant.csv')

# Save to CSV
interval_participants.to_csv(output_csv, index=False)

print(f"Saved phonetic interval labels by phrase, segment, and participant to: {output_csv}")
