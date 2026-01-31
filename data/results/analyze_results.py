import pandas as pd
import numpy as np
from scipy import stats

# Load data
results = pd.read_csv('pharylary_survey_results.csv')
trials_meta = pd.read_csv('../trials.csv')

# Filter to only completed participants (from Prolific)
completed_ids = [
    '68d4239be163cb15b16606a6',
    '68c03814183fb278ae8ac1c9',
    '5c1d19c810677f0001d9d56c',
    '68d40473576087257c6dcaf3',
    '611b00c6f6cc82766cd07c16',
    '68d4670874bf80ef3de3834d',
    '691290f8a9d2ff4090233076',
    '65f04e28869d1e36bcfe9bc2',
    '68d6aebcb40c4b93989642c1',
    '694ef288fcfc4439fcc94ef9',
    '68e3cfc22688ddaae48dc6c3',
    '68c831e4060bd0ef510259ca',
    '68d3fde63f2c4007620e15d0',
    '68cab84ebe4b9f5d7148e3dc',
    '68d43a3c7fb9b4c1a9eee9af',
    '6960de0ad3223c83f5fbdf92',
    '695ff3fde7fd7419df424235',
    '68c89b71500cd48edf4fefe3',
    '68c830029bf5aaec07eaa790',
    '66c0f6b657bd4b8a29485652'
]

print("=" * 80)
print("FILTERING TO COMPLETED PARTICIPANTS")
print("=" * 80)
print(f"\nTotal rows before filtering: {len(results)}")
print(f"Unique participants before filtering: {len(results['participant_id'].unique())}")

results = results[results['participant_id'].isin(completed_ids)].copy()

print(f"Total rows after filtering: {len(results)}")
print(f"Unique participants after filtering: {len(results['participant_id'].unique())}")
print(f"Expected completed participants: {len(completed_ids)}")

# Check if any completed IDs are missing from data
missing_ids = set(completed_ids) - set(results['participant_id'].unique())
if missing_ids:
    print(f"\n⚠ Warning: {len(missing_ids)} completed IDs not found in data:")
    for mid in missing_ids:
        print(f"  - {mid}")

# 1. Check completion status for each participant
print("=" * 80)
print("COMPLETION STATUS")
print("=" * 80)

# Get unique participants
all_participants = results['participant_id'].unique()
print(f"\nTotal unique participants: {len(all_participants)}")

# Check which participants reached the Final block
completed_participants = results[results['block'] == 'Final']['participant_id'].unique()
print(f"Participants who completed (reached Final block): {len(completed_participants)}")

# Find participants who didn't complete
incomplete_participants = set(all_participants) - set(completed_participants)
if len(incomplete_participants) > 0:
    print(f"\nIncomplete participants ({len(incomplete_participants)}):")
    for pid in incomplete_participants:
        blocks = results[results['participant_id'] == pid]['block'].unique()
        last_block = results[results['participant_id'] == pid]['block'].iloc[-1]
        print(f"  - {pid}: Last block = {last_block}, Blocks seen = {list(blocks)}")
else:
    print("\n✓ All participants completed the experiment!")

# 2. Calculate accuracy and log-scaled RT for each participant
print("\n" + "=" * 80)
print("PARTICIPANT PERFORMANCE (Trials Block Only)")
print("=" * 80)

# Filter to Trials block only
trials_data = results[results['block'] == 'Trials'].copy()

# Deduplicate: ensure only one response per participant per trial
# Keep the most recent response (closest to today) if duplicates exist
print("\n" + "=" * 80)
print("DEDUPLICATING TRIAL RESPONSES")
print("=" * 80)

print(f"\nTotal trial responses before deduplication: {len(trials_data)}")

# Convert timestamp to datetime for sorting
trials_data['timestamp_dt'] = pd.to_datetime(trials_data['timestamp_iso'])

# Sort by timestamp descending (most recent first) and drop duplicates
trials_data = trials_data.sort_values('timestamp_dt', ascending=False)
trials_data = trials_data.drop_duplicates(subset=['participant_id', 'trial_id'], keep='first')

print(f"Total trial responses after deduplication: {len(trials_data)}")

# Check the distribution of responses per trial
responses_per_trial = trials_data.groupby('trial_id').size()
print(f"\nResponses per trial after deduplication:")
print(f"  Min: {responses_per_trial.min()}")
print(f"  Max: {responses_per_trial.max()}")
print(f"  Mean: {responses_per_trial.mean():.2f}")

if responses_per_trial.max() > 20:
    print(f"\n⚠ Warning: Some trials still have > 20 responses. Max is {responses_per_trial.max()}")
else:
    print(f"\n✓ All trials have ≤ 20 responses")

# Calculate completion time for each participant
print("\n" + "=" * 80)
print("PARTICIPANT COMPLETION TIMES (Trials Block)")
print("=" * 80)

completion_times = []
for pid in completed_ids:
    participant_trials = trials_data[trials_data['participant_id'] == pid].copy()
    
    if len(participant_trials) > 0:
        # Sort by timestamp to get first and last trial
        participant_trials = participant_trials.sort_values('timestamp_dt')
        
        first_trial_time = participant_trials['timestamp_dt'].iloc[0]
        last_trial_time = participant_trials['timestamp_dt'].iloc[-1]
        
        # Calculate duration
        duration = last_trial_time - first_trial_time
        duration_minutes = duration.total_seconds() / 60
        
        completion_times.append({
            'participant_id': pid,
            'unique_trials': len(participant_trials),
            'first_trial': first_trial_time,
            'last_trial': last_trial_time,
            'duration_minutes': duration_minutes
        })

completion_times_df = pd.DataFrame(completion_times).sort_values('duration_minutes', ascending=False)

print("\nCompletion times per participant:")
print(completion_times_df[['participant_id', 'unique_trials', 'duration_minutes']].to_string(index=False))

print(f"\nSummary:")
print(f"  Mean completion time: {completion_times_df['duration_minutes'].mean():.2f} minutes")
print(f"  Median completion time: {completion_times_df['duration_minutes'].median():.2f} minutes")
print(f"  Min completion time: {completion_times_df['duration_minutes'].min():.2f} minutes")
print(f"  Max completion time: {completion_times_df['duration_minutes'].max():.2f} minutes")

# Calculate accuracy
participant_stats = trials_data.groupby('participant_id').agg(
    total_trials=('trial_index', 'count'),
    correct_trials=('is_correct', lambda x: (x == 'yes').sum()),
    mean_rt_ms=('reaction_time_ms', 'mean'),
    median_rt_ms=('reaction_time_ms', 'median')
).reset_index()

participant_stats['accuracy'] = participant_stats['correct_trials'] / participant_stats['total_trials']

# Calculate log-scaled RT (only for valid RTs > 0)
trials_data['log_rt'] = np.log(trials_data['reaction_time_ms'].replace(0, np.nan))
log_rt_stats = trials_data.groupby('participant_id')['log_rt'].agg(['mean', 'median']).reset_index()
log_rt_stats.columns = ['participant_id', 'mean_log_rt', 'median_log_rt']

participant_stats = participant_stats.merge(log_rt_stats, on='participant_id', how='left')

print("\nParticipant Statistics:")
print(participant_stats.to_string(index=False))

print("\n" + "=" * 80)
print("SUMMARY STATISTICS")
print("=" * 80)
print(f"\nOverall Accuracy: {participant_stats['accuracy'].mean():.3f} (SD = {participant_stats['accuracy'].std():.3f})")
print(f"Overall Mean RT: {participant_stats['mean_rt_ms'].mean():.1f} ms (SD = {participant_stats['mean_rt_ms'].std():.1f})")
print(f"Overall Mean Log RT: {participant_stats['mean_log_rt'].mean():.3f} (SD = {participant_stats['mean_log_rt'].std():.3f})")

# 2b. Check for duplicated and missing trials per participant
print("\n" + "=" * 80)
print("TRIAL COMPLETION CHECK (Expected: 282 unique trials per participant)")
print("=" * 80)

trial_check = []
expected_trials = set(range(1, 283))  # 1 to 282

for pid in completed_ids:
    participant_trials = trials_data[trials_data['participant_id'] == pid]
    
    # Get all trial_ids (including duplicates)
    all_trial_ids = participant_trials['trial_id'].tolist()
    unique_trial_ids = set(all_trial_ids)
    
    # Count occurrences of each trial
    trial_counts = participant_trials['trial_id'].value_counts()
    duplicated_trials = trial_counts[trial_counts > 1].index.tolist()
    
    # Find missing trials
    missing_trials = sorted(list(expected_trials - unique_trial_ids))
    
    trial_check.append({
        'participant_id': pid,
        'total_trial_rows': len(participant_trials),
        'unique_trials': len(unique_trial_ids),
        'expected_trials': 282,
        'duplicated_count': len(duplicated_trials),
        'missing_count': len(missing_trials),
        'duplicated_trials': str(duplicated_trials) if duplicated_trials else 'None',
        'missing_trials': str(missing_trials[:10]) if missing_trials else 'None'  # Show first 10
    })

trial_check_df = pd.DataFrame(trial_check)

print("\nTrial Completion Summary:")
print(trial_check_df.to_string(index=False))

# Check if any participants have issues
issues = trial_check_df[(trial_check_df['unique_trials'] != 282) | (trial_check_df['duplicated_count'] > 0)]
if len(issues) > 0:
    print(f"\n⚠ {len(issues)} participant(s) have trial issues:")
    for _, row in issues.iterrows():
        print(f"\n  {row['participant_id']}:")
        print(f"    - Unique trials: {row['unique_trials']}/282")
        if row['duplicated_count'] > 0:
            print(f"    - Duplicated trials ({row['duplicated_count']}): {row['duplicated_trials']}")
        if row['missing_count'] > 0:
            print(f"    - Missing trials ({row['missing_count']}): {row['missing_trials']}")
else:
    print("\n✓ All participants completed exactly 282 unique trials!")

# 3. Merge results with trials metadata
print("\n" + "=" * 80)
print("MERGING DATA WITH TRIAL METADATA")
print("=" * 80)

# Check column names in trials_meta
print(f"\nColumns in trials.csv: {list(trials_meta.columns)}")
print(f"Columns in results: {list(results.columns)}")

# The trials.csv has a 'Trials' column that matches 'trial_id' in results
# Rename for clarity
if 'Trial' in trials_meta.columns:
    trials_meta = trials_meta.rename(columns={'Trial': 'trial_id'}, inplace=False)
    trials_meta_copy = trials_meta.copy()
    print("\n✓ Renamed 'Trial' column to 'trial_id' for merging")
else:
    trials_meta_copy = trials_meta.copy()

# Merge trials data with metadata
trials_with_meta = trials_data.merge(
    trials_meta_copy, 
    on='trial_id', 
    how='left'
    # suffixes=('', '_meta')
)

print(f"\nOriginal trials data shape: {trials_data.shape}")
print(f"After merging with metadata: {trials_with_meta.shape}")
print(f"\nSample of merged data (first 3 rows):")
print(trials_with_meta.head(3).to_string())

# Check for unmatched trials
unmatched = trials_with_meta[trials_with_meta.isnull().any(axis=1)]
if len(unmatched) > 0:
    print(f"\n⚠ Warning: {len(unmatched)} trials could not be matched with metadata")
else:
    print("\n✓ All trials successfully matched with metadata")

# 4. Create useful dataframes for analysis
print("\n" + "=" * 80)
print("CREATING ANALYSIS-READY DATAFRAMES")
print("=" * 80)

# Separate by block for easy access
trials_df = results[results['block'] == 'Trials'].copy()
survey_df = results[results['block'] == 'Survey'].copy()
demographics_df = results[results['block'].str.contains('Demographics', na=False)].copy()
attention_df = results[results['block'] == 'AttentionCheck'].copy()
final_df = results[results['block'] == 'Final'].copy()

print(f"\ntrials_df: {trials_df.shape[0]} rows (trial responses)")
print(f"survey_df: {survey_df.shape[0]} rows (survey responses)")
print(f"demographics_df: {demographics_df.shape[0]} rows (demographic responses)")
print(f"attention_df: {attention_df.shape[0]} rows (attention checks)")
print(f"final_df: {final_df.shape[0]} rows (completion records)")

# Add log RT to trials_df
trials_df['log_rt'] = np.log(trials_df['reaction_time_ms'].replace(0, np.nan))

print("\n" + "=" * 80)
print("READY FOR ANALYSIS!")
print("=" * 80)
print("\nAvailable DataFrames:")
print("  - results: Full raw data (filtered to completed participants)")
print("  - participant_stats: Summary statistics per participant")
print("  - trial_check_df: Trial completion check (duplicates/missing)")
print("  - trials_df: All trial responses (with log_rt column)")
print("  - trials_with_meta: Trial responses merged with trial metadata")
print("  - survey_df: Survey responses")
print("  - demographics_df: Demographics responses")
print("  - attention_df: Attention check responses")
print("  - final_df: Final/completion records")
print("  - trials_meta: Trial metadata from trials.csv")

# Save dataframes to CSV for manual inspection
print("\n" + "=" * 80)
print("SAVING DATAFRAMES TO CSV")
print("=" * 80)

demographics_df.to_csv('demographics_df.csv', index=False)
trials_meta.to_csv('trials_meta.csv', index=False)
survey_df.to_csv('survey_df.csv', index=False)

print("\n✓ Saved the following files:")
print("  - demographics_df.csv")
print("  - trials_meta.csv")
print("  - survey_df.csv")

# Z-score survey ratings and calculate averages by trial_id
print("\n" + "=" * 80)
print("SURVEY RATINGS ANALYSIS (Z-SCORED)")
print("=" * 80)

# Columns to z-score
rating_columns = ['recognize', 'understand', 'meaning_match', 'natural']

# Create a copy of survey_df for analysis
survey_analysis = survey_df.copy()

# Z-score each rating column
from scipy import stats
for col in rating_columns:
    # Only z-score non-null values
    valid_values = survey_analysis[col].dropna()
    if len(valid_values) > 0:
        survey_analysis[f'{col}_zscore'] = stats.zscore(survey_analysis[col], nan_policy='omit')
        print(f"\n✓ Z-scored {col}: mean={survey_analysis[f'{col}_zscore'].mean():.3f}, std={survey_analysis[f'{col}_zscore'].std():.3f}")

# Calculate average z-scores grouped by trial_id
zscore_columns = [f'{col}_zscore' for col in rating_columns]
trial_ratings = survey_analysis.groupby('trial_id')[zscore_columns].mean().reset_index()

# Rename columns for clarity
trial_ratings.columns = ['trial_id'] + [col.replace('_zscore', '_zscore_mean') for col in zscore_columns]

print("\n" + "=" * 80)
print("AVERAGE Z-SCORED RATINGS BY TRIAL")
print("=" * 80)
print(f"\nTotal unique trials in survey: {len(trial_ratings)}")
print("\nFirst 10 trials:")
print(trial_ratings.head(10).to_string(index=False))

# Save to CSV
trial_ratings.to_csv('trial_ratings_zscored.csv', index=False)
print("\n✓ Saved trial_ratings_zscored.csv")

# Calculate accuracy and reaction time for ALL trials first
print("\n" + "=" * 80)
print("CALCULATING ACCURACY AND REACTION TIME FOR ALL TRIALS")
print("=" * 80)

# Calculate mean accuracy per trial
accuracy_by_trial = trials_data.groupby('trial_id').agg(
    mean_accuracy=('is_correct', lambda x: (x == 'yes').mean()),
    n_responses=('is_correct', 'count')
).reset_index()

# For reaction time: log transform, then z-score, then average
# First, log transform RT (excluding 0 or negative values)
trials_data_rt = trials_data.copy()
trials_data_rt['log_rt'] = np.log(trials_data_rt['reaction_time_ms'].replace(0, np.nan))

# Z-score the log RT across all responses
trials_data_rt['log_rt_zscore'] = stats.zscore(trials_data_rt['log_rt'], nan_policy='omit')

# Calculate mean z-scored log RT per trial
rt_by_trial = trials_data_rt.groupby('trial_id').agg(
    mean_log_rt_zscore=('log_rt_zscore', 'mean'),
    mean_rt_ms=('reaction_time_ms', 'mean')
).reset_index()

# Merge accuracy and RT stats
trial_performance = accuracy_by_trial.merge(rt_by_trial, on='trial_id', how='left')

print(f"\nCalculated performance stats for {len(trial_performance)} trials")
print(f"\nOverall performance statistics:")
print(f"  Mean accuracy: {trial_performance['mean_accuracy'].mean():.3f} (SD = {trial_performance['mean_accuracy'].std():.3f})")
print(f"  Mean z-scored log RT: {trial_performance['mean_log_rt_zscore'].mean():.3f} (SD = {trial_performance['mean_log_rt_zscore'].std():.3f})")
print(f"  Mean RT (ms): {trial_performance['mean_rt_ms'].mean():.1f} (SD = {trial_performance['mean_rt_ms'].std():.1f})")

# Merge trial ratings with metadata and performance stats, then select top 54 trials
print("\n" + "=" * 80)
print("SELECTING TOP 54 TRIALS (6 PAIRS × 9 TRIALS)")
print("=" * 80)

# Merge trial ratings with metadata
trial_ratings_with_meta = trial_ratings.merge(trials_meta_copy, on='trial_id', how='left')

# Merge with performance stats
trial_ratings_with_meta = trial_ratings_with_meta.merge(trial_performance, on='trial_id', how='left')

print(f"\nTotal trials before filtering: {len(trial_ratings_with_meta)}")
print(f"Columns in merged data: {list(trial_ratings_with_meta.columns)}")

# Calculate composite score (average of the 4 z-scored ratings)
zscore_mean_cols = ['recognize_zscore_mean', 'understand_zscore_mean', 
                     'meaning_match_zscore_mean', 'natural_zscore_mean']
trial_ratings_with_meta['composite_score'] = trial_ratings_with_meta[zscore_mean_cols].mean(axis=1)

# Filter out t-none and k-none pairs
filtered_trials = trial_ratings_with_meta[
    ~trial_ratings_with_meta['Pair'].isin(['t-none', 'k-none'])
].copy()

print(f"Trials after excluding t-none and k-none: {len(filtered_trials)}")

# Filter to only include trials with accuracy < 100% (exclude perfect accuracy trials)
filtered_trials = filtered_trials[filtered_trials['mean_accuracy'] < 1.0].copy()

print(f"Trials after filtering to accuracy < 100%: {len(filtered_trials)}")
print(f"  Accuracy range: {filtered_trials['mean_accuracy'].min():.3f} to {filtered_trials['mean_accuracy'].max():.3f}")

print(f"\nUnique Pair values: {sorted(filtered_trials['Pair'].unique())}")
print(f"Total unique Pairs: {filtered_trials['Pair'].nunique()}")

# Count trials per Pair
pair_counts = filtered_trials.groupby('Pair').size().sort_values(ascending=False)
print("\nTrials per Pair:")
print(pair_counts.to_string())

# Select top 9 trials from each of the top 6 pairs by composite score
selected_trials = []
pair_summary = []

for pair in filtered_trials['Pair'].unique():
    pair_trials = filtered_trials[filtered_trials['Pair'] == pair].copy()
    pair_trials = pair_trials.sort_values('composite_score', ascending=False)
    top_9 = pair_trials.head(9)
    
    pair_summary.append({
        'Pair': pair,
        'total_trials': len(pair_trials),
        'selected_trials': len(top_9),
        'mean_composite_score': top_9['composite_score'].mean(),
        'min_composite_score': top_9['composite_score'].min(),
        'max_composite_score': top_9['composite_score'].max()
    })
    
    selected_trials.append(top_9)

# Combine all selected trials
top_54_trials = pd.concat(selected_trials, ignore_index=True)

# Sort pairs by their mean composite score to get top 6
pair_summary_df = pd.DataFrame(pair_summary).sort_values('mean_composite_score', ascending=False)

print("\n" + "=" * 80)
print("PAIR SUMMARY (sorted by mean composite score)")
print("=" * 80)
print(pair_summary_df.to_string(index=False))

# Select only the top 6 pairs
top_6_pairs = pair_summary_df.head(6)['Pair'].tolist()
final_54_trials = top_54_trials[top_54_trials['Pair'].isin(top_6_pairs)].copy()

print("\n" + "=" * 80)
print("FINAL SELECTION: TOP 54 TRIALS")
print("=" * 80)
print(f"\nTotal trials selected: {len(final_54_trials)}")
print(f"Pairs included: {top_6_pairs}")
print(f"\nTrials per pair in final selection:")
print(final_54_trials.groupby('Pair').size().to_string())

print(f"\nComposite score statistics:")
print(f"  Mean: {final_54_trials['composite_score'].mean():.3f}")
print(f"  Min: {final_54_trials['composite_score'].min():.3f}")
print(f"  Max: {final_54_trials['composite_score'].max():.3f}")

# Save the final selection
final_54_trials_sorted = final_54_trials.sort_values(['Pair', 'composite_score'], ascending=[True, False])
final_54_trials_sorted.to_csv('top_54_trials.csv', index=False)
print("\n✓ Saved top_54_trials.csv")

print("\nFirst 5 trials from selection:")
print(final_54_trials_sorted[['trial_id', 'Pair', 'composite_score', 'mean_accuracy', 'mean_log_rt_zscore'] + zscore_mean_cols].head().to_string(index=False))

# Save the complete dataframe
final_54_trials_sorted.to_csv('top_54_trials_complete.csv', index=False)
print("\n✓ Saved top_54_trials_complete.csv (includes accuracy and RT stats)")

print("\n" + "=" * 80)
print("FINAL 54 TRIALS SUMMARY")
print("=" * 80)
print(f"\nPerformance statistics for selected 54 trials:")
print(f"  Mean accuracy: {final_54_trials_sorted['mean_accuracy'].mean():.3f} (SD = {final_54_trials_sorted['mean_accuracy'].std():.3f})")
print(f"  Accuracy range: {final_54_trials_sorted['mean_accuracy'].min():.3f} to {final_54_trials_sorted['mean_accuracy'].max():.3f}")
print(f"  Mean z-scored log RT: {final_54_trials_sorted['mean_log_rt_zscore'].mean():.3f} (SD = {final_54_trials_sorted['mean_log_rt_zscore'].std():.3f})")
print(f"  Mean RT (ms): {final_54_trials_sorted['mean_rt_ms'].mean():.1f} (SD = {final_54_trials_sorted['mean_rt_ms'].std():.1f})")
print(f"  Mean composite score: {final_54_trials_sorted['composite_score'].mean():.3f} (SD = {final_54_trials_sorted['composite_score'].std():.3f})")
