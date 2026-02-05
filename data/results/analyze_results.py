import pandas as pd
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
import seaborn as sns

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

# DIAGNOSTIC: Track q-k creaky trials through filtering
print("\n" + "=" * 80)
print("DIAGNOSTIC: Tracking q-k creaky trials through filtering")
print("=" * 80)

# Merge with metadata to identify q-k creaky trials
trials_data_with_meta = trials_data.merge(trials_meta_copy, on='trial_id', how='left')
qk_creaky_mask = (trials_data_with_meta['Pair'] == 'q-k') & (trials_data_with_meta['CarrierType'] == 'creaky')
qk_creaky_initial = trials_data_with_meta[qk_creaky_mask]['trial_id'].unique()
print(f"\nUnique q-k creaky trial IDs after deduplication: {len(qk_creaky_initial)}")
print(f"Trial IDs: {sorted(qk_creaky_initial)}")

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

# Remove outliers based on accuracy and reaction time
print("\n" + "=" * 80)
print("REMOVING OUTLIERS (2 SD threshold)")
print("=" * 80)

print(f"\nTotal responses before outlier removal: {len(trials_data)}")

# Step 1: Remove trials with extremely long reaction times (> 1000ms)
long_rt_trials = trials_data['reaction_time_ms'] > 1000
print(f"\nRemoving trials with RT > 1000ms: {long_rt_trials.sum()} trials")
trials_data = trials_data[~long_rt_trials].copy()
print(f"Total responses after removing long RTs: {len(trials_data)}")

# DIAGNOSTIC: Check q-k creaky trials after RT filter
trials_data_with_meta = trials_data.merge(trials_meta_copy, on='trial_id', how='left')
qk_creaky_mask = (trials_data_with_meta['Pair'] == 'q-k') & (trials_data_with_meta['CarrierType'] == 'creaky')
qk_creaky_after_rt = trials_data_with_meta[qk_creaky_mask]['trial_id'].unique()
print(f"q-k creaky trials after RT filter: {len(qk_creaky_after_rt)}")
if len(qk_creaky_initial) != len(qk_creaky_after_rt):
    removed = set(qk_creaky_initial) - set(qk_creaky_after_rt)
    print(f"  ⚠ Removed {len(removed)} q-k creaky trials: {sorted(removed)}")

# Calculate accuracy per trial (proportion of correct responses)
trial_accuracy = trials_data.groupby('trial_id')['is_correct'].apply(
    lambda x: (x == 'yes').mean()
).reset_index()
trial_accuracy.columns = ['trial_id', 'trial_accuracy']

# Merge trial accuracy back to trials_data
trials_data = trials_data.merge(trial_accuracy, on='trial_id', how='left')

# Calculate mean and SD for trial accuracy
accuracy_mean = trials_data['trial_accuracy'].mean()
accuracy_sd = trials_data['trial_accuracy'].std()

print(f"\nTrial Accuracy Stats:")
print(f"  Mean: {accuracy_mean:.3f}")
print(f"  SD: {accuracy_sd:.3f}")
print(f"  2 SD range: [{accuracy_mean - 2*accuracy_sd:.3f}, {accuracy_mean + 2*accuracy_sd:.3f}]")

# Calculate log RT and then z-score it
trials_data['log_rt_temp'] = np.log(trials_data['reaction_time_ms'].replace(0, np.nan))
trials_data['log_rt_zscore_temp'] = stats.zscore(trials_data['log_rt_temp'], nan_policy='omit')

# Calculate mean and SD for log z-scored RT
log_rt_zscore_mean = trials_data['log_rt_zscore_temp'].mean()
log_rt_zscore_sd = trials_data['log_rt_zscore_temp'].std()

print(f"\nLog Z-scored RT Stats:")
print(f"  Mean: {log_rt_zscore_mean:.3f}")
print(f"  SD: {log_rt_zscore_sd:.3f}")
print(f"  2 SD range: [{log_rt_zscore_mean - 2*log_rt_zscore_sd:.3f}, {log_rt_zscore_mean + 2*log_rt_zscore_sd:.3f}]")

# Identify outliers (responses where either metric is more than 2 SD from mean)
accuracy_outliers = np.abs(trials_data['trial_accuracy'] - accuracy_mean) > 2 * accuracy_sd
rt_outliers = np.abs(trials_data['log_rt_zscore_temp'] - log_rt_zscore_mean) > 2 * log_rt_zscore_sd

# Combine outlier flags
outliers = accuracy_outliers | rt_outliers

print(f"\nOutliers identified:")
print(f"  Accuracy outliers: {accuracy_outliers.sum()}")
print(f"  RT outliers: {rt_outliers.sum()}")
print(f"  Total outliers (combined): {outliers.sum()}")

# Remove outliers
trials_data = trials_data[~outliers].copy()

# Drop temporary columns
trials_data = trials_data.drop(columns=['trial_accuracy', 'log_rt_temp', 'log_rt_zscore_temp'])

print(f"\nTotal responses after trial-level outlier removal: {len(trials_data)}")
print(f"Trial-level responses removed: {outliers.sum()}")

# DIAGNOSTIC: Check q-k creaky trials after trial-level outlier removal
trials_data_with_meta = trials_data.merge(trials_meta_copy, on='trial_id', how='left')
qk_creaky_mask = (trials_data_with_meta['Pair'] == 'q-k') & (trials_data_with_meta['CarrierType'] == 'creaky')
qk_creaky_after_trial_outliers = trials_data_with_meta[qk_creaky_mask]['trial_id'].unique()
print(f"q-k creaky trials after trial-level outlier removal: {len(qk_creaky_after_trial_outliers)}")
if len(qk_creaky_after_rt) != len(qk_creaky_after_trial_outliers):
    removed = set(qk_creaky_after_rt) - set(qk_creaky_after_trial_outliers)
    print(f"  ⚠ Removed {len(removed)} q-k creaky trials: {sorted(removed)}")

# Step 2: Remove participants with extreme average log z-scored RT
print("\n" + "-" * 80)
print("Removing participants with extreme average log z-scored RT")
print("-" * 80)

# Calculate log RT and z-score it for participant-level filtering
trials_data['log_rt_temp2'] = np.log(trials_data['reaction_time_ms'].replace(0, np.nan))
trials_data['log_rt_zscore_temp2'] = stats.zscore(trials_data['log_rt_temp2'], nan_policy='omit')

# Calculate average log z-scored RT per participant
participant_avg_rt = trials_data.groupby('participant_id')['log_rt_zscore_temp2'].mean().reset_index()
participant_avg_rt.columns = ['participant_id', 'avg_log_rt_zscore']

# Calculate mean and SD of participant averages
avg_mean = participant_avg_rt['avg_log_rt_zscore'].mean()
avg_sd = participant_avg_rt['avg_log_rt_zscore'].std()

print(f"\nParticipant average log z-scored RT stats:")
print(f"  Mean: {avg_mean:.3f}")
print(f"  SD: {avg_sd:.3f}")
print(f"  2 SD range: [{avg_mean - 2*avg_sd:.3f}, {avg_mean + 2*avg_sd:.3f}]")

# Identify participants with extreme average RT
extreme_rt_participants = participant_avg_rt[
    np.abs(participant_avg_rt['avg_log_rt_zscore'] - avg_mean) > 2 * avg_sd
]['participant_id'].tolist()

if len(extreme_rt_participants) > 0:
    print(f"\nRemoving {len(extreme_rt_participants)} participant(s) with extreme average RT:")
    for pid in extreme_rt_participants:
        avg_rt = participant_avg_rt[participant_avg_rt['participant_id'] == pid]['avg_log_rt_zscore'].values[0]
        n_trials = len(trials_data[trials_data['participant_id'] == pid])
        print(f"  - {pid}: avg z-scored log RT = {avg_rt:.3f} ({n_trials} trials)")
    
    # Remove all trials from these participants
    trials_data = trials_data[~trials_data['participant_id'].isin(extreme_rt_participants)].copy()
else:
    print("\n✓ No participants with extreme average RT (all within 2 SD)")

# Drop temporary columns
trials_data = trials_data.drop(columns=['log_rt_temp2', 'log_rt_zscore_temp2'])

print(f"\nTotal responses after participant-level outlier removal: {len(trials_data)}")
print(f"Remaining participants: {trials_data['participant_id'].nunique()}")

# DIAGNOSTIC: Check q-k creaky trials after participant-level outlier removal
trials_data_with_meta = trials_data.merge(trials_meta_copy, on='trial_id', how='left')
qk_creaky_mask = (trials_data_with_meta['Pair'] == 'q-k') & (trials_data_with_meta['CarrierType'] == 'creaky')
qk_creaky_after_participant_outliers = trials_data_with_meta[qk_creaky_mask]['trial_id'].unique()
print(f"q-k creaky trials after participant-level outlier removal: {len(qk_creaky_after_participant_outliers)}")
if len(qk_creaky_after_trial_outliers) != len(qk_creaky_after_participant_outliers):
    removed = set(qk_creaky_after_trial_outliers) - set(qk_creaky_after_participant_outliers)
    print(f"  ⚠ Removed {len(removed)} q-k creaky trials: {sorted(removed)}")

# Check responses per trial after outlier removal
responses_per_trial_clean = trials_data.groupby('trial_id').size()
print(f"\nResponses per trial after outlier removal:")
print(f"  Min: {responses_per_trial_clean.min()}")
print(f"  Max: {responses_per_trial_clean.max()}")
print(f"  Mean: {responses_per_trial_clean.mean():.2f}")

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

# Filter out trials with missing accuracy or RT data
trials_with_missing = filtered_trials[
    filtered_trials['mean_accuracy'].isna() | 
    filtered_trials['mean_log_rt_zscore'].isna()
]
if len(trials_with_missing) > 0:
    print(f"\n⚠ Warning: Removing {len(trials_with_missing)} trials with missing performance data")
    print(f"  Trial IDs with missing data: {trials_with_missing['trial_id'].tolist()}")

filtered_trials = filtered_trials[
    filtered_trials['mean_accuracy'].notna() & 
    filtered_trials['mean_log_rt_zscore'].notna()
].copy()

print(f"Trials after removing missing data: {len(filtered_trials)}")

# DIAGNOSTIC: Check q-k creaky trials after missing data filter
qk_creaky_in_filtered = filtered_trials[
    (filtered_trials['Pair'] == 'q-k') & 
    (filtered_trials['CarrierType'] == 'creaky')
]['trial_id'].unique()
print(f"\nq-k creaky trials after missing data filter: {len(qk_creaky_in_filtered)}")
print(f"Trial IDs: {sorted(qk_creaky_in_filtered)}")
if len(qk_creaky_after_participant_outliers) != len(qk_creaky_in_filtered):
    removed = set(qk_creaky_after_participant_outliers) - set(qk_creaky_in_filtered)
    print(f"  ⚠ Removed {len(removed)} q-k creaky trials due to missing data: {sorted(removed)}")

# ============================================================================
# DIAGNOSTIC: Check for trials with all NaN log RTs
# ============================================================================
print("\n" + "=" * 80)
print("CHECKING FOR TRIALS WITH ALL NaN LOG RTs")
print("=" * 80)

all_trial_ids = trial_ratings_with_meta['trial_id'].unique()
trials_with_all_nan_rts = []

for trial_id in all_trial_ids:
    trial_log_rts = trials_data_rt[trials_data_rt['trial_id'] == trial_id]['log_rt']
    valid_log_rts = trial_log_rts.notna().sum()
    
    if valid_log_rts == 0:
        trials_with_all_nan_rts.append(trial_id)

if len(trials_with_all_nan_rts) > 0:
    print(f"\n⚠ Found {len(trials_with_all_nan_rts)} trial(s) with ALL NaN log RTs:")
    for trial_id in trials_with_all_nan_rts:
        pair = trial_ratings_with_meta[trial_ratings_with_meta['trial_id'] == trial_id]['Pair'].values[0] if len(trial_ratings_with_meta[trial_ratings_with_meta['trial_id'] == trial_id]) > 0 else 'Unknown'
        print(f"  - Trial {trial_id} ({pair})")
    print(f"\nThese trials will be filtered out due to NaN mean_log_rt_zscore.")
else:
    print(f"\n✓ No trials found with ALL NaN log RTs - all trials have at least one valid RT response.")

print(f"\nUnique Pair values: {sorted(filtered_trials['Pair'].unique())}")
print(f"Total unique Pairs: {filtered_trials['Pair'].nunique()}")

# Count trials per Pair
pair_counts = filtered_trials.groupby('Pair').size().sort_values(ascending=False)
print("\nTrials per Pair:")
print(pair_counts.to_string())

# NEW STRATEGY: Categorize trials into three types based on accuracy and RT
print("\n" + "=" * 80)
print("CATEGORIZING TRIALS BY DIFFICULTY")
print("=" * 80)

# Calculate median RT z-score for splitting high/low RT
median_rt_zscore = filtered_trials['mean_log_rt_zscore'].median()
print(f"\nMedian log RT z-score: {median_rt_zscore:.3f}")

# Categorize trials
def categorize_trial(row):
    if row['mean_accuracy'] < 0.95:
        return 'Ambiguous'
    elif row['mean_log_rt_zscore'] > median_rt_zscore:
        return 'Accurate-Slow'
    else:
        return 'Accurate-Fast'

filtered_trials['trial_type'] = filtered_trials.apply(categorize_trial, axis=1)

# Show distribution of trial types
print("\nTrial type distribution:")
type_counts = filtered_trials['trial_type'].value_counts()
print(type_counts.to_string())

print("\nTrial type distribution by Pair:")
type_by_pair = filtered_trials.groupby(['Pair', 'trial_type']).size().unstack(fill_value=0)
print(type_by_pair.to_string())

# First, identify the top 6 pairs by overall composite score
pair_avg_scores = filtered_trials.groupby('Pair')['composite_score'].mean().sort_values(ascending=False)
top_6_pairs = pair_avg_scores.head(6).index.tolist()

print("\n" + "=" * 80)
print("TOP 6 PAIRS BY COMPOSITE SCORE")
print("=" * 80)
print(f"\nSelected pairs: {top_6_pairs}")
for pair in top_6_pairs:
    print(f"  {pair}: mean composite = {pair_avg_scores[pair]:.3f}")

# Filter to only top 6 pairs
top_6_trials = filtered_trials[filtered_trials['Pair'].isin(top_6_pairs)].copy()

print("\n" + "=" * 80)
print("SELECTING 3 TRIALS OF EACH TYPE PER PAIR (9 × 6 = 54 TRIALS)")
print("=" * 80)

# Hard-coded trials for q-k and gs-k pairs based on naturalness scores
hardcoded_trials = [28, 29, 34, 35, 36, 37, 38, 39, 42, 43, 14, 15]
print(f"\nHard-coded trials for q-k and gs-k: {hardcoded_trials}")

# Select 3 trials of each type from each of the 6 pairs
# Track used UniquePairIndex values to ensure uniqueness
selected_trials = []
selection_summary = []
used_unique_pair_indices = set()

for pair in top_6_pairs:
    pair_trials = top_6_trials[top_6_trials['Pair'] == pair].copy()
    
    pair_selected = []
    type_counts = {}
    
    # For q-k and gs-k, prioritize hard-coded trials
    if pair in ['q-k', 'gs-k']:
        # First, select hard-coded trials for this pair
        hardcoded_for_pair = pair_trials[pair_trials['trial_id'].isin(hardcoded_trials)].copy()
        hardcoded_for_pair = hardcoded_for_pair.sort_values('composite_score', ascending=False)
        
        print(f"\n{pair}: Found {len(hardcoded_for_pair)} hard-coded trials")
        
        # Add hard-coded trials first
        hardcoded_selected = []
        for _, trial in hardcoded_for_pair.iterrows():
            if trial['UniquePairIndex'] not in used_unique_pair_indices:
                hardcoded_selected.append(trial)
                used_unique_pair_indices.add(trial['UniquePairIndex'])
        
        if len(hardcoded_selected) > 0:
            hardcoded_df = pd.DataFrame(hardcoded_selected)
            pair_selected.append(hardcoded_df)
            print(f"  Selected {len(hardcoded_selected)} hard-coded trials")
        
        # Calculate how many more trials we need (target is 9 per pair)
        needed = 9 - len(hardcoded_selected)
        print(f"  Need {needed} more trials to reach 9 total")
        
        # Fill remaining with best trials from any type
        if needed > 0:
            remaining_trials = pair_trials[~pair_trials['trial_id'].isin(hardcoded_trials)].copy()
            remaining_trials = remaining_trials.sort_values('composite_score', ascending=False)
            
            additional_selected = []
            for _, trial in remaining_trials.iterrows():
                if trial['UniquePairIndex'] not in used_unique_pair_indices:
                    additional_selected.append(trial)
                    used_unique_pair_indices.add(trial['UniquePairIndex'])
                    
                    if len(additional_selected) >= needed:
                        break
            
            if len(additional_selected) > 0:
                additional_df = pd.DataFrame(additional_selected)
                pair_selected.append(additional_df)
                print(f"  Filled {len(additional_selected)} additional trials")
        
        # Count by type for summary
        if pair_selected:
            all_selected = pd.concat(pair_selected, ignore_index=True)
            type_counts = all_selected['trial_type'].value_counts().to_dict()
        
        selection_summary.append({
            'Pair': pair,
            'Ambiguous': type_counts.get('Ambiguous', 0),
            'Accurate_Slow': type_counts.get('Accurate-Slow', 0),
            'Accurate_Fast': type_counts.get('Accurate-Fast', 0),
            'Total': sum(type_counts.values())
        })
        
        if pair_selected:
            selected_trials.extend(pair_selected)
        
        continue  # Skip the normal selection logic for q-k and gs-k
    
    # Normal selection logic for other pairs
    # Filter to only include trials with natural z-score >= (mean - 2 SD)
    natural_mean = pair_trials['natural_zscore_mean'].mean()
    natural_sd = pair_trials['natural_zscore_mean'].std()
    natural_threshold = natural_mean - (2 * natural_sd)
    pair_trials = pair_trials[pair_trials['natural_zscore_mean'] >= natural_threshold].copy()
    
    if len(pair_trials) == 0:
        print(f"\n⚠ Warning: {pair} has no trials with positive natural z-score")
        selection_summary.append({
            'Pair': pair,
            'Ambiguous': 0,
            'Accurate_Slow': 0,
            'Accurate_Fast': 0,
            'Total': 0
        })
        continue
    
    for trial_type in ['Ambiguous', 'Accurate-Slow', 'Accurate-Fast']:
        type_trials = pair_trials[pair_trials['trial_type'] == trial_type].copy()
        type_trials = type_trials.sort_values('composite_score', ascending=False)
        
        # Select top 3 trials, checking for unique UniquePairIndex
        selected_list = []
        for _, trial in type_trials.iterrows():
            if trial['UniquePairIndex'] not in used_unique_pair_indices:
                selected_list.append(trial)
                used_unique_pair_indices.add(trial['UniquePairIndex'])
                
                if len(selected_list) >= 3:
                    break
        
        if len(selected_list) > 0:
            selected = pd.DataFrame(selected_list)
            pair_selected.append(selected)
        type_counts[trial_type] = len(selected_list)
        
        if len(selected_list) < 3:
            print(f"\n⚠ Warning: {pair} has only {len(selected_list)} unique {trial_type} trials (need 3)")
            
            # Try to fill from other trial types if this type doesn't have enough
            if len(selected_list) < 3:
                print(f"  → Attempting to fill from other trial types for {pair}")
                needed = 3 - len(selected_list)
                
                # Try other types in order of preference
                other_types = [t for t in ['Ambiguous', 'Accurate-Slow', 'Accurate-Fast'] if t != trial_type]
                for other_type in other_types:
                    if needed <= 0:
                        break
                    
                    other_trials = pair_trials[pair_trials['trial_type'] == other_type].copy()
                    other_trials = other_trials.sort_values('composite_score', ascending=False)
                    
                    for _, trial in other_trials.iterrows():
                        if trial['UniquePairIndex'] not in used_unique_pair_indices:
                            selected_list.append(trial)
                            used_unique_pair_indices.add(trial['UniquePairIndex'])
                            needed -= 1
                            
                            if needed <= 0:
                                break
                
                # Update with filled trials
                if len(selected_list) > len(pair_selected[-1]) if len(pair_selected) > 0 else 0:
                    print(f"  → Filled {3 - needed - type_counts[trial_type]} additional trials from other types")
                    if len(pair_selected) > 0:
                        pair_selected[-1] = pd.DataFrame(selected_list)
                    else:
                        pair_selected.append(pd.DataFrame(selected_list))
                    type_counts[trial_type] = len(selected_list)
    
    selection_summary.append({
        'Pair': pair,
        'Ambiguous': type_counts.get('Ambiguous', 0),
        'Accurate_Slow': type_counts.get('Accurate-Slow', 0),
        'Accurate_Fast': type_counts.get('Accurate-Fast', 0),
        'Total': sum(type_counts.values())
    })
    
    if pair_selected:
        selected_trials.extend(pair_selected)

print(f"\nTotal unique UniquePairIndex values used: {len(used_unique_pair_indices)}")

# Combine all selected trials
final_54_trials = pd.concat(selected_trials, ignore_index=True)

# Display selection summary
selection_summary_df = pd.DataFrame(selection_summary)
print("\nSelection Summary:")
print(selection_summary_df.to_string(index=False))
print(f"\nTotal trials selected: {len(final_54_trials)}")

# Show distribution of trial types in final selection
print("\nFinal trial type distribution:")
print(final_54_trials['trial_type'].value_counts().to_string())

# Sort by Pair and composite score for output
final_54_trials_sorted = final_54_trials.sort_values(['Pair', 'trial_type', 'composite_score'], 
                                                      ascending=[True, True, False])

print("\n" + "=" * 80)
print("FINAL 54 TRIALS SUMMARY")
print("=" * 80)
print(f"\nTotal trials selected: {len(final_54_trials_sorted)}")
print(f"Pairs included: {top_6_pairs}")

print(f"\nTrials per pair in final selection:")
print(final_54_trials_sorted.groupby('Pair').size().to_string())

print(f"\nComposite score statistics:")
print(f"  Mean: {final_54_trials_sorted['composite_score'].mean():.3f}")
print(f"  Min: {final_54_trials_sorted['composite_score'].min():.3f}")
print(f"  Max: {final_54_trials_sorted['composite_score'].max():.3f}")

print(f"\nPerformance statistics for selected trials:")
print(f"  Mean accuracy: {final_54_trials_sorted['mean_accuracy'].mean():.3f} (SD = {final_54_trials_sorted['mean_accuracy'].std():.3f})")
print(f"  Accuracy range: {final_54_trials_sorted['mean_accuracy'].min():.3f} to {final_54_trials_sorted['mean_accuracy'].max():.3f}")
print(f"  Mean z-scored log RT: {final_54_trials_sorted['mean_log_rt_zscore'].mean():.3f} (SD = {final_54_trials_sorted['mean_log_rt_zscore'].std():.3f})")
print(f"  Mean RT (ms): {final_54_trials_sorted['mean_rt_ms'].mean():.1f} (SD = {final_54_trials_sorted['mean_rt_ms'].std():.1f})")

# Show stats by trial type
print("\nPerformance by trial type:")
for trial_type in ['Ambiguous', 'Accurate-Slow', 'Accurate-Fast']:
    type_trials = final_54_trials_sorted[final_54_trials_sorted['trial_type'] == trial_type]
    if len(type_trials) > 0:
        print(f"\n  {trial_type} (n={len(type_trials)}):")
        print(f"    Accuracy: {type_trials['mean_accuracy'].mean():.3f} (SD = {type_trials['mean_accuracy'].std():.3f})")
        print(f"    Log RT z-score: {type_trials['mean_log_rt_zscore'].mean():.3f} (SD = {type_trials['mean_log_rt_zscore'].std():.3f})")
        print(f"    RT (ms): {type_trials['mean_rt_ms'].mean():.1f} (SD = {type_trials['mean_rt_ms'].std():.1f})")
        print(f"    Composite: {type_trials['composite_score'].mean():.3f} (SD = {type_trials['composite_score'].std():.3f})")

# Save the final selection
final_54_trials_sorted.to_csv('top_54_trials.csv', index=False)
print("\n✓ Saved top_54_trials.csv")

print("\nFirst 10 trials from selection:")
print(final_54_trials_sorted[['trial_id', 'Pair', 'trial_type', 'composite_score', 'mean_accuracy', 'mean_log_rt_zscore'] + zscore_mean_cols].head(10).to_string(index=False))

# Save the complete dataframe
final_54_trials_sorted.to_csv('top_54_trials_complete.csv', index=False)
print("\n✓ Saved top_54_trials_complete.csv (includes accuracy and RT stats)")

# Show which hard-coded trials were included
if len(final_54_trials_sorted[final_54_trials_sorted['trial_id'].isin(hardcoded_trials)]) > 0:
    print("\nHard-coded trials included in final selection:")
    hardcoded_included = final_54_trials_sorted[final_54_trials_sorted['trial_id'].isin(hardcoded_trials)]
    print(hardcoded_included[['trial_id', 'Pair', 'Position', 'CarrierType']].to_string(index=False))

# ============================================================================
# PERFORMANCE PLOTS (ACCURACY + RT) BY SEGMENT/PAIR
# ============================================================================
print("\n" + "=" * 80)
print("CREATING PERFORMANCE PLOTS (ACCURACY + RT)")
print("=" * 80)
print("\n" + "=" * 80)
print("CREATING PERFORMANCE PLOTS (ACCURACY + RT)")
print("=" * 80)

# Define colors for CarrierType
carrier_colors = {
    'non-creaky': '#D32F2F',
    'creaky': '#1976D2',
    'CV': '#7B1FA2'
}

def create_grouped_barplot(data, y_col, ylabel, title, center_at_zero=False):
    """
    Create a figure with subplots by Position, grouped by TargetSegment, split by CarrierType.
    """
    position_order = ['Initial', 'Medial', 'Final']
    positions = [p for p in position_order if p in data['Position'].unique()]
    n_positions = len(positions)

    fig, axes = plt.subplots(1, n_positions, figsize=(6 * n_positions, 6), sharey=True)
    if n_positions == 1:
        axes = [axes]

    handles_dict = {}
    all_max_extents = []  # Track max extent across all subplots

    for idx, position in enumerate(positions):
        ax = axes[idx]
        position_data = data[data['Position'] == position]

        target_segments = sorted(position_data['TargetSegment'].unique())
        carrier_types = sorted(position_data['CarrierType'].unique())

        x = np.arange(len(target_segments))
        width = 0.25

        for i, carrier in enumerate(carrier_types):
            carrier_data = position_data[position_data['CarrierType'] == carrier]

            means = []
            errors = []
            for segment in target_segments:
                segment_values = carrier_data[carrier_data['TargetSegment'] == segment][y_col]
                if len(segment_values) > 0:
                    means.append(segment_values.mean())
                    errors.append(segment_values.std() / np.sqrt(len(segment_values)))
                else:
                    means.append(0)
                    errors.append(0)

            # Track max extent including error bars
            if len(means) > 0 and len(errors) > 0:
                upper_extent = [m + e for m, e in zip(means, errors)]
                lower_extent = [m - e for m, e in zip(means, errors)]
                all_max_extents.extend(upper_extent + lower_extent)

            offset = (i - len(carrier_types)/2 + 0.5) * width
            bars = ax.bar(
                x + offset,
                means,
                width,
                label=carrier,
                color=carrier_colors.get(carrier, 'gray'),
                yerr=errors,
                capsize=5,
                alpha=0.8,
                edgecolor='black',
                linewidth=1
            )

            if carrier not in handles_dict:
                handles_dict[carrier] = bars

        ax.set_xlabel('Target Segment', fontsize=12, fontweight='bold')
        ax.set_ylabel(ylabel if idx == 0 else '', fontsize=12, fontweight='bold')
        ax.set_title(f'Position: {position}', fontsize=14, fontweight='bold')
        ax.set_xticks(x)
        ax.set_xticklabels(target_segments, fontsize=11)
        ax.grid(axis='y', alpha=0.3, linestyle='--')

    # Set consistent y-axis limits across all subplots
    if center_at_zero and len(all_max_extents) > 0:
        # Filter out NaN and Inf values
        valid_extents = [e for e in all_max_extents if np.isfinite(e)]
        if len(valid_extents) > 0:
            max_abs = max(abs(min(valid_extents)), abs(max(valid_extents)))
            max_abs *= 1.1  # Add 10% padding
            for ax in axes:
                ax.set_ylim(-max_abs, max_abs)
                ax.axhline(0, color='black', linewidth=1.5, linestyle='-', alpha=0.5)

    fig.suptitle(title, fontsize=16, fontweight='bold', y=1.02)

    carrier_types_sorted = sorted(handles_dict.keys())
    handles = [handles_dict[ct] for ct in carrier_types_sorted]
    fig.legend(
        handles,
        carrier_types_sorted,
        title='Carrier Type',
        loc='upper center',
        bbox_to_anchor=(0.5, -0.02),
        ncol=len(carrier_types_sorted),
        fontsize=11,
        frameon=True
    )

    plt.tight_layout()
    return fig

def create_grouped_barplot_by_pair(data, y_col, ylabel, title, center_at_zero=False):
    """
    Create a figure with subplots by Position, grouped by Pair, split by CarrierType.
    """
    position_order = ['Initial', 'Medial', 'Final']
    positions = [p for p in position_order if p in data['Position'].unique()]
    n_positions = len(positions)

    fig, axes = plt.subplots(1, n_positions, figsize=(8 * n_positions, 6), sharey=True)
    if n_positions == 1:
        axes = [axes]

    handles_dict = {}
    all_max_extents = []  # Track max extent across all subplots

    for idx, position in enumerate(positions):
        ax = axes[idx]
        position_data = data[data['Position'] == position]

        pairs = sorted(position_data['Pair'].unique())
        carrier_types = sorted(position_data['CarrierType'].unique())

        x = np.arange(len(pairs))
        width = 0.25

        for i, carrier in enumerate(carrier_types):
            carrier_data = position_data[position_data['CarrierType'] == carrier]

            means = []
            errors = []
            for pair in pairs:
                pair_values = carrier_data[carrier_data['Pair'] == pair][y_col]
                if len(pair_values) > 0:
                    means.append(pair_values.mean())
                    errors.append(pair_values.std() / np.sqrt(len(pair_values)))
                else:
                    means.append(0)
                    errors.append(0)

            # Track max extent including error bars
            if len(means) > 0 and len(errors) > 0:
                upper_extent = [m + e for m, e in zip(means, errors)]
                lower_extent = [m - e for m, e in zip(means, errors)]
                all_max_extents.extend(upper_extent + lower_extent)

            offset = (i - len(carrier_types)/2 + 0.5) * width
            bars = ax.bar(
                x + offset,
                means,
                width,
                label=carrier,
                color=carrier_colors.get(carrier, 'gray'),
                yerr=errors,
                capsize=5,
                alpha=0.8,
                edgecolor='black',
                linewidth=1
            )

            if carrier not in handles_dict:
                handles_dict[carrier] = bars

        ax.set_xlabel('Pair', fontsize=12, fontweight='bold')
        ax.set_ylabel(ylabel if idx == 0 else '', fontsize=12, fontweight='bold')
        ax.set_title(f'Position: {position}', fontsize=14, fontweight='bold')
        ax.set_xticks(x)
        ax.set_xticklabels(pairs, fontsize=9, rotation=45, ha='right')
        ax.grid(axis='y', alpha=0.3, linestyle='--')

    # Set consistent y-axis limits across all subplots
    if center_at_zero and len(all_max_extents) > 0:
        # Filter out NaN and Inf values
        valid_extents = [e for e in all_max_extents if np.isfinite(e)]
        if len(valid_extents) > 0:
            max_abs = max(abs(min(valid_extents)), abs(max(valid_extents)))
            max_abs *= 1.1  # Add 10% padding
            for ax in axes:
                ax.set_ylim(-max_abs, max_abs)
                ax.axhline(0, color='black', linewidth=1.5, linestyle='-', alpha=0.5)

    fig.suptitle(title, fontsize=16, fontweight='bold', y=1.02)

    carrier_types_sorted = sorted(handles_dict.keys())
    handles = [handles_dict[ct] for ct in carrier_types_sorted]
    fig.legend(
        handles,
        carrier_types_sorted,
        title='Carrier Type',
        loc='upper center',
        bbox_to_anchor=(0.5, -0.05),
        ncol=len(carrier_types_sorted),
        fontsize=11,
        frameon=True
    )

    plt.tight_layout()
    return fig

# Prepare datasets for plotting
trials_all_plot = trial_ratings_with_meta[
    trial_ratings_with_meta['mean_accuracy'].notna() &
    trial_ratings_with_meta['mean_log_rt_zscore'].notna()
].copy()

trials_top54_plot = final_54_trials_sorted.copy()

# Plot 1: Mean Accuracy (Top 54)
fig1 = create_grouped_barplot(
    trials_top54_plot,
    'mean_accuracy',
    'Mean Accuracy',
    'Mean Accuracy by Position, Target Segment, and Carrier Type (Top 54 Trials)',
    center_at_zero=False
)
fig1.savefig('plot_accuracy_top54.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_accuracy_top54.png")

# Plot 2: Mean Log RT Z-score (Top 54)
fig2 = create_grouped_barplot(
    trials_top54_plot,
    'mean_log_rt_zscore',
    'Mean Log RT (Z-score)',
    'Mean Log RT Z-score by Position, Target Segment, and Carrier Type (Top 54 Trials)',
    center_at_zero=True
)
fig2.savefig('plot_rt_zscore_top54.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_rt_zscore_top54.png")

# Plot 3: Mean Accuracy (All Trials)
fig3 = create_grouped_barplot(
    trials_all_plot,
    'mean_accuracy',
    'Mean Accuracy',
    'Mean Accuracy by Position, Target Segment, and Carrier Type (All Trials)',
    center_at_zero=False
)
fig3.savefig('plot_accuracy_all.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_accuracy_all.png")

# Plot 4: Mean Log RT Z-score (All Trials)
fig4 = create_grouped_barplot(
    trials_all_plot,
    'mean_log_rt_zscore',
    'Mean Log RT (Z-score)',
    'Mean Log RT Z-score by Position, Target Segment, and Carrier Type (All Trials)',
    center_at_zero=True
)
fig4.savefig('plot_rt_zscore_all.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_rt_zscore_all.png")

# Plot 5: Mean Accuracy (All Trials, by Pair)
fig5 = create_grouped_barplot_by_pair(
    trials_all_plot,
    'mean_accuracy',
    'Mean Accuracy',
    'Mean Accuracy by Position, Pair, and Carrier Type (All Trials)',
    center_at_zero=False
)
fig5.savefig('plot_accuracy_all_by_pair.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_accuracy_all_by_pair.png")

# Plot 6: Mean Log RT Z-score (All Trials, by Pair)
fig6 = create_grouped_barplot_by_pair(
    trials_all_plot,
    'mean_log_rt_zscore',
    'Mean Log RT (Z-score)',
    'Mean Log RT Z-score by Position, Pair, and Carrier Type (All Trials)',
    center_at_zero=True
)
fig6.savefig('plot_rt_zscore_all_by_pair.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_rt_zscore_all_by_pair.png")

# Plot 7: Natural Z-score for Top 54 Trials by Pair and CarrierType
print("\nCreating natural z-score plot for top 54 trials...")

# Create a single bar plot similar to the z-scored rating plots
fig7, ax = plt.subplots(figsize=(12, 6))

# Use seaborn barplot with Pair on x-axis and CarrierType as hue
sns.barplot(data=trials_top54_plot, x='Pair', y='natural_zscore_mean', hue='CarrierType', 
            ax=ax, palette='Set2', order=sorted(trials_top54_plot['Pair'].unique()),
            errorbar='se', capsize=0.1)

# Customize plot
ax.set_title('Z-Scored Natural Ratings by Pair Type and Carrier Type (Top 54 Trials)', 
             fontsize=14, fontweight='bold')
ax.set_xlabel('Pair Type', fontsize=12)
ax.set_ylabel('Z-Scored Natural', fontsize=12)
ax.legend(title='Carrier Type', fontsize=10)
ax.grid(axis='y', alpha=0.3)
ax.axhline(0, color='black', linewidth=1.5, linestyle='-', alpha=0.5)

# Rotate x-axis labels for better readability
plt.xticks(rotation=45, ha='right')

plt.tight_layout()

fig7.savefig('plot_natural_zscore_top54_by_pair.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_natural_zscore_top54_by_pair.png")

plt.close()

# ============================================================================
# PLOTTING RAW (NON-Z-SCORED) RATINGS BY PAIR TYPE AND CARRIER TYPE
# ============================================================================
print("\n" + "=" * 80)
print("CREATING DISTRIBUTION PLOTS FOR RAW (NON-Z-SCORED) RATINGS")
print("=" * 80)

# Merge survey_analysis with metadata to get Pair and CarrierType for each individual rating
raw_rating_columns = ['recognize', 'understand', 'meaning_match', 'natural']
survey_with_meta = survey_analysis.merge(trials_meta_copy, on='trial_id', how='left')

print(f"\nColumns available for plotting raw ratings: {list(survey_with_meta.columns)}")
print(f"Total individual survey responses: {len(survey_with_meta)}")

# Create 4 separate distribution plots for each raw rating metric
for metric in raw_rating_columns:
    # Create figure with subplots for each Pair type
    unique_pairs = sorted(survey_with_meta['Pair'].dropna().unique())
    n_pairs = len(unique_pairs)
    
    # Create a grid of subplots
    n_cols_dist = 4
    n_rows_dist = int(np.ceil(n_pairs / n_cols_dist))
    
    fig, axes = plt.subplots(n_rows_dist, n_cols_dist, figsize=(n_cols_dist * 4, n_rows_dist * 3))
    axes = axes.flatten() if n_pairs > 1 else [axes]
    
    # Plot distribution for each Pair
    for idx, pair in enumerate(unique_pairs):
        ax = axes[idx]
        pair_data = survey_with_meta[survey_with_meta['Pair'] == pair]
        
        # Plot distribution for each CarrierType
        for carrier_type in sorted(pair_data['CarrierType'].dropna().unique()):
            carrier_data = pair_data[pair_data['CarrierType'] == carrier_type][metric].dropna()
            if len(carrier_data) > 0:
                sns.histplot(carrier_data, kde=True, label=carrier_type, ax=ax, 
                           stat='density', alpha=0.5, bins=20)
        
        ax.set_title(f'{pair}', fontsize=10, fontweight='bold')
        ax.set_xlabel(f'{metric.replace("_", " ").title()} Score', fontsize=8)
        ax.set_ylabel('Density', fontsize=8)
        ax.legend(title='Carrier Type', fontsize=7)
        ax.grid(axis='y', alpha=0.3)
    
    # Hide empty subplots
    for idx in range(n_pairs, len(axes)):
        axes[idx].set_visible(False)
    
    # Overall title
    fig.suptitle(f'Distribution of Raw {metric.replace("_", " ").title()} Ratings by Pair Type and Carrier Type', 
                 fontsize=14, fontweight='bold')
    
    plt.tight_layout()
    
    # Save figure
    filename = f'{metric}_raw_distribution_by_pair_carriertype.png'
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    print(f"✓ Saved {filename}")
    
    plt.close()

print("\n✓ All 4 raw rating distribution plots created successfully!")

# ============================================================================
# PLOTTING Z-SCORED RATINGS BY PAIR TYPE AND CARRIER TYPE
# ============================================================================
print("\n" + "=" * 80)
print("CREATING PLOTS FOR Z-SCORED RATINGS")
print("=" * 80)

# Prepare data for plotting: merge ratings with metadata to get Pair and CarrierType
plot_data = trial_ratings.merge(trials_meta_copy, on='trial_id', how='left')

# Extract the two pair components and carrier type information from the Pair column
# Pair format is like "t-p" with carrier, so we need CarrierType column
print(f"\nColumns available for plotting: {list(plot_data.columns)}")

# Create 4 separate plots for each rating metric
rating_metrics = ['recognize', 'understand', 'meaning_match', 'natural']
zscore_cols_for_plotting = ['recognize_zscore_mean', 'understand_zscore_mean', 
                            'meaning_match_zscore_mean', 'natural_zscore_mean']

for metric, zscore_col in zip(rating_metrics, zscore_cols_for_plotting):
    # Create figure
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Create the plot with Pair on x-axis and CarrierType as hue (split)
    sns.barplot(data=plot_data, x='Pair', y=zscore_col, hue='CarrierType', 
                ax=ax, palette='Set2', order=sorted(plot_data['Pair'].unique()))
    
    # Customize plot
    ax.set_title(f'Z-Scored {metric.replace("_", " ").title()} Ratings by Pair Type and Carrier Type', 
                 fontsize=14, fontweight='bold')
    ax.set_xlabel('Pair Type', fontsize=12)
    ax.set_ylabel(f'Z-Scored {metric.replace("_", " ").title()}', fontsize=12)
    ax.legend(title='Carrier Type', fontsize=10)
    ax.grid(axis='y', alpha=0.3)
    
    # Rotate x-axis labels for better readability
    plt.xticks(rotation=45, ha='right')
    
    plt.tight_layout()
    
    # Save figure
    filename = f'{metric}_zscored_by_pair_carriertype.png'
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    print(f"✓ Saved {filename}")
    
    plt.close()

print("\n✓ All 4 plots created successfully!")

# ============================================================================
# LARGE SUBPLOT FIGURE: Z-SCORED RATINGS FOR EACH TRIAL
# ============================================================================
print("\n" + "=" * 80)
print("CREATING LARGE SUBPLOT FIGURE WITH ONE SUBPLOT PER TRIAL")
print("=" * 80)

# Get unique trials and sort them
unique_trials = plot_data.drop_duplicates(subset=['trial_id']).copy()
unique_trials = unique_trials.sort_values('trial_id').reset_index(drop=True)

n_trials = len(unique_trials)
print(f"\nTotal trials to plot: {n_trials}")

# Calculate subplot grid dimensions (aim for roughly square layout)
n_cols = int(np.ceil(np.sqrt(n_trials)))
n_rows = int(np.ceil(n_trials / n_cols))

print(f"Subplot grid: {n_rows} rows × {n_cols} columns")

# Create large figure
fig, axes = plt.subplots(n_rows, n_cols, figsize=(n_cols * 3, n_rows * 3))

# Flatten axes array for easier iteration
if n_trials == 1:
    axes = np.array([axes])
elif n_rows == 1 or n_cols == 1:
    axes = axes.flatten()
else:
    axes = axes.flatten()

print(f"Figure shape: {fig.get_size_inches()}")

# Iterate through each trial
for idx, (_, trial_row) in enumerate(unique_trials.iterrows()):
    trial_id = trial_row['trial_id']
    pair_type = trial_row['Pair']
    
    # Get data for this trial
    trial_data = plot_data[plot_data['trial_id'] == trial_id].copy()
    
    # Prepare data in long format for plotting the 4 metrics
    trial_metrics_data = []
    for metric, zscore_col in zip(rating_metrics, zscore_cols_for_plotting):
        for _, row in trial_data.iterrows():
            trial_metrics_data.append({
                'Metric': metric.replace('_', ' ').title(),
                'Z-Scored Value': row[zscore_col],
                'Carrier Type': row['CarrierType']
            })
    
    trial_metrics_df = pd.DataFrame(trial_metrics_data)
    
    # Create bar plot in this subplot
    ax = axes[idx]
    
    if len(trial_metrics_df) > 0:
        # Color bars based on positive (green) or negative (red) values
        colors = ['#2ECC71' if val >= 0 else '#E74C3C' for val in trial_metrics_df['Z-Scored Value']]
        
        x_pos = np.arange(len(trial_metrics_df))
        ax.bar(x_pos, trial_metrics_df['Z-Scored Value'], color=colors, alpha=0.7, edgecolor='black', linewidth=0.5)
        
        # Add horizontal line at 0
        ax.axhline(0, color='black', linewidth=1.5, linestyle='-', alpha=0.8)
        
        ax.set_title(f'Trial {trial_id}: {pair_type}', fontsize=10, fontweight='bold')
        ax.set_xlabel('', fontsize=8)
        ax.set_ylabel('Z-Scored Rating', fontsize=8)
        ax.set_xticks(x_pos)
        ax.set_xticklabels(trial_metrics_df['Metric'], fontsize=7, rotation=45, ha='right')
        ax.tick_params(axis='y', labelsize=7)
        ax.grid(axis='y', alpha=0.3, zorder=0)
    else:
        ax.text(0.5, 0.5, f'Trial {trial_id}\nNo data', 
               ha='center', va='center', transform=ax.transAxes)
        ax.set_xticks([])
        ax.set_yticks([])

# Hide empty subplots
for idx in range(n_trials, len(axes)):
    axes[idx].set_visible(False)

# Overall title
fig.suptitle('Z-Scored Ratings by Trial and Carrier Type', 
             fontsize=16, fontweight='bold', y=0.995)

plt.tight_layout(rect=[0, 0, 1, 0.99])

# Save the large figure
large_fig_filename = 'all_trials_subplots_zscored_ratings.png'
plt.savefig(large_fig_filename, dpi=150, bbox_inches='tight')
print(f"\n✓ Saved {large_fig_filename} ({n_rows}×{n_cols} grid)")

plt.close()

print("\n✓ Large subplot figure created successfully!")

# ============================================================================
# SUBPLOT FIGURE FOR TOP 54 TRIALS: Z-SCORED RATINGS FOR EACH TRIAL
# ============================================================================
print("\n" + "=" * 80)
print("CREATING SUBPLOT FIGURE FOR TOP 54 TRIALS")
print("=" * 80)

# Get unique trials from final_54_trials_sorted and sort them
top54_unique_trials = final_54_trials_sorted.drop_duplicates(subset=['trial_id']).copy()
top54_unique_trials = top54_unique_trials.sort_values('trial_id').reset_index(drop=True)

n_top54_trials = len(top54_unique_trials)
print(f"\nTotal top 54 trials to plot: {n_top54_trials}")

# Calculate subplot grid dimensions (aim for roughly square layout)
n_cols_top54 = int(np.ceil(np.sqrt(n_top54_trials)))
n_rows_top54 = int(np.ceil(n_top54_trials / n_cols_top54))

print(f"Subplot grid: {n_rows_top54} rows × {n_cols_top54} columns")

# Create large figure
fig, axes = plt.subplots(n_rows_top54, n_cols_top54, figsize=(n_cols_top54 * 3, n_rows_top54 * 3))

# Flatten axes array for easier iteration
if n_top54_trials == 1:
    axes = np.array([axes])
elif n_rows_top54 == 1 or n_cols_top54 == 1:
    axes = axes.flatten()
else:
    axes = axes.flatten()

print(f"Figure shape: {fig.get_size_inches()}")

# Iterate through each trial
for idx, (_, trial_row) in enumerate(top54_unique_trials.iterrows()):
    trial_id = trial_row['trial_id']
    pair_type = trial_row['Pair']
    
    # Get data for this trial from final_54_trials_sorted
    trial_data = final_54_trials_sorted[final_54_trials_sorted['trial_id'] == trial_id].copy()
    
    # Prepare data in long format for plotting the 4 metrics
    trial_metrics_data = []
    for metric, zscore_col in zip(rating_metrics, zscore_cols_for_plotting):
        for _, row in trial_data.iterrows():
            trial_metrics_data.append({
                'Metric': metric.replace('_', ' ').title(),
                'Z-Scored Value': row[zscore_col],
                'Carrier Type': row['CarrierType']
            })
    
    trial_metrics_df = pd.DataFrame(trial_metrics_data)
    
    # Create bar plot in this subplot
    ax = axes[idx]
    
    if len(trial_metrics_df) > 0:
        # Color bars based on positive (green) or negative (red) values
        colors = ['#2ECC71' if val >= 0 else '#E74C3C' for val in trial_metrics_df['Z-Scored Value']]
        
        x_pos = np.arange(len(trial_metrics_df))
        ax.bar(x_pos, trial_metrics_df['Z-Scored Value'], color=colors, alpha=0.7, edgecolor='black', linewidth=0.5)
        
        # Add horizontal line at 0
        ax.axhline(0, color='black', linewidth=1.5, linestyle='-', alpha=0.8)
        
        ax.set_title(f'Trial {trial_id}: {pair_type}', fontsize=10, fontweight='bold')
        ax.set_xlabel('', fontsize=8)
        ax.set_ylabel('Z-Scored Rating', fontsize=8)
        ax.set_xticks(x_pos)
        ax.set_xticklabels(trial_metrics_df['Metric'], fontsize=7, rotation=45, ha='right')
        ax.tick_params(axis='y', labelsize=7)
        ax.grid(axis='y', alpha=0.3, zorder=0)
    else:
        ax.text(0.5, 0.5, f'Trial {trial_id}\nNo data', 
               ha='center', va='center', transform=ax.transAxes)
        ax.set_xticks([])
        ax.set_yticks([])

# Hide empty subplots
for idx in range(n_top54_trials, len(axes)):
    axes[idx].set_visible(False)

# Overall title
fig.suptitle('Z-Scored Ratings by Trial and Carrier Type (Top 54 Trials)', 
             fontsize=16, fontweight='bold', y=0.995)

plt.tight_layout(rect=[0, 0, 1, 0.99])

# Save the figure
top54_fig_filename = 'top_54_trials_subplots_zscored_ratings.png'
plt.savefig(top54_fig_filename, dpi=150, bbox_inches='tight')
print(f"\n✓ Saved {top54_fig_filename} ({n_rows_top54}×{n_cols_top54} grid)")

plt.close()

print("\n✓ Top 54 trials subplot figure created successfully!")

# ============================================================================
# FILTERED SUBPLOT FIGURE: GS-K AND Q-K PAIRS ONLY
# ============================================================================
print("\n" + "=" * 80)
print("CREATING FILTERED SUBPLOT FIGURE (GS-K AND Q-K PAIRS ONLY)")
print("=" * 80)

# Filter to only gs-k and q-k pairs
filtered_plot_data = plot_data[plot_data['Pair'].isin(['gs-k', 'q-k'])].copy()

# Get unique trials from filtered data and sort them
unique_filtered_trials = filtered_plot_data.drop_duplicates(subset=['trial_id']).copy()
unique_filtered_trials = unique_filtered_trials.sort_values('trial_id').reset_index(drop=True)

n_filtered_trials = len(unique_filtered_trials)
print(f"\nTotal trials to plot (gs-k and q-k only): {n_filtered_trials}")

if n_filtered_trials > 0:
    # Calculate subplot grid dimensions (aim for roughly square layout)
    n_cols_filtered = int(np.ceil(np.sqrt(n_filtered_trials)))
    n_rows_filtered = int(np.ceil(n_filtered_trials / n_cols_filtered))
    
    print(f"Subplot grid: {n_rows_filtered} rows × {n_cols_filtered} columns")
    
    # Create large figure
    fig, axes = plt.subplots(n_rows_filtered, n_cols_filtered, 
                            figsize=(n_cols_filtered * 3, n_rows_filtered * 3))
    
    # Flatten axes array for easier iteration
    if n_filtered_trials == 1:
        axes = np.array([axes])
    elif n_rows_filtered == 1 or n_cols_filtered == 1:
        axes = axes.flatten()
    else:
        axes = axes.flatten()
    
    print(f"Figure shape: {fig.get_size_inches()}")
    
    # Iterate through each trial
    for idx, (_, trial_row) in enumerate(unique_filtered_trials.iterrows()):
        trial_id = trial_row['trial_id']
        pair_type = trial_row['Pair']
        
        # Get data for this trial
        trial_data = filtered_plot_data[filtered_plot_data['trial_id'] == trial_id].copy()
        
        # Prepare data in long format for plotting the 4 metrics
        trial_metrics_data = []
        for metric, zscore_col in zip(rating_metrics, zscore_cols_for_plotting):
            for _, row in trial_data.iterrows():
                trial_metrics_data.append({
                    'Metric': metric.replace('_', ' ').title(),
                    'Z-Scored Value': row[zscore_col],
                    'Carrier Type': row['CarrierType']
                })
        
        trial_metrics_df = pd.DataFrame(trial_metrics_data)
        
        # Create bar plot in this subplot
        ax = axes[idx]
        
        if len(trial_metrics_df) > 0:
            sns.barplot(data=trial_metrics_df, x='Metric', y='Z-Scored Value', 
                       hue='Carrier Type', ax=ax, palette='Set2')
            ax.set_title(f'Trial {trial_id}: {pair_type}', fontsize=10, fontweight='bold')
            ax.set_xlabel('', fontsize=8)
            ax.set_ylabel('Z-Scored Rating', fontsize=8)
            ax.tick_params(axis='x', labelsize=7, rotation=45)
            ax.tick_params(axis='y', labelsize=7)
            ax.legend(fontsize=7, title_fontsize=7)
            ax.grid(axis='y', alpha=0.3)
        else:
            ax.text(0.5, 0.5, f'Trial {trial_id}\nNo data', 
                   ha='center', va='center', transform=ax.transAxes)
            ax.set_xticks([])
            ax.set_yticks([])
    
    # Hide empty subplots
    for idx in range(n_filtered_trials, len(axes)):
        axes[idx].set_visible(False)
    
    # Overall title
    fig.suptitle('Z-Scored Ratings by Trial and Carrier Type (GS-K and Q-K Pairs Only)', 
                 fontsize=16, fontweight='bold', y=0.995)
    
    plt.tight_layout(rect=[0, 0, 1, 0.99])
    
    # Save the filtered figure
    filtered_fig_filename = 'gsk_qk_trials_subplots_zscored_ratings.png'
    plt.savefig(filtered_fig_filename, dpi=150, bbox_inches='tight')
    print(f"\n✓ Saved {filtered_fig_filename} ({n_rows_filtered}×{n_cols_filtered} grid)")
    
    plt.close()
    
    print("\n✓ Filtered subplot figure (gs-k and q-k) created successfully!")
else:
    print("\n⚠ No trials found for gs-k and q-k pairs")
