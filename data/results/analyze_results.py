import pandas as pd
import numpy as np

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
