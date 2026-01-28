import pandas as pd
import numpy as np

# Load data
results = pd.read_csv('pharylary_survey_results.csv')
trials_meta = pd.read_csv('../trials.csv')

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

# 3. Merge results with trials metadata
print("\n" + "=" * 80)
print("MERGING DATA WITH TRIAL METADATA")
print("=" * 80)

# Check column names in trials_meta
print(f"\nColumns in trials.csv: {list(trials_meta.columns)}")
print(f"Columns in results: {list(results.columns)}")

# The trials.csv has a 'Trials' column that matches 'trial_id' in results
# Rename for clarity
if 'Trials' in trials_meta.columns:
    trials_meta = trials_meta.rename(columns={'Trials': 'trial_id'})
    print("\n✓ Renamed 'Trials' column to 'trial_id' for merging")

# Merge trials data with metadata
trials_with_meta = trials_data.merge(
    trials_meta, 
    on='trial_id', 
    how='left',
    suffixes=('', '_meta')
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
print("  - results: Full raw data")
print("  - participant_stats: Summary statistics per participant")
print("  - trials_df: All trial responses (with log_rt column)")
print("  - trials_with_meta: Trial responses merged with trial metadata")
print("  - survey_df: Survey responses")
print("  - demographics_df: Demographics responses")
print("  - attention_df: Attention check responses")
print("  - final_df: Final/completion records")
print("  - trials_meta: Trial metadata from trials.csv")
