import pandas as pd
import numpy as np
from scipy import stats

results = pd.read_csv('pharylary_survey_results.csv')
trials_meta = pd.read_csv('../trials.csv')

completed_ids = [
    '68d4239be163cb15b16606a6','68c03814183fb278ae8ac1c9','5c1d19c810677f0001d9d56c',
    '68d40473576087257c6dcaf3','611b00c6f6cc82766cd07c16','68d4670874bf80ef3de3834d',
    '691290f8a9d2ff4090233076','65f04e28869d1e36bcfe9bc2','68d6aebcb40c4b93989642c1',
    '694ef288fcfc4439fcc94ef9','68e3cfc22688ddaae48dc6c3','68c831e4060bd0ef510259ca',
    '68d3fde63f2c4007620e15d0','68cab84ebe4b9f5d7148e3dc','68d43a3c7fb9b4c1a9eee9af',
    '6960de0ad3223c83f5fbdf92','695ff3fde7fd7419df424235','68c89b71500cd48edf4fefe3',
    '68c830029bf5aaec07eaa790','66c0f6b657bd4b8a29485652'
]

results = results[results['participant_id'].isin(completed_ids)].copy()
trials_data = results[results['block'] == 'Trials'].copy()

trials_data['timestamp_dt'] = pd.to_datetime(trials_data['timestamp_iso'])
trials_data = trials_data.sort_values('timestamp_dt', ascending=False)
trials_data = trials_data.drop_duplicates(subset=['participant_id', 'trial_id'], keep='first')

trials_data = trials_data[trials_data['reaction_time_ms'] <= 10000].copy()

trial_accuracy = trials_data.groupby('trial_id')['is_correct'].apply(lambda x: (x=='yes').mean()).reset_index()
trial_accuracy.columns = ['trial_id','trial_accuracy']
trials_data = trials_data.merge(trial_accuracy, on='trial_id', how='left')

trials_data['log_rt_temp'] = np.log(trials_data['reaction_time_ms'].replace(0, np.nan))
trials_data['log_rt_zscore_temp'] = stats.zscore(trials_data['log_rt_temp'], nan_policy='omit')

accuracy_mean = trials_data['trial_accuracy'].mean()
accuracy_sd = trials_data['trial_accuracy'].std()
log_rt_zscore_mean = trials_data['log_rt_zscore_temp'].mean()
log_rt_zscore_sd = trials_data['log_rt_zscore_temp'].std()

accuracy_outliers = np.abs(trials_data['trial_accuracy'] - accuracy_mean) > 2*accuracy_sd
rt_outliers = np.abs(trials_data['log_rt_zscore_temp'] - log_rt_zscore_mean) > 2*log_rt_zscore_sd
outliers = accuracy_outliers | rt_outliers
trials_data = trials_data[~outliers].copy()

trials_data['log_rt_temp2'] = np.log(trials_data['reaction_time_ms'].replace(0, np.nan))
trials_data['log_rt_zscore_temp2'] = stats.zscore(trials_data['log_rt_temp2'], nan_policy='omit')
participant_avg_rt = trials_data.groupby('participant_id')['log_rt_zscore_temp2'].mean().reset_index()
participant_avg_rt.columns = ['participant_id','avg_log_rt_zscore']
avg_mean = participant_avg_rt['avg_log_rt_zscore'].mean()
avg_sd = participant_avg_rt['avg_log_rt_zscore'].std()

extreme_rt_participants = participant_avg_rt[np.abs(participant_avg_rt['avg_log_rt_zscore']-avg_mean) > 2*avg_sd]['participant_id'].tolist()
trials_data = trials_data[~trials_data['participant_id'].isin(extreme_rt_participants)].copy()

trials_data['log_rt'] = np.log(trials_data['reaction_time_ms'].replace(0, np.nan))
trials_data['log_rt_zscore'] = stats.zscore(trials_data['log_rt'], nan_policy='omit')

accuracy_by_trial = trials_data.groupby('trial_id').agg(
    mean_accuracy=('is_correct', lambda x: (x=='yes').mean()),
    n_responses=('is_correct','count')
).reset_index()

rt_by_trial = trials_data.groupby('trial_id').agg(
    mean_log_rt_zscore=('log_rt_zscore','mean'),
    mean_rt_ms=('reaction_time_ms','mean')
).reset_index()

trial_performance = accuracy_by_trial.merge(rt_by_trial, on='trial_id', how='left')

if 'Trial' in trials_meta.columns:
    trials_meta = trials_meta.rename(columns={'Trial':'trial_id'})

trials_all = trial_performance.merge(trials_meta, on='trial_id', how='left')
trials_all = trials_all[trials_all['mean_accuracy'].notna() & trials_all['mean_log_rt_zscore'].notna()].copy()

subset = trials_all[(trials_all['Pair'] == 'ʔ-k') & (trials_all['CarrierType'] == 'creaky')].copy()

print("=" * 80)
print("ʔ-k + CREAKY: DESCRIPTIVE STATISTICS")
print("=" * 80)

print(f"\nTotal trials: {len(subset)}")
print(f"\nDescriptive stats for mean_log_rt_zscore:")
print(subset['mean_log_rt_zscore'].describe())

print(f"\n\nIndividual trials (sorted by RT z-score):")
print(subset[['trial_id', 'Position', 'TargetSegment', 'mean_log_rt_zscore', 'mean_accuracy', 'n_responses']].sort_values('mean_log_rt_zscore', ascending=False).to_string(index=False))

print(f"\n\nBy Position/TargetSegment:")
for pos in sorted(subset['Position'].dropna().unique()):
    for seg in sorted(subset[subset['Position']==pos]['TargetSegment'].dropna().unique()):
        group = subset[(subset['Position']==pos) & (subset['TargetSegment']==seg)]
        print(f"{pos}/{seg}: n={len(group)}, mean_rt={group['mean_log_rt_zscore'].mean():.3f}, std={group['mean_log_rt_zscore'].std():.3f}, min={group['mean_log_rt_zscore'].min():.3f}, max={group['mean_log_rt_zscore'].max():.3f}")

print("\n" + "=" * 80)
print("VERDICT:")
print("=" * 80)
if len(subset) <= 5:
    print(f"Very small sample size (n={len(subset)}) - std will be extremely unstable!")
    print("This is the likely cause of the large error bars.")
