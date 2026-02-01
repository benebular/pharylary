import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load the top 54 trials data
trials_top54 = pd.read_csv('top_54_trials_complete.csv')

print("=" * 80)
print("PLOTTING TRIAL SELECTION RESULTS")
print("=" * 80)
print(f"\nLoaded {len(trials_top54)} top selected trials")

# Load ALL trials data with metadata and performance stats
print("\nLoading all trials data...")
trial_ratings = pd.read_csv('trial_ratings_zscored.csv')
trials_meta = pd.read_csv('trials_meta.csv')

# Load trial performance from analyze_results (we need to recalculate or load)
# For now, we'll load from the saved file or recalculate
# We need to load the raw results and recalculate performance
results = pd.read_csv('pharylary_survey_results.csv')

# Filter to completed participants (same as in analyze_results.py)
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

results = results[results['participant_id'].isin(completed_ids)].copy()
trials_data = results[results['block'] == 'Trials'].copy()

# Deduplicate and clean (same as analyze_results.py)
trials_data['timestamp_dt'] = pd.to_datetime(trials_data['timestamp_iso'])
trials_data = trials_data.sort_values('timestamp_dt', ascending=False)
trials_data = trials_data.drop_duplicates(subset=['participant_id', 'trial_id'], keep='first')

# Calculate accuracy per trial for outlier detection
trial_accuracy = trials_data.groupby('trial_id')['is_correct'].apply(
    lambda x: (x == 'yes').mean()
).reset_index()
trial_accuracy.columns = ['trial_id', 'trial_accuracy']
trials_data = trials_data.merge(trial_accuracy, on='trial_id', how='left')

# Calculate log RT z-score for outlier detection
from scipy import stats
trials_data['log_rt_temp'] = np.log(trials_data['reaction_time_ms'].replace(0, np.nan))
trials_data['log_rt_zscore_temp'] = stats.zscore(trials_data['log_rt_temp'], nan_policy='omit')

# Remove outliers (2 SD threshold)
accuracy_mean = trials_data['trial_accuracy'].mean()
accuracy_sd = trials_data['trial_accuracy'].std()
log_rt_zscore_mean = trials_data['log_rt_zscore_temp'].mean()
log_rt_zscore_sd = trials_data['log_rt_zscore_temp'].std()

accuracy_outliers = np.abs(trials_data['trial_accuracy'] - accuracy_mean) > 2 * accuracy_sd
rt_outliers = np.abs(trials_data['log_rt_zscore_temp'] - log_rt_zscore_mean) > 2 * log_rt_zscore_sd
outliers = accuracy_outliers | rt_outliers

trials_data = trials_data[~outliers].copy()
trials_data = trials_data.drop(columns=['trial_accuracy', 'log_rt_temp', 'log_rt_zscore_temp'])

# Calculate mean accuracy per trial
accuracy_by_trial = trials_data.groupby('trial_id').agg(
    mean_accuracy=('is_correct', lambda x: (x == 'yes').mean()),
    n_responses=('is_correct', 'count')
).reset_index()

# Calculate log RT z-score per trial
trials_data['log_rt'] = np.log(trials_data['reaction_time_ms'].replace(0, np.nan))
trials_data['log_rt_zscore'] = stats.zscore(trials_data['log_rt'], nan_policy='omit')

rt_by_trial = trials_data.groupby('trial_id').agg(
    mean_log_rt_zscore=('log_rt_zscore', 'mean'),
    mean_rt_ms=('reaction_time_ms', 'mean')
).reset_index()

# Merge accuracy and RT stats
trial_performance = accuracy_by_trial.merge(rt_by_trial, on='trial_id', how='left')

# Merge with metadata
trials_all = trial_ratings.merge(trials_meta, on='trial_id', how='left')
trials_all = trials_all.merge(trial_performance, on='trial_id', how='left')

# Filter out only missing data (keeping t-none and k-none pairs)
trials_all = trials_all[
    trials_all['mean_accuracy'].notna() &
    trials_all['mean_log_rt_zscore'].notna()
].copy()

print(f"Total trials in full dataset: {len(trials_all)}")

# Check the unique values for grouping variables
print(f"\n--- Top 54 Trials ---")
print(f"Unique Positions: {sorted(trials_top54['Position'].unique())}")
print(f"Unique Target Segments: {sorted(trials_top54['TargetSegment'].unique())}")
print(f"Unique Carrier Types: {sorted(trials_top54['CarrierType'].unique())}")

print(f"\n--- All Trials ---")
print(f"Unique Positions: {sorted(trials_all['Position'].unique())}")
print(f"Unique Target Segments: {sorted(trials_all['TargetSegment'].unique())}")
print(f"Unique Carrier Types: {sorted(trials_all['CarrierType'].unique())}")

# Define colors for CarrierType
carrier_colors = {
    'non-creaky': '#D32F2F',      # Red
    'creaky': '#1976D2',   # Blue
    'CV': '#7B1FA2'         # Purple
}

def create_grouped_barplot(data, y_col, ylabel, title, center_at_zero=False):
    """
    Create a figure with subplots by Position, grouped by TargetSegment, split by CarrierType
    """
    # Get unique positions in specific order
    position_order = ['Initial', 'Medial', 'Final']
    positions = [p for p in position_order if p in data['Position'].unique()]
    n_positions = len(positions)
    
    # Create figure with subplots
    fig, axes = plt.subplots(1, n_positions, figsize=(6 * n_positions, 6), sharey=True)
    
    # If only one position, make axes iterable
    if n_positions == 1:
        axes = [axes]
    
    # Track handles and labels for shared legend
    handles_dict = {}
    
    for idx, position in enumerate(positions):
        ax = axes[idx]
        position_data = data[data['Position'] == position]
        
        # Get unique target segments and carrier types
        target_segments = sorted(position_data['TargetSegment'].unique())
        carrier_types = sorted(position_data['CarrierType'].unique())
        
        # Set up x positions for bars
        x = np.arange(len(target_segments))
        width = 0.25  # Width of each bar
        
        # Plot bars for each carrier type
        for i, carrier in enumerate(carrier_types):
            carrier_data = position_data[position_data['CarrierType'] == carrier]
            
            # Calculate mean for each target segment
            means = []
            errors = []
            for segment in target_segments:
                segment_values = carrier_data[carrier_data['TargetSegment'] == segment][y_col]
                if len(segment_values) > 0:
                    means.append(segment_values.mean())
                    errors.append(segment_values.std())
                else:
                    means.append(0)
                    errors.append(0)
            
            # Plot bars
            offset = (i - len(carrier_types)/2 + 0.5) * width
            bars = ax.bar(x + offset, means, width, 
                   label=carrier, 
                   color=carrier_colors.get(carrier, 'gray'),
                   yerr=errors,
                   capsize=5,
                   alpha=0.8,
                   edgecolor='black',
                   linewidth=1)
            
            # Store handle for legend (only once per carrier type)
            if carrier not in handles_dict:
                handles_dict[carrier] = bars
        
        # Customize subplot
        ax.set_xlabel('Target Segment', fontsize=12, fontweight='bold')
        ax.set_ylabel(ylabel if idx == 0 else '', fontsize=12, fontweight='bold')
        ax.set_title(f'Position: {position}', fontsize=14, fontweight='bold')
        ax.set_xticks(x)
        ax.set_xticklabels(target_segments, fontsize=11)
        ax.grid(axis='y', alpha=0.3, linestyle='--')
        
        # If centering at zero, adjust y-axis
        if center_at_zero:
            ylim = ax.get_ylim()
            max_abs = max(abs(ylim[0]), abs(ylim[1]))
            ax.set_ylim(-max_abs, max_abs)
            ax.axhline(0, color='black', linewidth=1.5, linestyle='-', alpha=0.5)
    
    # Overall title
    fig.suptitle(title, fontsize=16, fontweight='bold', y=1.02)
    
    # Add single legend for entire figure
    carrier_types_sorted = sorted(handles_dict.keys())
    handles = [handles_dict[ct] for ct in carrier_types_sorted]
    fig.legend(handles, carrier_types_sorted, title='Carrier Type', 
               loc='upper center', bbox_to_anchor=(0.5, -0.02), 
               ncol=len(carrier_types_sorted), fontsize=11, frameon=True)
    
    plt.tight_layout()
    
    return fig

def create_grouped_barplot_by_pair(data, y_col, ylabel, title, center_at_zero=False):
    """
    Create a figure with subplots by Position, grouped by Pair, split by CarrierType
    """
    # Get unique positions in specific order
    position_order = ['Initial', 'Medial', 'Final']
    positions = [p for p in position_order if p in data['Position'].unique()]
    n_positions = len(positions)
    
    # Create figure with subplots
    fig, axes = plt.subplots(1, n_positions, figsize=(8 * n_positions, 6), sharey=True)
    
    # If only one position, make axes iterable
    if n_positions == 1:
        axes = [axes]
    
    # Track handles and labels for shared legend
    handles_dict = {}
    
    for idx, position in enumerate(positions):
        ax = axes[idx]
        position_data = data[data['Position'] == position]
        
        # Get unique pairs and carrier types
        pairs = sorted(position_data['Pair'].unique())
        carrier_types = sorted(position_data['CarrierType'].unique())
        
        # Set up x positions for bars
        x = np.arange(len(pairs))
        width = 0.25  # Width of each bar
        
        # Plot bars for each carrier type
        for i, carrier in enumerate(carrier_types):
            carrier_data = position_data[position_data['CarrierType'] == carrier]
            
            # Calculate mean for each pair
            means = []
            errors = []
            for pair in pairs:
                pair_values = carrier_data[carrier_data['Pair'] == pair][y_col]
                if len(pair_values) > 0:
                    means.append(pair_values.mean())
                    errors.append(pair_values.std())
                else:
                    means.append(0)
                    errors.append(0)
            
            # Plot bars
            offset = (i - len(carrier_types)/2 + 0.5) * width
            bars = ax.bar(x + offset, means, width, 
                   label=carrier, 
                   color=carrier_colors.get(carrier, 'gray'),
                   yerr=errors,
                   capsize=5,
                   alpha=0.8,
                   edgecolor='black',
                   linewidth=1)
            
            # Store handle for legend (only once per carrier type)
            if carrier not in handles_dict:
                handles_dict[carrier] = bars
        
        # Customize subplot
        ax.set_xlabel('Pair', fontsize=12, fontweight='bold')
        ax.set_ylabel(ylabel if idx == 0 else '', fontsize=12, fontweight='bold')
        ax.set_title(f'Position: {position}', fontsize=14, fontweight='bold')
        ax.set_xticks(x)
        ax.set_xticklabels(pairs, fontsize=9, rotation=45, ha='right')
        ax.grid(axis='y', alpha=0.3, linestyle='--')
        
        # If centering at zero, adjust y-axis
        if center_at_zero:
            ylim = ax.get_ylim()
            max_abs = max(abs(ylim[0]), abs(ylim[1]))
            ax.set_ylim(-max_abs, max_abs)
            ax.axhline(0, color='black', linewidth=1.5, linestyle='-', alpha=0.5)
    
    # Overall title
    fig.suptitle(title, fontsize=16, fontweight='bold', y=1.02)
    
    # Add single legend for entire figure
    carrier_types_sorted = sorted(handles_dict.keys())
    handles = [handles_dict[ct] for ct in carrier_types_sorted]
    fig.legend(handles, carrier_types_sorted, title='Carrier Type', 
               loc='upper center', bbox_to_anchor=(0.5, -0.05), 
               ncol=len(carrier_types_sorted), fontsize=11, frameon=True)
    
    plt.tight_layout()
    
    return fig

# ============================================================================
# PLOTS FOR TOP 54 SELECTED TRIALS
# ============================================================================

print("\n" + "=" * 80)
print("CREATING PLOTS FOR TOP 54 SELECTED TRIALS")
print("=" * 80)

# Plot 1: Mean Accuracy (Top 54)
print("\nCreating accuracy plot for top 54 trials...")
fig1 = create_grouped_barplot(
    trials_top54, 
    'mean_accuracy', 
    'Mean Accuracy', 
    'Mean Accuracy by Position, Target Segment, and Carrier Type (Top 54 Trials)',
    center_at_zero=False
)
fig1.savefig('plot_accuracy_top54.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_accuracy_top54.png")

# Plot 2: Mean Log RT Z-score (centered at 0) (Top 54)
print("Creating reaction time plot for top 54 trials...")
fig2 = create_grouped_barplot(
    trials_top54, 
    'mean_log_rt_zscore', 
    'Mean Log RT (Z-score)', 
    'Mean Log RT Z-score by Position, Target Segment, and Carrier Type (Top 54 Trials)',
    center_at_zero=True
)
fig2.savefig('plot_rt_zscore_top54.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_rt_zscore_top54.png")

# ============================================================================
# PLOTS FOR ALL TRIALS
# ============================================================================

print("\n" + "=" * 80)
print("CREATING PLOTS FOR ALL TRIALS")
print("=" * 80)

# Plot 3: Mean Accuracy (All Trials)
print("\nCreating accuracy plot for all trials...")
fig3 = create_grouped_barplot(
    trials_all, 
    'mean_accuracy', 
    'Mean Accuracy', 
    'Mean Accuracy by Position, Target Segment, and Carrier Type (All Trials)',
    center_at_zero=False
)
fig3.savefig('plot_accuracy_all.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_accuracy_all.png")

# Plot 4: Mean Log RT Z-score (centered at 0) (All Trials)
print("Creating reaction time plot for all trials...")
fig4 = create_grouped_barplot(
    trials_all, 
    'mean_log_rt_zscore', 
    'Mean Log RT (Z-score)', 
    'Mean Log RT Z-score by Position, Target Segment, and Carrier Type (All Trials)',
    center_at_zero=True
)
fig4.savefig('plot_rt_zscore_all.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_rt_zscore_all.png")

# ============================================================================
# PLOTS FOR ALL TRIALS (GROUPED BY PAIR)
# ============================================================================

print("\n" + "=" * 80)
print("CREATING PLOTS FOR ALL TRIALS (GROUPED BY PAIR)")
print("=" * 80)

# Plot 5: Mean Accuracy (All Trials, by Pair)
print("\nCreating accuracy plot for all trials (grouped by Pair)...")
fig5 = create_grouped_barplot_by_pair(
    trials_all, 
    'mean_accuracy', 
    'Mean Accuracy', 
    'Mean Accuracy by Position, Pair, and Carrier Type (All Trials)',
    center_at_zero=False
)
fig5.savefig('plot_accuracy_all_by_pair.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_accuracy_all_by_pair.png")

# Plot 6: Mean Log RT Z-score (centered at 0) (All Trials, by Pair)
print("Creating reaction time plot for all trials (grouped by Pair)...")
fig6 = create_grouped_barplot_by_pair(
    trials_all, 
    'mean_log_rt_zscore', 
    'Mean Log RT (Z-score)', 
    'Mean Log RT Z-score by Position, Pair, and Carrier Type (All Trials)',
    center_at_zero=True
)
fig6.savefig('plot_rt_zscore_all_by_pair.png', dpi=300, bbox_inches='tight')
print("✓ Saved plot_rt_zscore_all_by_pair.png")

# Print summary statistics
print("\n" + "=" * 80)
print("SUMMARY STATISTICS BY GROUPING VARIABLES")
print("=" * 80)

for dataset_name, dataset in [('Top 54 Trials', trials_top54), ('All Trials', trials_all)]:
    print(f"\n{'=' * 80}")
    print(f"{dataset_name} (n={len(dataset)})")
    print(f"{'=' * 80}")
    
    for position in sorted(dataset['Position'].unique()):
        print(f"\n{position}:")
        position_data = dataset[dataset['Position'] == position]
        
        for segment in sorted(position_data['TargetSegment'].unique()):
            segment_data = position_data[position_data['TargetSegment'] == segment]
            print(f"\n  {segment}:")
            
            for carrier in sorted(segment_data['CarrierType'].unique()):
                carrier_data = segment_data[segment_data['CarrierType'] == carrier]
                
                acc_mean = carrier_data['mean_accuracy'].mean()
                acc_std = carrier_data['mean_accuracy'].std()
                rt_mean = carrier_data['mean_log_rt_zscore'].mean()
                rt_std = carrier_data['mean_log_rt_zscore'].std()
                n = len(carrier_data)
                
                print(f"    {carrier} (n={n}):")
                print(f"      Accuracy: {acc_mean:.3f} ± {acc_std:.3f}")
                print(f"      RT Z-score: {rt_mean:.3f} ± {rt_std:.3f}")

print("\n" + "=" * 80)
print("PLOTTING COMPLETE!")
print("=" * 80)
print("\nGenerated files:")
print("  Top 54 Trials (by Target Segment):")
print("    - plot_accuracy_top54.png")
print("    - plot_rt_zscore_top54.png")
print("  All Trials (by Target Segment):")
print("    - plot_accuracy_all.png")
print("    - plot_rt_zscore_all.png")
print("  All Trials (by Pair):")
print("    - plot_accuracy_all_by_pair.png")
print("    - plot_rt_zscore_all_by_pair.png")

plt.show()
