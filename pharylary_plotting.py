### plotting pharylary data as time series and some bar graphs too
# author: ben lang, blang@ucsd.edu, & GPT-4

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
from scipy.interpolate import interp1d

data = pd.read_csv('/Users/bcl/Desktop/preproc_output.csv', encoding='utf8')

# data = data[data['tier']=='V-sequence']


# # Example time series data - replace with your actual data
# time_series_data = [
#     {'Time': np.linspace(0, 5, 6), 'AcousticValue': np.random.rand(6)},
#     {'Time': np.linspace(0, 5, 10), 'AcousticValue': np.random.rand(10)},
#     # Add more time series here
# ]

# Grouping by TierType and PhraseType
grouped = data.groupby(['phrase', 'tier'])

# Proportional time points (0% to 100%)
proportional_time = np.linspace(0, 100, 101)

# Initializing a dictionary to store average time series for each group
average_series = {}

# Processing each group
for group, df in grouped:
    interpolated_values = []

    for _, sub_df in df.groupby('t_ms'):
        # Convert time to a proportional scale
        prop_time = 100 * (sub_df['t_ms'] - sub_df['t_ms'].min()) / (sub_df['t_ms'].max() - sub_df['t_ms'].min())
        # Interpolate acoustic values to the proportional time points
        f = interp1d(prop_time, sub_df['strF0'], kind='linear', fill_value='extrapolate', bounds_error=False)
        interpolated_values.append(f(proportional_time))

    # Calculate the average of the interpolated values for each proportional time point
    average_series[group] = np.mean(interpolated_values, axis=0)

# Plotting the average time series for each group
fig, axs = plt.subplots(len(average_series), 1, figsize=(10, 4 * len(average_series)))
axs = axs.flatten()

for i, (group, avg_values) in enumerate(average_series.items()):
    axs[i].plot(proportional_time, avg_values)
    axs[i].set_title(f'Average for TierType {group[0]}, PhraseType {group[1]}')
    axs[i].set_xlabel('Proportional Time (%)')
    axs[i].set_ylabel('Average strF0')

plt.tight_layout()
plt.show()

#################
# STATIC VALUES #
#################

data = pd.read_csv('/Users/bcl/Desktop/preproc_output.csv', encoding='utf8')
data = data[data['tier'] == 'phonetic']
# grouped = data.pivot_table(index = ['phrase'], columns = 'tier', values='strF0', aggfunc=np.mean)

window = 50
F0_avg_temp = data.groupby(['phrase','interval'])['strF0'].rolling(window=window).mean().to_frame()
F0_avg = F0_avg_temp.groupby(['phrase','interval'], as_index=False)['strF0'].mean()



