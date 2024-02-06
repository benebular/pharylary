### plotting pharylary data as time series and some bar graphs too
# author: ben lang, blang@ucsd.edu, & GPT-4

import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.sandbox.regression.predstd import wls_prediction_std
from statsmodels.gam.api import GLMGam, BSplines
# from statsmodels.families import Gaussian
from pygam import LinearGAM, s
import ptitprince as pt

# with pd.option_context('display.max_rows', None, 'display.max_columns', None):  # more options can be specified also
#     print(data[['interval','Position','Position_2']])


data = pd.read_csv('/Volumes/circe/vs/output_preproc/preproc_output.csv', encoding='utf8')
# data = pd.read_csv('/Users/bcl/Desktop/preproc_output.csv', encoding='utf8')

# data = data[data['tier'] == 'phonetic']

##### RAINCLOUD MEANS #####

# ### slicing the data below takes about 5 minutes to run because iterrows() is slow ####
# Initialize an empty DataFrame to store matching rows
# matching_data = pd.DataFrame()

# # Iterate through each unique phrase
# for phrase in data['phrase'].unique():
#     # Filter data for the current phrase
#     phrase_data = data[data['phrase'] == phrase]

#     # Separate interval and V-sequence data within this phrase
#     interval_data = phrase_data[phrase_data['tier'] == 'phonetic']
#     v_sequence_data = phrase_data[phrase_data['tier'] == 'V-sequence']

#     # Iterate through the interval data
#     for index, interval_row in interval_data.iterrows():
#         # Check if this interval t_ms matches any V-sequence t_ms in the same phrase
#         if any(interval_row['t_ms'] == v_sequence_row.t_ms for v_sequence_row in v_sequence_data.itertuples()):
#             # Append matching row to the matching_data DataFrame
#             matching_data = matching_data.append(interval_row)

# matching_data.to_csv('preproc_matchesformeans.csv', index=False)
matching_data = pd.read_csv('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv', encoding='utf8')

### now slice so that it's just the C segments
means_data = matching_data[(matching_data['interval'] == 'ħ') | (matching_data['interval'] == 'h') | (matching_data['interval'] == 'ʔ') | (matching_data['interval'] == 'ʕ')]

# Define the colors for the specified labels
color_dict = {'h': '#d95f02', 'ʔ': '#7570b3', 'ħ': '#1b9e77', 'ʕ': '#e7298a'}
# Ensure that the color palette is ordered according to the labels in the 'interval' column
palette = [color_dict[label] for label in means_data['interval'].unique()]


# ax = pt.RainCloud(x='interval', y='H1H2c', data=means_data, bw=.2, width_viol=.6)

# # Show the plot
# ax.set_title('Distribution of Acoustic Features by Phonetic Label')
# plt.show()

# # F0, adding the boxplot with quartiles
# plot_F0_mean = pd.DataFrame({'group':'F0', 'F0': ratings_all['F0_mean']}).drop_duplicates()
# plot_F0_90 = pd.DataFrame({'group':'F0_90', 'F0': ratings_all['F0_90']}).drop_duplicates()
# plot_F0_10 = pd.DataFrame({'group':'F0_10', 'F0': ratings_all['F0_10']}).drop_duplicates()
# plot_F0 = pd.concat([plot_F0_10, plot_F0_mean, plot_F0_90])

# plot_F0_mean = pd.DataFrame({'group':'F0', 'F0': ratings_all['F0_mean']}).drop_duplicates()


# dx="group"; dy="F0"; ort="h"; pal = sns.color_palette(n_colors=3); sigma = .2
# f, ax = plt.subplots(figsize=(7, 5))
# pt.RainCloud(x = dx, y = dy, data = plot_F0, palette = pal, bw = sigma,
#                  width_viol = .6, ax = ax, orient = ort)

# plt.title("10th percentile; Average F0; 90th percentile, by speaker (across entire utterance), %s Participants"%number_participants)
# # plt.show()
# plt.savefig(os.path.join(fig_dir, 'F0_raincloud.png'), bbox_inches='tight', dpi=300)
# plt.close()



#########################################
####### Look at those GAMs baby #########
#########################################

data = data[data['tier'] == 'V-sequence']

# exclude the consonants for now
exclude_intervals = ['V-h-C','C-h-V','C-ħ-V','C-h','C-ħ','C-ʕ-V','V-ʔ-C','V-ħ-C']

# Filter the DataFrame
data = data[~data['interval'].isin(exclude_intervals)]

## assign colors consistently so they match the consonants
color_dict = {'ħ-V': '#1b9e77', 'h-V': '#d95f02', 'ʔ-V': '#7570b3', 'ʕ-V':'#e7298a',
            'V-ħ-V': '#1b9e77', 'V-h-V': '#d95f02', 'V-ʔ-V': '#7570b3', 'V-ʕ-V':'#e7298a',
            'V-ħ': '#1b9e77', 'V-h': '#d95f02', 'V-ʔ': '#7570b3', 'V-ʕ':'#e7298a'}

# make a big list of acoustic features you are interested in
acoustic_features = ['H1H2c','CPP','HNR05','SHR','strF0','soe','energy_prop','sF1','sF2','sF3']

# Identify features and positions
features = ['H1H2c','CPP','soe'] ## gamm plot
# features = ['SHR','strF0','HNR05'] ## gamm plot 2
# features = ['sF1','sF2','sF3'] ## gamm plot 3
positions = data['Position 2'].unique()

# establish labels of interest
# all_labels = ['ħ-V', 'h-V', 'ʔ-V', 'ʕ-V', 'V-ħ-V', 'V-h-V', 'V-ʔ-V', 'V-ʕ-V', 'V-ħ', 'V-h', 'V-ʔ', 'V-ʕ']
initial_labels = ['ħ-V', 'h-V', 'ʔ-V', 'ʕ-V']
medial_labels = ['V-ħ-V', 'V-h-V', 'V-ʔ-V', 'V-ʕ-V']
final_labels = ['V-ħ', 'V-h', 'V-ʔ', 'V-ʕ']

# Define a mapping of positions to their respective labels
position_label_map = {
    'CV': ['ħ-V', 'h-V', 'ʔ-V', 'ʕ-V'],
    'VCV': ['V-ħ-V', 'V-h-V', 'V-ʔ-V', 'V-ʕ-V'],
    'VC': ['V-ħ', 'V-h', 'V-ʔ', 'V-ʕ'],
    # Add more mappings as needed
}

# Define the colors for the specified labels
label_colors = {'h': '#d95f02', 'ʔ': '#7570b3', 'ħ': '#1b9e77', 'ʕ': '#e7298a'}

# Modify the positions list to reflect the swapping of first and third columns
# Swap 'Pos1' and 'Pos3' in the positions list
positions[0], positions[2] = positions[2], positions[0]

# Create a 3x3 grid of subplots
fig, axs = plt.subplots(3, 3, figsize=(15, 15))
axs = axs.flatten()  # Flatten for easy indexing

# Loop over each combination of feature and position
for i, feature in enumerate(features):

    # Dropping rows where required columns have NaNs
    data = data.dropna(subset=[feature])

    for j, position in enumerate(positions):

        # Determine the subplot index
        idx = i * len(positions) + j
        ax = axs[idx]

        # Get the designated labels for this position
        designated_labels = position_label_map.get(position, [])

        # Filter data for this feature and position
        subset = data[(data['Position 2'] == position) & (data['interval'].isin(designated_labels))]

        # Fit and plot GAM for each label
        # if pos == 'init':
        #     labels = initial_labels
        # if pos == 'med':
        #     labels = medial_labels
        # if pos == 'fin':
        #     labels = final_labels

        # if subset.empty:
        #     ax.axis('off')  # Turn off axis for empty subplots
        #     continue  # Skip the rest of the loop for empty subsets

        for label in designated_labels:
            label_subset = subset[subset['interval'] == label]

            # Define the spline for the model
            x_spline = BSplines(label_subset['t_prop'], df=[4], degree=[3])

            # Fit the GAM model
            gam = GLMGam.from_formula('%s ~ 1'%feature, data=label_subset, smoother=x_spline)
            gam_results = gam.fit()

            # Generate predictions for the plot
            XX = np.linspace(label_subset['t_prop'].min(), label_subset['t_prop'].max(), 100)
            XX_df = pd.DataFrame({'t_prop': XX})

            # Create predictions and confidence intervals
            predictions = gam_results.predict(XX)
            pred_conf_int = gam_results.get_prediction(XX).conf_int()

            # Plotting
            ax.plot(XX, predictions, color=color_dict[label], label=f'{label}', linewidth=1.5)
            ax.fill_between(XX, pred_conf_int[:, 0], pred_conf_int[:, 1], color=color_dict[label], alpha=0.2)

            # # pyGAM API
            # # Fit GAM for this label subset
            # gam = LinearGAM(s(0)).fit(label_subset['t_prop'], label_subset[feature])

            # # Make predictions
            # XX = np.linspace(label_subset['t_prop'].min(), label_subset['t_prop'].max(), 100)
            # predictions = gam.predict(XX)
            # intervals = gam.confidence_intervals(XX, width=.95)

            # # Plotting in subplot
            # ax.plot(XX, predictions, color=color_dict[label], label=f'{label}', linewidth=1.5)
            # ax.fill_between(XX, intervals[:, 0], intervals[:, 1], color=color_dict[label], alpha=0.2)

        ax.set_title(f'{position}')
        ax.set_xlabel('Proportional Time (t_prop)')
        ax.set_ylabel(feature)
        ax.legend()

# First pass: determine y-axis limits for each row
y_lims_per_row = [None] * 3
for i in range(3):
    min_y, max_y = float('inf'), float('-inf')
    for j in range(3):
        idx = i * 3 + j
        ax = axs[idx]
        current_min, current_max = ax.get_ylim()
        min_y = min(min_y, current_min)
        max_y = max(max_y, current_max)
    y_lims_per_row[i] = (min_y, max_y)

# Second pass: apply settings
for i in range(3):
    for j in range(3):
        idx = i * 3 + j
        ax = axs[idx]
        ax.set_ylim(y_lims_per_row[i])  # Standardize y-axis
        ax.grid(color='grey', alpha=0.3, linestyle='-', which='both')  # Apply grid

        # Remove x-axis labels but keep ticks
        ax.set_xlabel('')
        ax.tick_params(axis='x', which='both', length=6)  # Adjust x-tick properties if needed

        # Adjust y-axis for first column only
        if j == 0:
            ax.set_ylabel(ax.get_ylabel(), fontsize=16)  # Increase y-axis label font size
        else:
            ax.set_ylabel('')
            ax.tick_params(axis='y', colors='none')  # Hide y-axis ticks

        # # Turn off the legend for the first and second column
        # if i % 3 != 2:
        #     ax.legend().remove()

for ax in axs:
    ax.legend().remove()

# Add a common x-axis label
fig.text(0.5, 0.06, 'Proportional Time', ha='center', va='center', fontsize=16)

# Adding an overall title to the plot
fig.suptitle('Acoustic Features Over Proportional Time for Laryngeal and Pharyngeal Consonants', fontsize=18)

# Create a single shared legend at the bottom of the figure
# Using dummy line objects for the legend
line_objects = [plt.Line2D([0], [0], color=color, linewidth=4) for color in label_colors.values()]
fig.legend(line_objects, label_colors.keys(), loc='upper center', bbox_to_anchor=(0.5, 0.94), ncol=len(label_colors), fontsize=16)

# plt.tight_layout(rect=[0, 0.03, 1, 0.95])
# plt.savefig('/Users/bcl/Library/CloudStorage/GoogleDrive-blang@ucsd.edu/My Drive/Dissertation/experiment1/figs/gamm_plot.png', dpi=300)  # Save the figure with 300 dpi
# plt.close(fig)  # Close the figure after saving
plt.show()


##################
### HUGE PLOTS ###
##################


data = data[data['tier'] == 'V-sequence']
data = data[(data['interval'] == 'ħ-V') | (data['interval'] == 'h-V') | (data['interval'] == 'ʔ-V') | (data['interval'] == 'ʕ-V') |
                    (data['interval'] == 'V-ħ-V') | (data['interval'] == 'V-h-V') | (data['interval'] == 'V-ʔ-V') | (data['interval'] == 'V-ʕ-V') |
                    (data['interval'] == 'V-ħ') | (data['interval'] == 'V-h') | (data['interval'] == 'V-ʔ') | (data['interval'] == 'V-ʕ')]

acoustic_features = ['H1H2c','CPP','HNR05','SHR','strF0','soe','energy_prop','sF1','sF2','sF3']
# acoustic_features = ['sF1','sF2','sF3']
color_dict = {'ħ-V': '#1b9e77', 'h-V': '#d95f02', 'ʔ-V': '#7570b3', 'ʕ-V':'#e7298a',
            'V-ħ-V': '#1b9e77', 'V-h-V': '#d95f02', 'V-ʔ-V': '#7570b3', 'V-ʕ-V':'#e7298a',
            'V-ħ': '#1b9e77', 'V-h': '#d95f02', 'V-ʔ': '#7570b3', 'V-ʕ':'#e7298a'}

# Create a 3x3 grid of subplots
fig, axs = plt.subplots(2, 5, figsize=(20, 10))
axs = axs.flatten()  # Flatten the 2D array of axes for easier indexing

initial_labels = ['ħ-V', 'h-V', 'ʔ-V', 'ʕ-V']
medial_labels = ['V-ħ-V', 'V-h-V', 'V-ʔ-V', 'V-ʕ-V']
final_labels = ['V-ħ', 'V-h', 'V-ʔ', 'V-ʕ']

label_dict = {'initial': initial_labels, 'medial': medial_labels, 'final':final_labels}

for i, feature in enumerate(acoustic_features):
    # Dropping rows where 't_prop' or 'Acoustic Value' is NaN
    data = data.dropna(subset=['t_prop', feature])

    # grouped = data.pivot_table(index = ['phrase'], columns = 'tier', values='strF0', aggfunc=np.mean)
    # subset_data = data[(data['interval'] == 'V-ħ-V') | (data['interval'] == 'V-h-V') | (data['interval'] == 'V-ʔ-V') | (data['interval'] == 'V-ʕ-V')]
    # subset_data = data[(data['interval'] == 'ħ-V') | (data['interval'] == 'h-V') | (data['interval'] == 'ʔ-V') | (data['interval'] == 'ʕ-V')]

    # # Plotting
    # plt.figure(figsize=(10, 6))

    # # Add grid
    # plt.grid(color='grey', alpha=0.3)

    for label in final_labels:
        subset = data[data['interval'] == label]

        # Fitting a GAM
        gam = LinearGAM(s(0)).fit(subset['t_prop'], subset[feature])

        # Make predictions
        XX = np.linspace(subset['t_prop'].min(), subset['t_prop'].max(), 100)
        predictions = gam.predict(XX)
        intervals = gam.confidence_intervals(XX, width=.95)

        # Plotting the spline
        axs[i].plot(XX, predictions, color=color_dict[label], label=f'{label}')
        axs[i].fill_between(XX, intervals[:, 0], intervals[:, 1], color=color_dict[label], alpha=0.2)

    # Adding title, labels, and legend
    axs[i].set_title(f'Acoustic Feature: {feature}')
    axs[i].set_xlabel('Proportional Time (t_prop)')
    axs[i].set_ylabel(f'{feature}')
    axs[i].legend()
    axs[i].grid(color='grey',alpha=0.3)

# Hide any unused subplots
for j in range(i + 1, 10):
    axs[j].axis('off')

# Adding an overall title to the plot
fig.suptitle('Acoustic Features Over Proportional Time for Laryngeal and Pharyngeal Consonants in VCV Position', fontsize=16)

# Show plot
plt.tight_layout()
plt.show()







#########################
####### OLD STUFF #######
#########################
# Grouping data by 'phrase' and 'label'
grouped = subset_data.groupby(['phrase', 'interval'])

# Plotting scatter points for each category
plt.figure(figsize=(10, 6))
# for name, group in grouped:
#     plt.scatter(group['t_prop'], group['strF0'], label=f'{name[0]}, {name[1]}')

# Colors for the regression lines
# colors = {'V-ħ-V': '#fdae61', 'V-h-V': '#018571', 'V-ʔ-V': '#2c7bb6', 'V-ʕ-V':'#d7191c'}
colors = {'ħ-V': '#fdae61', 'h-V': '#018571', 'ʔ-V': '#2c7bb6', 'ʕ-V':'#d7191c'}


# degree for smoothing function of polynomial fit
degree = 2

# Fit and plot regression model for each label
# labels = subset_data['interval'].unique()
# for label in labels:
#     subset = subset_data[subset_data['interval'] == label]
#     X = sm.add_constant(subset['t_prop'])  # adding a constant
#     model = sm.OLS(subset['strF0'], X).fit()

#     # Regression line
#     line = model.predict(X)
#     plt.plot(subset['t_prop'], line, label=f'Regression Line {label}')

#     # Confidence interval
#     prstd, iv_l, iv_u = wls_prediction_std(model)
#     plt.fill_between(subset['t_prop'], iv_l, iv_u, alpha=0.1)

labels = subset_data['interval'].unique()
for label in labels:
    subset = subset_data[subset_data['interval'] == label]
    subset = subset[subset['H1H2c'].notna()]

    # plot raw scatter
    plt.scatter(subset['t_prop'], subset['H1H2c'], color=colors[label], alpha=0.05, label=f'Scatter {label}')

    # Fit polynomial
    coefs = np.polyfit(subset['t_prop'], subset['H1H2c'], degree)

    # Evaluate polynomial
    polynomial = np.poly1d(coefs)
    t_prop_sorted = np.sort(subset['t_prop'])
    plt.plot(t_prop_sorted, polynomial(t_prop_sorted), color=colors[label], label=f'Regression {label}', linewidth=2.0)

# set x-axis for plot
plt.xlim(0,100)
# plt.ylim(0,25)

# Adding title, labels, and legend
plt.title('H1H2c Over Proportional Time with Polynomial Regression Lines (degree = 2)')
plt.xlabel('Proportional Time (t_prop)')
plt.ylabel('H1H2c')
plt.legend()

# Show plot
plt.show()



#################
# STATIC VALUES #
#################

data = pd.read_csv('/Volumes/circe/vs/output/preproc_output.csv', encoding='utf8')
data = data[data['tier'] == 'phonetic']
# grouped = data.pivot_table(index = ['phrase'], columns = 'tier', values='strF0', aggfunc=np.mean)

subset_data = data[(data['interval'] == 'ħ') | (data['interval'] == 'h') | (data['interval'] == 'ʔ') | (data['interval'] == 'ʕ')]

for phrase
    for interval


window = 50
F0_avg_temp = data.groupby(['phrase','interval'])['strF0'].rolling(window=window).mean().to_frame()
F0_avg = F0_avg_temp.groupby(['phrase','interval'], as_index=False)['strF0'].mean()



