### plotting pharylary data as time series and some bar graphs too
# author: ben lang, blang@ucsd.edu, & GPT-4

import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.sandbox.regression.predstd import wls_prediction_std

data = pd.read_csv('/Volumes/circe/vs/output/preproc_output.csv', encoding='utf8')
data = data[data['tier'] == 'V-sequence']
# grouped = data.pivot_table(index = ['phrase'], columns = 'tier', values='strF0', aggfunc=np.mean)
subset_data = data[(data['interval'] == 'V-ħ-V') | (data['interval'] == 'V-h-V') | (data['interval'] == 'V-ʔ-V') | (data['interval'] == 'V-ʕ-V')]

# Grouping data by 'phrase' and 'label'
grouped = subset_data.groupby(['phrase', 'interval'])

# Plotting scatter points for each category
plt.figure(figsize=(10, 6))
# for name, group in grouped:
#     plt.scatter(group['t_prop'], group['strF0'], label=f'{name[0]}, {name[1]}')

# Colors for the regression lines
# colors = {'ħ': 'red', 'h': 'blue'}

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
    # Fit polynomial
    coefs = np.polyfit(subset['t_prop'], subset['CPP'], degree)
    # Evaluate polynomial
    polynomial = np.poly1d(coefs)
    t_prop_sorted = np.sort(subset['t_prop'])
    plt.plot(t_prop_sorted, polynomial(t_prop_sorted), label=f'{label}')

# set x-axis for plot
plt.xlim(0,100)

# Adding title, labels, and legend
plt.title('CPP Over Proportional Time with Polynomial Regression Lines')
plt.xlabel('Proportional Time (t_prop)')
plt.ylabel('CPP')
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



