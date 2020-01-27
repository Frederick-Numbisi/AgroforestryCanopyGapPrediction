# -*- coding: utf-8 -*-
"""
Created on Thu May  2 10:20:04 2019

@author: Fred
"""

# Pandas is used for data manipulation
import pandas as pd

import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import MinMaxScaler

# Read in data and display first 5 rows
features = pd.read_csv('BokEfoul_Gamma0GF.csv') # read data set using pandas
features.head(5)

print(features.info()) # Overview of dataset
#features.loc[(features['luse'] == 'B_Co') or (features['luse'] == 'E_Co'),'UseClass'] = 'cafs'
#features.loc[(features['luse'] == 'B_Sf') or (features['luse'] == 'E_Sf'),'UseClass'] = 'sf'

CAFS = ['E_Co','B_Co']
SF = ['E_Sf','B_Sf']

#Tcoa.loc[Tcoa['PlotID'].isin(Fplots),  'Origin'] = 'Forest'
features.loc[features['luse'].isin(['E_Co','B_Co']),'UseClass'] = 'cafs'
features.loc[features['luse'].isin(['E_Sf','B_Sf']),'UseClass'] = 'sf'     
 
features.describe()        
features['gap_fraction'].describe() 
features.groupby('UseClass')['gap_fraction'].describe()
features.groupby('UseClass')['gap_fraction'].describe().transpose()

features = features.drop(['PlotID'],axis=1) # Drop PlotID feature
features = features.dropna(inplace=False)  # Remove all nan entries.
features = features.drop(['no_of_gaps', 'gap_area', 'luse', 'UseClass'],axis=1) # Drop 'no_of_gaps' and 'gap_area' feature
features.head(5)


print('The shape of our features is:', features.shape)


# Descriptive statistics for each column
features.describe()

'''
# One-hot encode the data using pandas get_dummies
features = pd.get_dummies(features)

# Display the first 5 rows of the last 12 columns
features.iloc[:,5:].head(5)

'''

# Use numpy to convert to arrays

# Labels are the values we want to predict
labels = np.array(features['gap_fraction'])

# Remove the labels from the features
# axis 1 refers to the columns
features= features.drop('gap_fraction', axis = 1)

# Saving feature names for later use
feature_list = list(features.columns)

# Convert to numpy array
features = np.array(features)



#----------------------------
# Using Skicit-learn to split data into training and testing sets
from sklearn.model_selection import train_test_split

# Split the data into training and testing sets
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = 0.3, random_state = 42)

print('Training Features Shape:', train_features.shape)
print('Training Labels Shape:', train_labels.shape)
print('Testing Features Shape:', test_features.shape)
print('Testing Labels Shape:', test_labels.shape)

# SAVING NUMPY ARRAYS AS CSV FILES
np.savetxt('train_features.csv', train_features, delimiter=",")
np.savetxt('train_labels.csv', train_labels, delimiter=",")
np.savetxt('test_features.csv', test_features, delimiter=",")
np.savetxt('test_labels.csv', test_labels, delimiter=",")




# The baseline predictions are the average (global) gap fraction for all sampled plot
#baseline_preds = 14.28 # Global mean of Gap Fraction for all plots
baseline_preds = np.mean(test_labels) # mean Gap Fraction for the test dataset
baseline_preds



# Baseline errors, and display average baseline error
baseline_errors = abs(baseline_preds - test_labels)

print('Average baseline error: ', round(np.mean(baseline_errors), 2))

'''
We now have our goal! 
If we can’t beat an average error of 5.55 %, then we need to rethink our 
approach or revise our model.

'''

#------------TRAINING THE MODEL----------------

# Import the model we are using
from sklearn.ensemble import RandomForestRegressor

# Instantiate model with 1000 decision trees
rf = RandomForestRegressor(n_estimators = 500, random_state = 42)
#rf = RandomForestRegressor(random_state = 42)

# Train the model on training data
rf.fit(train_features, train_labels)


#---------------------------------
'''
Making predictions with out model is another 1-line command in Skicit-learn.
'''
# Use the forest's predict method on the test data
predictions = rf.predict(test_features)

# Calculate the absolute errors
errors = abs(predictions - test_labels)

# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2), '%.')


'''
from sklearn.metrics import roc_curve, auc

false_positive_rate, true_positive_rate, thresholds = roc_curve(test_labels, predictions)

roc_auc = auc(false_positive_rate, true_positive_rate)

roc_auc
'''

# Calculate mean absolute percentage error (MAPE)
mape = 100 * (errors / test_labels)

# Calculate and display accuracy
accuracy = 100 - np.mean(mape)
print('Accuracy:', round(accuracy, 2), '%.')



#TestPred = pd.concat([pd.Dataframe(test_labels), pd.Dataframe(predictions)])



'''
#Import tools needed for visualization
from sklearn.tree import export_graphviz
import pydot

# Pull out one tree from the forest
tree = rf.estimators_[5]

# Export the image to a dot file
export_graphviz(tree, out_file = 'tree.dot', feature_names = feature_list, rounded = True, precision = 1)

# Use dot file to create a graph
(graph, ) = pydot.graph_from_dot_file('tree.dot')

# Write graph to a png file
graph.write_png('tree.png')
'''

# Get numerical feature importances
importances = list(rf.feature_importances_)

# List of tuples with variable and importance
feature_importances = [(feature, round(importance, 2)) for feature,
                       importance in zip(feature_list, importances)]

# Sort the featue importances by most important first
feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse =True)

# Print out thhe feature and importances

[print('Variable: {:10} Importance: {}'.format(*pair)) for pair in feature_importances]


# PLOTTING THE IMPORTANT VARIBLES



# Set the stype
#plt.style.use('fivethirtyeight')

#%matplotlib inline

# list of x location for plotting
x_values = list(range(len(importances)))

# ake a bar chart
plt.bar(x_values, importances, orientation = 'vertical')

# Tick labes for x axis
plt.xticks(x_values, feature_list, rotation = 'vertical')

# Axis labels and title
plt.ylabel('Importance'); plt.xlabel('Variables');

plt.savefig("Gap_Fraction_RFimportance.jpg", dpi=300)
plt.savefig("Gap_Fraction_RFimportance.pdf", dpi=300)
plt.savefig("Gap_Fraction_RFimportance.eps", dpi=300)
plt.savefig("Gap_Fraction_RFimportance.svg", dpi=300)


#----------------------------------------------------
#---------------------------------------------------- MSE and RMSE
'''
Making predictions with out model is another 1-line command in Skicit-learn.

'''

from sklearn.metrics import mean_squared_error
from math import sqrt

# Use the forest's predict method on the test data
predictions = rf.predict(test_features)

# Calculate the absolute errors
errors = abs(predictions - test_labels)

# Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2), '%.')
print('MAE:', round(np.mean(errors), 2), '%.')

'''
from sklearn.metrics import roc_curve, auc

false_positive_rate, true_positive_rate, thresholds = roc_curve(test_labels, predictions)

roc_auc = auc(false_positive_rate, true_positive_rate)

roc_auc
'''

# Calculate mean absolute percentage error (MAPE)
mape = 100 * (errors / test_labels)

# Calculate and display accuracy
accuracy = 100 - np.mean(mape)
print('Accuracy:', round(accuracy, 2), '%.')


# MSE

print(predictions)

print("Number of predictions:", len(predictions))

MSE = mean_squared_error(test_labels,predictions)
print("MSE:", MSE)
RMSE = sqrt(MSE)
print("RMSE:", RMSE)


