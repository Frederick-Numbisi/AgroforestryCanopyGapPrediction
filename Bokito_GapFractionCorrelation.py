# -*- coding: utf-8 -*-
"""
Created on Thu Mar 14 20:02:29 2019

@author: Fred
"""

# -*- coding: utf-8 -*-
"""
Created on Tue Jan 30 22:59:26 2018

@author: Fred
"""

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt



Bok_Gapfraction = pd.read_csv('Bokito_GapFraction2016n2017.csv')
Bok_Gamma0 = pd.read_csv('Bokito_Extracted_SAR_Gamma0db.csv')


print (Bok_Gamma0.columns)

# Summary the backscatter data by the mean of each plot

Gamma0mean = Bok_Gamma0.groupby(['PlotID', 'SARdate'])[['VHgamma0', 'VVgamma0',
       'VHdb', 'VVdb', 'VV-VH', 'VH-VVnorm', 'VH-VV', 'VVVHratio', 'VHVVratio',
       'VV-VHnorm']].mean()

Gamma0mean.head(10) # view the first 10 rows of dataframe

# Sort the date frame in ascending order of PlotID
Gamma0mean = Gamma0mean.sort_values(by=['PlotID'], ascending=[True])
Gamma0mean.to_csv("Bok_Gamma0mean.csv")
#Gamma0mean.to_csv("Bok_Gamma0sum.csv")

print(Bok_Gapfraction.columns)

# Summarize Gap Fractions dataframe by the mean in each Plot

Bok_GFmean = Bok_Gapfraction.groupby(['PlotID','Year'])[['no_of_gaps', 'gap_area', 'gap_fraction']].std()

Bok_GFmean.head(10)

Bok_GFmean = Bok_GFmean.sort_values(by=['PlotID'], ascending=[True])
Bok_GFmean.to_csv("Bok_GFstd.csv")

# Merge two dataframes: SAR Gamma0 mean intensity with Gap Fracation for each plot (means) 
# One-to-one merge using PlotId in both dataframes

Bok_Gamma0GF = pd.merge(left=Gamma0mean, right=Bok_GFmean, left_on='PlotID', right_on='PlotID') 

print(Bok_Gamma0GF.columns)

corr = Bok_Gamma0GF.corr()
sns.heatmap(corr,
            xticklabels=corr.columns.values,
            yticklabels=corr.columns.values)


plt.matshow(Bok_Gamma0GF.corr())
plt.show()

# To visualize the correlation matrix using pandas styling option

df = pd.DataFrame(Bok_Gamma0GF)
correl = df.corr()
# limit the correlation value to a precision of digits
correl.style.background_gradient(cmap='coolwarm').set_precision(2)

#BACoa = Coa2017.groupby(['Origin','PlotID2','AgeG'])[['Total BA']].var()

# To show the cluster relationships, use the clustermap of seeaborn
correlate = df.corr().mul(100).astype(int)
sns.clustermap(data=correlate, annot=True, fmt='d', cmap="Greens").savefig('Gamma0meanGFstd.pdf', dpi=300)
sns.clustermap(data=correlate, annot=True, fmt='d', cmap="Greens").savefig('Gamma0meanFFstd.tiff', dpi=300)
sns.clustermap(data=correlate, annot=True, fmt='d', cmap="Greens").savefig('Gamma0meanGFstd.jpg', dpi=300)


#Bok_GmGFs = pd.read_csv('Bokito_Gamma0mean_GFsum.csv')
Bok_GmGFs = pd.read_csv('Bokito_Gamma0mean_GFstd.csv')


print(Bok_GmGFs.columns)



plt.style.use ('seaborn-whitegrid')

colors = Bok_GmGFs['VV-VH']

plt.scatter(Bok_GmGFs['gap_fraction'], Bok_GmGFs['VH-VVnorm'], c=colors, alpha=0.3, cmap='viridis')
plt.ylabel("Normalized VH-VV Backscatter (Gamma0 dB)")
plt.xlabel("Canopy Gap Fraction")
plt.colorbar();
plt.savefig("Correlation_VH-VVNormVsGapFraction.tiff", dpi=300)
plt.savefig("Correlation_VH-VVNormVsGapFraction.pdf", dpi=300)
#plt.legend()

Bok_GmGFs2 = Bok_GmGFs.drop('Year', 1)
Bok_GmGFs2 = Bok_GmGFs2.drop(Bok_GmGFs2.columns[[0, 1]], axis=1)

Bok_GmGFs2 = pd.DataFrame(Bok_GmGFs2)
pd.scatter_matrix(Bok_GmGFs2, alpha=0.2, figsize=(10, 10), diagonal='kde')
plt.show
plt.savefig("Scatter_Gamma0_Bands_GapFraction.tiff", dpi=300)
plt.savefig("Scatter_Gamma0_Bands_GapFraction.pdf", dpi=300)


pp = sns.pairplot(data = Bok_GmGFs,
                  y_vars =['gap_fraction'],
                  x_vars = ['VH-VVnorm','VVVHratio','VV-VH'])
plt.savefig("GapFraction_PairPlot_meanGamma0GFstd.tiff", dpi=300)
plt.savefig("GapFraction_PairPlot_meanGamma0GFstd.pdf", dpi=300)

Bok_GmGFs.describe() # ger summary statistics of each variable in Bok_GmGFs

'''
PlotID', 'SARdate', 'VHgamma0', 'VVgamma0', 'VHdb', 'VVdb', 'VV-VH',
       'VH-VVnorm', 'VH-VV', 'VVVHratio', 'VHVVratio', 'VV-VHnorm', 'Year',
       'no_of_gaps', 'gap_area', 'gap_fraction', 'Ptype'],
'''


BokEfoul_GapF = pd.read_csv('GapFraction_BokitoEfoulan_2016_17.csv')

BokEfoul_GapF.describe()
print(BokEfoul_GapF.columns)

BokEoul_GFmean = BokEfoul_GapF.groupby(['Site','PlotID','Year'])[['no_of_gaps', 'gap_area', 'gap_fraction']].mean()
BokEoul_GFmean = BokEoul_GFmean.sort_values(by=['PlotID'], ascending=[True])
BokEoul_GFmean.to_csv('BokitoEoulan_meanGapFraction.csv')

BokEoul_GFvariance = BokEfoul_GapF.groupby(['Site','PlotID','Year'])[['no_of_gaps', 'gap_area', 'gap_fraction']].std()
BokEoul_GFvariance.to_csv('BokitoEoulan_varianceGapFraction.csv')


Mean_GapF = pd.read_csv('BokitoEoulan_meanGapFraction.csv')

print(Mean_GapF.columns)


sns.distplot(Mean_GapF['gap_fraction'])

sns.distplot(Mean_GapF['gap_fraction'], hist = False, bins = 10 )

sns.catplot(y ='gap_fraction', x = 'Site', kind = "box", data = Mean_GapF)




BokEfoul_Gamma0 = pd.read_csv('BokitoEfoulan_SAR_Gamma0_extracted.csv')
print(BokEfoul_Gamma0.columns)

BokEfoul_SARmean = BokEfoul_Gamma0_pixels.groupby(['Site', 'PlotID'])[['Gamma0VH',
       'Gamma0VV', 'Gamma0VHdb', 'Gamma0VVdb', 'Gamma0VV-VH', 'VH-VVonVH+VV',
       'VH-VV', 'VVVHratio', 'VHVVratio', 'VV-VHonVV+VH']].mean()

BokEfoul_SARmean = BokEfoul_SARmean.sort_values(by=['PlotID'], ascending=[True])

BokEfoul_SARmean.to_csv('BokEfoul_SAR_meanGamma0.csv')

BokEfoul_Gamma0GF = pd.merge(left=BokEfoul_SARmean, right=BokEoul_GFmean, left_on='PlotID', right_on='PlotID') 

print(BokEfoul_Gamma0GF.columns)

BokEfoul_Gamma0GF.to_csv('BokEfoul_Gamma0GF.csv')

BokEfoul_Gamma0GFmean = pd.read_csv('BokEfoul_Gamma0GF.csv')

print(BokEfoul_Gamma0GFmean.columns)

BokEfoul_Gamma0GF.to_csv('BokEfoul_Gamma0GF.csv')

BokEfoul_Gamma0GFmean = pd.read_csv('BokEfoul_Gamma0GF.csv')

print(BokEfoul_Gamma0GFmean.columns)

df = BokEfoul_Gamma0GFmean.copy()

df = df.drop(['no_of_gaps', 'gap_area'],axis=1)

print(df.columns)

print(df.info())


from sklearn.preprocessing import MinMaxScaler
import xgboost as xgb
from sklearn.metrics import accuracy_score

import tensorflow as tf

x_SAR = BokEfoul_Gamma0GFmean[:, 1:11]
y_SAR = BokEfoul_Gamma0GFmean[:, -1]




# SUMMARY OF SAR BACKSCATTER FROM EXTRACTED PIXELS IN BOTH STUDY SITES

# Import Gap Fraction data
BokEoul_GFmean = pd.read_csv('BokitoEoulan_meanGapFraction.csv')
print(BokEoul_GFmean.columns)


BokEfoul_Gamma0_pixels = pd.read_csv('BokitoEfoulan_SAR_Gamma0_extractedpixels.csv')
print(BokEfoul_Gamma0_pixels.columns)

BokEfoul_pixSARmean = BokEfoul_Gamma0_pixels.groupby(['Site', 'PlotID'])[['Gamma0VH',
       'Gamma0VV', 'Gamma0VHdb', 'Gamma0VVdb', 'Gamma0VV-VH', 'VH-VVonVH+VV',
       'VH-VV', 'VVVHratio', 'VHVVratio', 'VV-VHonVV+VH']].mean()


BokEfoul_pixSARmean = BokEfoul_pixSARmean.sort_values(by=['PlotID'], ascending=[True])

BokEfoul_pixSARmean.to_csv('BokEfoul_SAR_meanGamma0pixels.csv')


BokEfoul_Gamma0GFpixels = pd.merge(left=BokEfoul_pixSARmean, right=BokEoul_GFmean, left_on='PlotID', right_on='PlotID') 

print(BokEfoul_Gamma0GFpixels.columns)

BokEfoul_Gamma0GFpixels.to_csv('BokEfoul_Gamma0GFpixels.csv')



BokEfoul_Gamma0GFpixels = pd.read_csv('BokEfoul_Gamma0GFpixels.csv')
BokEfoul_Gamma0GFpixels = BokEfoul_Gamma0GFpixels.drop(['Unnamed: 0'],axis=1)
BokEfoul_Gamma0GFpixels.to_csv('BokEfoul_Gamma0GFpixels.csv')



BokEfoul_Gamma0GFpixels = pd.read_csv('BokEfoul_Gamma0GFpixels.csv')

print(BokEfoul_Gamma0GFpixels.columns)

dfPix = BokEfoul_Gamma0GFpixels.copy()

dfPix2 = dfPix.drop(['PlotID','Site', 'Year','no_of_gaps', 'gap_area'],axis=1)

print(dfPix2.columns)

print(dfPix2.info())

dfPix2_stats = dfPix2.describe() # Get summary statistice of dataframe
print(dfPix2_stats.shape)

#df2_stats.pop('gap_fraction')
dfPix2_stats = dfPix2_stats.transpose()
dfPix2_stats

dfPix2_stats.to_csv('Summary_PixelGamm0.csv')


sns.pairplot(dfPix[['Gamma0VH', 'Gamma0VV', 'Gamma0VHdb', 'Gamma0VVdb',
       'Gamma0VV-VH', 'VH-VVonVH+VV', 'VH-VV', 'VVVHratio', 'VHVVratio',
       'VV-VHonVV+VH', 'gap_fraction']], diag_kind="kde")





Pixeldf = pd.read_csv('BokEfoul_Gamma0GFpixels_Luse.csv')


# PLOT BOXPLOT OF SIGNIFICANT STEPWISE REGRESSION PREDICTORS

names = ['Gamma0VV', "Gamma0VVdb", "VVVHratio", 'VHVVratio']
fig, axes = plt.subplots(1,4, constrained_layout=True)
#plt.subplots_adjust(hspace = 3.8)
sns.set_style("whitegrid")

for i, t in enumerate(names):
    sns.boxplot(y = t, x = "luse", data = Pixeldf, ax = axes[i % 4])
    #set_xticklabels(rotation =45)

plt.savefig("Boxplot_StepRegressPredictorsPixel.tiff", dpi=300)
plt.savefig("Boxplot_StepRegressPredictorsPixel.pdf", dpi=300)   
plt.savefig("Boxplot_StepRegressPredictorsPixel.eps", dpi=300)     

