# -*- coding: utf-8 -*-
"""
Created on Wed Feb  2 17:35:51 2022

@author: rory
"""
import pandas as pd
import numpy as np
from minepy import MINE
import seaborn as sns  
from matplotlib import pyplot as plt
import warnings
from RSC import *
df = pd.read_csv('wdidataset_selected.csv',index_col = 0)
col_name = df.columns
col_len = len(df.columns)
#--------MIC calculate I--------
micdict = {}
mine = MINE(alpha=0.6, c=15)
for i in range(col_len-1):
    mine.compute_score(df.iloc[:,i],df.iloc[:,-1])
    micdict[col_name[i]] = mine.mic()
i1 = micdict['Fertilizer consumption (kilograms per hectare of arable land)(2018)']
i2 = micdict['Strength of legal rights index (0=weak to 12=strong)(2018)']
#-----------------Calculate InfoGain
x1 = np.array(df.loc[:,'Fertilizer consumption (kilograms per hectare of arable land)(2018)']).flatten()
x2 = np.array(df.loc[:,'Strength of legal rights index (0=weak to 12=strong)(2018)']).flatten()
y =np.array(df.iloc[:,-1]).flatten()
g2,g1 =InfoGain(x1,x2,y,2,adjust = 'False')


#--------------

print('I1,I2,G1,G2 is ',i1,i2,g1,g2)
print('RSC IS ', max(g1/i1,g2/i2))
