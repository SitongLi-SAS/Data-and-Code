# -*- coding: utf-8 -*-
"""
Created on Sat Jan 22 13:34:27 2022

@author: rory
"""



import pandas as pd
import numpy as np
from minepy import MINE


def binning(df,x,y,bin_size,way):
    """
    Input：
    x:需要被离散化的变量[n,] or [n,1]
    y：如果采用有监督的离散化需要的标签值
    bin_size:离散化后的份数
    way：离散化的方法；无监督的有等频，等距，聚类，有监督的有chimerge，tmerge等
    Output：{分箱信息，分箱后结果}
    分箱信息：{分箱值的dict}
    分箱后结果：[n,1]
    """
    #pass
    setv = {x for x in set(df[x].values.flatten()) if x==x}
    set_len = len(setv) 
    if set_len<=bin_size:
        setv.add(-1)
        return setv,df[x]
    
    x_bin,bins = pd.qcut(df[x],bin_size,duplicates = 'drop',retbins = True)
    return bins,x_bin






def cross_score(x1,x2,y,bin_size,way):
    """
    Input：
    x1:第一个变量
    x2:第二个变量
    y:标签变量
    bin_size:离散化后的份数
    way：离散化的方法；无监督的有等频，等距，有监督的有chimerge，tmerge等
    Output：
    score:一个用于评价x1和x2是否对y有交叉影响的分数
    """
    df_n = df[[x1,x2,y]]
    bin1,x1_bin = binning(df_n,x1,y,bin_size,way)
    bin2,x2_bin = binning(df_n,x2,y,bin_size,way)
    bin_size1 = len(bin1)-1
    bin_size2 = len(bin2)-1
    df_n['x1_bin'] = x1_bin
    df_n['x2_bin'] = x2_bin
    df_result = df_n.groupby(['x1_bin','x2_bin']).agg(['mean','count',np.std])[y]
    df_1 = df_n.groupby(['x1_bin']).agg(['mean','count',np.std])[y]
    df_2 = df_n.groupby(['x2_bin']).agg(['mean','count',np.std])[y]
    score1 = 0
    sample_n1 = df_1['count'].sum()
    sample_n2 = df_2['count'].sum()
    for x1_b in df_1.index:
        score1 += (df_1.loc[x1_b]['count']/sample_n1)*rowscore(df_result.loc[(x1_b,slice(None)),:],df_1.loc[x1_b])       
    score2 = 0
    for x2_b in df_2.index:
        score2 += (df_2.loc[x2_b]['count']/sample_n2)*rowscore(df_result.loc[(slice(None),x2_b),:],df_2.loc[x2_b])
    return score1/np.sqrt(bin_size1),score2/np.sqrt(bin_size2)   


def rowscore(row1,row2):
    score = 0
    x2,n2,s2 = row2['mean'],row2['count'],row2['std']

    for index, row in row1.iterrows():
        x1,n1,s1 = row['mean'],row['count'],row['std']
        #t统计量
        #score += (n1/n2)*np.divide(np.abs(x1-x2),np.sqrt((1/n1+1/n2)*((n1-1)*s1*s1+(n2-1)*s2*s2)/(n1+n2-2)))
        #
        #print(np.divide(np.abs(x1-x2),np.sqrt(s1*s2)))
        if n1<10:
            continue
        score += (n1/n2)*np.divide(np.abs(x1-x2),np.sqrt(s1*s2))
        
    return score  

def InfoGain(x1,x2,y,bin_size,bin_way=None,y_type='C',adjust = 'True'):
    """
    Input：
    x1:第一个变量
    x2:第二个变量
    y:标签变量
    bin_size:离散化后的份数
    bin_way：离散化的方法；无监督的有等频，等距，有监督的有chimerge，tmerge等
    y_type: 'C' or 'D', continous or discete
    Output：
    RSC:一个用于评价x1和x2是否对y有交叉影响的分数
    """
    assert y_type =='C' or y_type=='D', 'y_type should be C or D for continous y and discrete y'
    df_n = pd.DataFrame(np.array([x1,x2,y]).T,columns = ['x1','x2','y'])
    bin1,x1_bin = binning(df_n,'x1','y',bin_size,bin_way)
    bin2,x2_bin = binning(df_n,'x2','y',bin_size,bin_way)
    bin_size1 = len(bin1)-1
    bin_size2 = len(bin2)-1
    df_n['x1_bin'] = x1_bin
    df_n['x2_bin'] = x2_bin
    if y_type == 'C' :
        df_result = df_n.groupby(['x1_bin','x2_bin']).agg(['mean','count',np.std])['y']
        df_1 = df_n.groupby(['x1_bin']).agg(['mean','count',np.std])['y']
        df_2 = df_n.groupby(['x2_bin']).agg(['mean','count',np.std])['y']
        score1 = 0
        sample_n1 = df_1['count'].sum()
        sample_n2 = df_2['count'].sum()
        for x1_b in df_1.index:
            score1 += (df_1.loc[x1_b]['count']/sample_n1)*rowscore(df_result.loc[(x1_b,slice(None)),:],df_1.loc[x1_b])       
        score2 = 0
        for x2_b in df_2.index:
            score2 += (df_2.loc[x2_b]['count']/sample_n2)*rowscore(df_result.loc[(slice(None),x2_b),:],df_2.loc[x2_b])
        if adjust =='False':
            return score1,score2
        else:
            return score1/np.sqrt(bin_size1),score2/np.sqrt(bin_size2)  
    else:
        pass

        
    