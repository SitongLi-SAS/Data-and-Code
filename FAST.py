import warnings
import pandas as pd
import csv  # 测试时使用
import numpy as np
'''
X1: data of the first feature, type: list
X2: data of the second feature, type: list
Y: data of the target feature, type: list
B: bins (discrete number), type: integer
'''

# 读取的csv信息仅作为读取测试数据和参数使用。数据量：单个特征17280条



def FAST(x1, x2, y, b):
    # 合并数据，计算全局参数

    merged_data = pd.DataFrame(np.array([np.reshape(x1,-1),np.reshape(x2,-1),np.reshape(y,-1)]).T,columns = ['x1','x2','y'])
    merged_data =  merged_data.dropna(axis = 0)
    total_targets = merged_data['y'].sum()
    total_weights = merged_data['y'].count()

    # 数据离散化，对x1和x2进行分箱处理，箱的数量由参数b决定
    x1_bin = pd.qcut(merged_data['x1'], b,  duplicates='drop', labels = list(range(b)))
    x2_bin = pd.qcut(merged_data['x2'], b,  duplicates='drop', labels = list(range(b)))
    merged_data['x1_bin'] = x1_bin
    merged_data['x2_bin'] = x2_bin


    # 计算所有的CH(w)和CH(t)

    temp = merged_data.loc[:,['x1_bin','y']].groupby('x1_bin').agg(['sum','count'])
    CH_i_t, CH_i_w  = temp.iloc[:,0].cumsum().values, temp.iloc[:,1].cumsum().values
    



    temp = merged_data.loc[:,['x2_bin','y']].groupby('x2_bin').agg(['sum','count'])
    CH_j_t, CH_j_w  = temp.iloc[:,0].cumsum().values, temp.iloc[:,1].cumsum().values


    # 计算所有的Hij(w)和Hij(t)
    temp = merged_data.loc[:,['x1_bin','x2_bin','y']].groupby(['x1_bin','x2_bin']).agg(['sum','count'])
    H_ij_t,H_ij_w = np.reshape(temp.iloc[:,0].values,(b,b)),np.reshape(temp.iloc[:,1].values,(b,b))
    

    # 建立查找表Lt和Lw
    temp_sum_t, temp_sum_w = 0,0
    temp_a_t = np.zeros([b-1,b-1])
    temp_a_w = np.zeros([b-1,b-1])
    Lt = np.zeros([b-1,b-1,4])
    Lw = np.zeros([b-1,b-1,4])
    for q in range(b-1):
        temp_sum_t = temp_sum_t + H_ij_t[0][q]
        temp_sum_w = temp_sum_w + H_ij_w[0][q]
        temp_a_t[0][q] = temp_sum_t
        temp_a_w[0][q] = temp_sum_w
        Lt[0][q][0],Lt[0][q][1],Lt[0][q][2],Lt[0][q][3] = temp_a_t[0][q], CH_i_t[0]-temp_a_t[0][q], CH_j_t[q]-temp_a_t[0][q], total_targets-CH_i_t[0]-CH_j_t[q]+temp_a_t[0][q]
        Lw[0][q][0],Lw[0][q][1],Lw[0][q][2],Lw[0][q][3] = temp_a_w[0][q], CH_i_w[0]-temp_a_w[0][q], CH_j_w[q]-temp_a_w[0][q], total_weights-CH_i_w[0]-CH_j_w[q]+temp_a_w[0][q]
        
    for p in range(1,b-1):
        temp_sum_t, temp_sum_w = 0,0
        for q in range(b-1):
            temp_sum_t = temp_sum_t + H_ij_t[p][q]
            temp_sum_w = temp_sum_w + H_ij_w[p][q]
            temp_a_t[p][q] = temp_sum_t + temp_a_t[p-1][q]
            temp_a_w[p][q] = temp_sum_w + temp_a_w[p-1][q]
            Lt[p][q][0],Lt[p][q][1],Lt[p][q][2],Lt[p][q][3] = temp_a_t[p][q], CH_i_t[p]-temp_a_t[p][q], CH_j_t[q]-temp_a_t[p][q], total_targets-CH_i_t[p]-CH_j_t[q]+temp_a_t[p][q]
            Lw[p][q][0],Lw[p][q][1],Lw[p][q][2],Lw[p][q][3] = temp_a_w[p][q] , CH_i_w[p]-temp_a_w[p][q] , CH_j_w[q]-temp_a_w[p][q] , total_weights-CH_i_w[p]-CH_j_w[q]+temp_a_w[p][q] 
            
    RSS = np.zeros([b-1,b-1])
    for p in range(b-1):
        for q in range(b-1):
            RSS[p][q] = np.sum(Lt[p][q]**2/Lw[p][q])
            
    return np.max(RSS)/total_weights



x1 = np.random.rand(10000)
x2 = np.random.rand(10000)
noise = np.random.rand(10000) 
noi = 0
y1 = np.sin(x1*2*np.pi)+np.cos(x2*2*np.pi)+noi*noise
y2 = np.sin(x1*2*np.pi+x2*2*np.pi)
FAST(x1,x2,y2,6)

y = [1 if i<0.25 and j<0.75 else 0 for i,j in zip(x1,x2)]

FAST(x1,x2,y,4)


function_name = ['Mountain','Wave','Plate','Crater','Ellipsoidal cap','Rand\Line','X','Sin\Cos']
fig = plt.figure(figsize=plt.figaspect(0.5))
noi = 0
for y_id in range(1,9):
    #ax = fig.add_subplot(2, 4, 1, projection='3d')
    i = y_id-1
    #plt.sca(ax[i//4][i%4])
    # ax = figure.gca(projection='3d')
    if y_id == 1:
        Y = np.sin(X1*2*np.pi)+np.cos(X2*2*np.pi)+noi*noise
    elif y_id == 2:
        Y = np.sin(X1*2*np.pi+X2*2*np.pi)+noi*noise
    elif y_id == 3:
        Y= X1+X2+noi*noise
    elif y_id == 4:
        Y = np.sin(2*np.pi*(np.sqrt((X1-0.5) ** 2 + (X2-0.5) ** 2)))+noi*noise
    elif y_id == 5:
        Y = [np.sqrt((0.16-(x1-0.5)**2-(x2-0.5)**2))+0.5 if (x1-0.5)**2+(x2-0.5)**2<0.16 else 0.66-np.sqrt((x1-0.5)**2+(x2-0.5)**2) for x1,x2 in zip(X1.flatten(),X2.flatten())]
        Y = np.reshape(Y,X1.shape)
    elif y_id == 6:
        x1 = np.random.rand(600)
        y1 = np.random.rand(600)
        x2 = np.random.rand(1000)
        y2 = x2
        axx = fig.add_subplot(2, 4, y_id)
        p1 = axx.plot(x1,y1,'.g',label="$X_{2}=0$",markersize=20)
        p2 = axx.plot(x2,y2,'.r',label="$X_{2}=1$",markersize=20)
        axx.set_title(function_name[y_id-1])
        axx.set_xlabel('X1')
        axx.set_ylabel('Y')
        axx.legend(['$X_{2}=0$','$X_{2}=1$'])
        axx.set_xlim(0,1)
        axx.set_ylim(0,1)




