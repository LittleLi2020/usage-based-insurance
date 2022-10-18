#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import time
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.neighbors import KernelDensity
from sklearn.decomposition import PCA


# In[2]:


def timestamp2date(timestamp):
    """Convert timestamp to datetime."""
    timeArray = time.localtime(timestamp)
    formatted_time = time.strftime("%Y-%m-%d %H:%M:%S", timeArray)
    return formatted_time
def get_tgAcceleration(df):
    """Calculate tangential acceleration."""
    dVehSpdLgtA = df["VehSpdLgtA"].diff() / 3.6
    dt = df["t"].diff()
    df["tg_acceleration"] = dVehSpdLgtA / dt
    return df
def kde2D(x, y, bandwidth, xbins=10j, ybins=10j, **kwargs): 
    """Build 2D kernel density estimate (KDE)."""
    
    # create grid of sample locations (default: 10x10)
    xx, yy = np.mgrid[x.min():x.max():xbins, 
                      y.min():y.max():ybins]

    xy_sample = np.vstack([yy.ravel(), xx.ravel()]).T
    xy_train  = np.vstack([y, x]).T

    kde_skl = KernelDensity(bandwidth=bandwidth, **kwargs)
    kde_skl.fit(xy_train)

    # score_samples() returns the log-likelihood of the samples
    z = np.exp(kde_skl.score_samples(xy_sample))
    zz = np.reshape(z, xx.shape)
    z_norm = z / z.sum()
    z_norm= z_norm.ravel()
    return xx,yy,zz,z_norm


# In[3]:


# read data
df = pd.read_csv(r"D:\Research\UBI车险\data\486_wuhan_test - LT.csv")
df[3716:3725]


# In[4]:


# convert timestamp
df["date"] = df["t"].apply(timestamp2date)


# In[5]:


# convert m/s to km/h
df["VehSpdLgtA"] = df["VehSpdLgtA"] * 3.6
df = df[["t","date","VehSpdLgtA"]]
df[3716:3725]


# In[6]:


# drop reduplicative records
df = df.drop_duplicates(subset = ["t"]).sort_values(by = ["t"]).reset_index(drop = True)


# In[7]:


# partition journey
df["dt"] = df["t"].diff()
df["stop"] = df["dt"] != 1
df["journeyID"] = df["stop"].cumsum()
df.loc[df["dt"] != 1].head(5)


# In[8]:


# interpolate speed
df["VehSpdLgtA"] = df.groupby("journeyID")["VehSpdLgtA"].apply(lambda v: v.interpolate(limit_direction = "both"))
df["VehSpdLgtA"] = df["VehSpdLgtA"].fillna(0)
df.isnull().sum()


# In[9]:


# calculate tangential acceleration
df = df.groupby("journeyID").apply(get_tgAcceleration)


# In[10]:


# omit the first row of every journey (beacause of NaN of tg_acceleration)
df = df.groupby("journeyID").apply(lambda x:x.iloc[1:]).reset_index(drop = True)


# In[11]:


# omit meaningless journeies(v == 0 or cumsum(t) less than 3min)
df["is_meaningless"] = df.groupby("journeyID")["VehSpdLgtA"].transform(lambda v: True if (np.sum(v) == 0 or len(v) < 3 * 60) else False)
df = df[~df["is_meaningless"]].reset_index(drop = True)
df = df[["t","date","journeyID","VehSpdLgtA","tg_acceleration"]]
df[3432 - 5:3432 + 5]


# In[12]:


# kernel density analysis
results = df.groupby("journeyID").apply(lambda x:kde2D(x["VehSpdLgtA"],x["tg_acceleration"],bandwidth = 0.75))
results = [{"journeyID":index, "z_norm":results[index][3]} for index in results.index]
z_norms = pd.DataFrame(results)
z_norms = pd.DataFrame(z_norms["z_norm"].to_list(),columns = ["z_norm" + str(i) for i in range(1,101)])
mean = z_norms.mean(axis = 0)
std = z_norms.std(axis = 0)
z_norms = (z_norms - mean)/std
z_norms 


# In[13]:


# principal components analysis
pca = PCA(n_components = 5)
pca.fit(z_norms)
print("explained_variance_ratio_: ",pca.explained_variance_ratio_)
print("cummulative explained_variance_ratio_: ",np.cumsum(pca.explained_variance_ratio_))
pca_score = np.dot(z_norms,pca.components_.T)
pca_score


# In[18]:


# speed visualize
df_vis = df[df["journeyID"] == 37]
df_vis["VehSpdLgtA"].plot()


# In[19]:


# speed-acceleration visualize
ax_1 = plt.scatter(df_vis["VehSpdLgtA"],df_vis["tg_acceleration"],s = 10)
plt.xlabel("VehSpdLgtA")
plt.ylabel("tg_acceleration")


# In[20]:


# kernel density visualize
x,y = df_vis["VehSpdLgtA"],df_vis["tg_acceleration"]
xx,yy,zz,z_norm = kde2D(x,y,bandwidth = 2)
plt.pcolormesh(xx, yy, zz)
plt.colorbar()
plt.scatter(x, y, s=2, facecolor='white')
plt.xlabel("VehSpdLgtA")
plt.ylabel("tg_acceleration")


# In[21]:


# principal components visualize
journeyID_num = len([result["journeyID"] for result in results])
labels = [str(result["journeyID"]) for result in results]
X = np.arange(journeyID_num) + 1

first_PCA_score = (pca_score[:,0] - pca_score[:,0].min()) / (pca_score[:,0].max() - pca_score[:,0].min())
second_PCA_score = (pca_score[:,1] - pca_score[:,1].min()) / (pca_score[:,1].max() - pca_score[:,1].min())

plt.bar(X,first_PCA_score,width = 0.35,alpha = 0.9,facecolor = "red",label = "First")
plt.bar(X + 0.35,second_PCA_score,width = 0.35,alpha = 0.9, facecolor = "blue",label = "Second")
plt.xlabel("Journey ID")
plt.ylabel("PCA Score")
plt.xticks(X,labels)
plt.legend()
plt.show()

