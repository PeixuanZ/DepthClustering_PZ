#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 14 20:08:14 2019

@author: emilyzhang
"""
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from sklearn import datasets
from sklearn.decomposition import PCA
import numpy as np
import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()

from rpy2.robjects.packages import importr
importr("robustbase")
"""
from rpy2.robjects.packages import importr
utils = importr('utils')
utils.install_packages('robustbase')
"""
# import some data to play with
iris = datasets.load_iris()
#def depth(x1,x2):

from rpy2 import robjects
r = robjects.r

r.source("/Volumes/PXWIN/datadepth/DepthbasedClustering/RMD.r")

#/Volumes/PXWIN/datadepth/DepthbasedClustering
    
# depth function
def depth(x1,x2):
    return r.RMD(x1,x2)

depth_mat = np.zeros(shape = (150,150))

iris2 = iris.data
depth_mat = np.array(r.RMD(iris2))
#print(r.RMD(iris2)(1,1))
"""
for i in range(iris2.shape[0]):
    for j in range(iris2.shape[0]):
        depth_mat[i,j] = depth(iris2[i,:],iris2[j,:])
"""