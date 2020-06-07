import numpy as np
import matplotlib.pyplot as plt
import copy
from sklearn.datasets import make_moons
from sklearn.datasets.samples_generator import make_blobs
import random
import time
import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()
from rpy2.robjects.packages import importr
importr("robustbase")
mclust = importr("mclust")
"""
from rpy2.robjects.packages import importr
utils = importr('utils')
utils.install_packages('robustbase')
"""
from rpy2 import robjects
r = robjects.r

r.source("/Volumes/PXWIN/datadepth/DepthbasedClustering/RMD.r")





class OPTICS():
    def __init__(self, epsilon, MinPts):
        self.epsilon = epsilon
        self.MinPts = MinPts

    # depth function (delete)
    def depth(self, X):
        depth_mat = r.RMD(X)
        return np.array(depth_mat)
  
    def getCoreObjectSet(self, X):
        N = X.shape[0]
        Depth = np.eye(N) * 9999999
        CoreObjectIndex = []
        Depth = self.depth(X)

        for i in range(N):

            depth = Depth[i]
            num = depth[depth > self.epsilon].shape[0] # eosilon is the minimum
            if num >= self.MinPts:
                CoreObjectIndex.append(i)
        return np.array(CoreObjectIndex), Depth
 
    def get_neighbers(self, p, Depth):
        N = []
        depth = Depth[p].reshape(-1)
        for i in range(depth.shape[0]):
            if depth[i] > self.epsilon:
                N.append(i)
        return N

    def get_core_depth(self, p, Depth):
        depth = Depth[p].reshape(-1)
        sort_depth = sorted(depth,reverse =True)
        sort_depth = np.array(sort_depth)
        return sort_depth[self.MinPts - 1]

    def resort(self): # reorder

        reachdepth = copy.deepcopy(self.ReachDepth)
        reachdepth = np.array(reachdepth)
        reachdepth = reachdepth[self.Seeds]
        new_index = np.argsort(reachdepth)
        Seeds = copy.deepcopy(self.Seeds)
        Seeds = np.array(Seeds)
        Seeds = Seeds[new_index]
        self.Seeds = Seeds.tolist()

    def update(self, N, p, Depth, D):

        for i in N:
            if i in D:
                new_reach_depth = min(self.get_core_depth(p, Depth), Depth[i][p])
                if i not in self.Seeds:
                    self.Seeds.append(i)
                    self.ReachDepth[i] = new_reach_depth
                else:
                    if new_reach_depth > self.ReachDepth[i]:
                        self.ReachDepth[i] = new_reach_depth
                self.resort()
                
    def fit(self, X):

        length = X.shape[0]
        CoreObjectIndex, Depth = self.getCoreObjectSet(X)
        self.Seeds = []
        self.Ordered = []
        D = np.arange(length).tolist()
        self.ReachDepth = [-0.1] * length

        while (len(D) != 0):
            p = random.randint(0, len(D) - 1)  
            p = D[p]
            self.Ordered.append(p)
            D.remove(p)

            if p in CoreObjectIndex:
                N = self.get_neighbers(p, Depth)
                self.update(N, p, Depth, D)

                while(len(self.Seeds) != 0):
                    q = self.Seeds.pop(0)
                    self.Ordered.append(q)
                    D.remove(q)
                    if q in CoreObjectIndex:
                        N = self.get_neighbers(q, Depth)
                        self.update(N, q, Depth, D)
        return self.Ordered, self.ReachDepth

    def plt_show(self, X, Y, ReachDepth, Ordered, name=0):
        if X.shape[1] == 2:
            fig = plt.figure(name)
            plt.subplot(211)
            plt.scatter(X[:, 0], X[:, 1], marker='o', c=Y)
            plt.subplot(212)
            ReachDepth = np.array(ReachDepth)
            plt.plot(range(len(Ordered)), ReachDepth[Ordered])
        else:
            print('error arg')
            ReachDepth = np.array(ReachDepth)
            plt.plot(range(len(Ordered)), ReachDepth[Ordered])


#if __name__ == '__main__':
    # 111111
'''   
    from sklearn import datasets
    iris=datasets.load_iris()
    #iris.data and iris.target
    optics1 = OPTICS(epsilon=0.25, MinPts=14)
    Ordered, ReachDepth = optics1.fit(iris.data)
    optics1.plt_show(iris.data, iris.target, ReachDepth, Ordered, name=1)
    plt.savefig("/Volumes/PXWIN/datadepth/DepthbasedClustering/iris.png")
    # 2
    center = [[1, 1], [-1, -1], [1, -1]]
    cluster_std = 0.35
    X1, Y1 = make_blobs(n_samples=300, centers=center,
                        n_features=2, cluster_std=cluster_std, random_state=1)
    optics1 = OPTICS(epsilon=0.85, MinPts=15)
    Ordered, ReachDist = optics1.fit(X1)
    optics1.plt_show(X1, Y1, ReachDist, Ordered, name=1)
    plt.savefig("/Volumes/PXWIN/datadepth/DepthbasedClustering/pic1.png")
    
    # 3
    
    center = [[1, 1], [-1, -1], [2, -2]]
    cluster_std = [0.35, 0.1, 0.8]
    X2, Y2 = make_blobs(n_samples=300, centers=center,
                        n_features=2, cluster_std=cluster_std, random_state=1)
    optics2 = OPTICS(epsilon=0.6, MinPts=10)
    Ordered, ReachDepth = optics2.fit(X2)
    optics2.plt_show(X2, Y2, ReachDepth, Ordered, name=2)
    plt.savefig("/Volumes/PXWIN/datadepth/DepthbasedClustering/pic2.png")
    # 33333333
    X3, Y3 = make_moons(n_samples=500, noise=0.1)
    optics3 = OPTICS(epsilon=0.85, MinPts=15)
    Ordered, ReachDist = optics3.fit(X3)
    optics3.plt_show(X3, Y3, ReachDist, Ordered, name=3)
    plt.savefig("/Volumes/PXWIN/datadepth/DepthbasedClustering/pic3.png")
'''
#flea = data(mclust).fetch('flea')['flea']
from rpy2 import robjects
from rpy2.robjects import Formula, Environment
from rpy2.robjects.vectors import IntVector, FloatVector
from rpy2.robjects.lib import grid
from rpy2.robjects.packages import importr, data
from rpy2.rinterface import RRuntimeError
import warnings

# The R 'print' function
rprint = robjects.globalenv.get("print")
stats = importr('stats')
grdevices = importr('grDevices')
base = importr('base')
datasets = importr('datasets')

grid.activate()
import math, datetime
import rpy2.robjects.lib.ggplot2 as ggplot2
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
base = importr('base')

mtcars = data(mclust).fetch('banknote')['banknote']
