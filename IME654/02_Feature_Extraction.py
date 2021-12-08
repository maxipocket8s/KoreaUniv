# -*- coding: utf-8 -*-
"""
Created on Wed Dec  8 18:54:06 2021

@author: Juwon
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import offsetbox
from sklearn import (manifold, datasets, decomposition, ensemble, discriminant_analysis, random_projection)

digits = datasets.load_digits(n_class=6)
X = digits.data
y = digits.target
n_samples, n_features = X.shape
n_neighbors = 30

def plot_embedding(X, title=None):
    x_min, x_max = np.min(X, 0), np.max(X, 0)
    X = (X-x_min)/(x_max-x_min)
    plt.figure()
    ax = plt.subplot(111)
    for i in range(X.shape[0]):
        plt.text(X[i, 0], X[i, 1], str(y[i]), color=plt.cm.Set1(y[i]/10.), fontdict={'weight':'bold', 'size':9})
    if hasattr(offsetbox, 'AnnotationBbox'):
        shown_images = np.array([[1., 1.]])
        for i in range(X.shape[0]):
            dist = np.sum((X[i]-shown_images) ** 2, 1)
            if np.min(dist) < 4e-3:
                continue
            shown_images = np.r_[shown_images, [X[i]]]
            imagebox = offsetbox.AnnotationBbox(offsetbox.OffsetImage(digits.images[i], cmap=plt.cm.gray_r), X[i])
            ax.add_artist(imagebox)
    plt.xticks([]), plt.yticks([])
    if title is not None:
        plt.title(title)
        
X_iso = manifold.Isomap(n_neighbors, n_components=2).fit_transform(X)
plot_embedding(X_iso, 'Isomap projection')

X_lle = manifold.LocallyLinearEmbedding(n_neighbors, n_components = 2, method = 'standard').fit_transform(X)
plot_embedding(X_lle, 'Locally Linear Embedding of the digits')

X_tsne = manifold.TSNE(n_components = 2, init = 'pca', random_state = 0).fit_transform(X)
plot_embedding(X_tsne, 't-SNE Embedding of the digits')