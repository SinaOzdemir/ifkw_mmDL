# Do we need multi-modality in automated content analysis? An experiment with EU executive tweets

Sina Ozdemir,  
PhD candidate,  
Department of sociology and political science,  
Norwegian university of science and technology (NTNU)  
  
Patrick Schwabl,  
PhD candidate,  
Department of communication science,  
Ludwig Maximilian University (LMU)    

## Abstract:

Recent developments in the machine learning field has enabled reserachers to incorporate audio and visual materials into their analysis by using deep learning.
These methods are especially valuable as multi-modal political communication has increasingly become more ubiquitous with the advent of web 3.0 and similar technologies.
There is, however, very little empirical work evaluating how applicable and beneficial these methods are in automated content analysis in political communication research.
This study aims to fill this gap by comparing classification performances of shallow learning and deep learning algorithms using multi-modal embeddings and tabular data.
To test their performance, we classify 898 tweets from EU executives into binary category using a set of deep learning and shallow learning algorithms.
In our experiments, shallow learners with tabular data have regularly outperformed the deep learning classifier using multi-modal embedding.
Our results tell a few cautionary tales about using multi-modal representation for future researchers. First, most visual political communication have large variation.
Therefore, multi-modal automated content analysis requires large amount of manually annotated data. 
Compounded with the general data-gready nature of deep learning, it can be more reseource efficient to use shallow learners with monomodal tabular data for classification tasks.
It is also possible to use multi-modal embeddings with shallow learners such logistic regression and support vector machine. 
However, these algorithms are not capable of handling high dimensional datasets such as tensors (i.e embeddings).
With an appropriate dimension reduction method applied to a tensor, these might yield the optimum results yet there is more research needed to identify such dimension reduction methods
