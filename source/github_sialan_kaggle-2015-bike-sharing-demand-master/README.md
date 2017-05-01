SAMPLE DATA PROJECT STRUCTURE
======

GENERAL
numpy, pandas
iPython, matplotlib, seaborn
scikit-learn, xgboost, statsmodels, pymc

SIGNAL PROCESSING
scipy, opencv, simplecv

NLP
gensim, nltk, textblob, textract

ONLINE LEARNING
vorpal rabbit

DEEP LEARNING
Theano, Pylearn2, Caffe

DISTRUBTED COMPUTING
MLlib (incl. H20 for Sparkling Water), MrJob

OTHER
rlpy, cvxopt


a. Use iPython, pandas, matplotlib, seaborn for data exploration.
b. scikit-learn, xgboost, statsmodels, pymc
c. better features (scikit-learn, SIGNAL PROCESSING, NLP)
d. distributed/online learning? (AWS EMR + MrJob, Apache Spark + MlLib + H20)
e. deep learning approaches (Theano, Pylearn2, Caffe)
f. ensemble and present

1. March Madness
    pymc, statsmodels, scikit-learn, xgboost

2. Genomics Sequencing Pipeline
    AWS Spark & MlLib
@UCB ii

3. Bike Share Demand Forecasting
    STATISTICAL AND PROBABILISTIC GRAPHICAL statsmodels, pymc + SIGNALPROCESSING w/scipy and opencv/simplecv
    SUPERVISED LEARNING METHODS scikit-learn, xgboost
    DEEP LEARNING METHODS Theano, Pylearn2 Caffe

4i. Multi-Task Semi-Supervised Learning For Topic-Based Sentiment Analysis
    AWS EMR MrJob and Pig
    gensim, nltk, textblob
    scikit-learn and ipython parallel or multiprocessing
@UCB i

5. A Comparison of Deep Learning for Time-Series Motion Classification
    scikit-learn,
@UofT i

6. MSFT Learning to Rank
    Vowpal Wabbit

7. Visualization POLITICAL
@UCB iii

8. Capstone SPORTS ANALYTICS
@UCB iv

Setup
------
Make sure you already have node and npm installed locally locally. Install `yo` along with dependencies and cd into desired project directory:

    npm install -g yo bower grunt-cli
    npm install -g generator-backbone jasmine
    mkdir my-yo-project
    cd my-yo-project

Create backbone scaffolding and install project dependencies:
    
    yo backbone --requirejs --test-framework=jasmine
    npm install & bower install

    yo backbone:model blog
    yo backbone:collection blog
    yo backbone:router blog
    yo backbone:view blog