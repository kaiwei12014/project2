import os
import glob
import argparse

import numpy as np
import scipy as sp
import pandas as pd
import sklearn.ensemble
from sklearn import cross_validation

from src.features import *
from src.utils import *

def run(train_dir, test_dir, **kwargs):
    """Return a header and data array of a csv file.

    Args:
        filename (string): Complete file path including name of file
        header (boolean): Indicates the presence of data column labels
    Returns:
        header (array): Labels to the output data columns
        data (array): Delimited list of data
    """
    cached_input = kwargs.get('cached_input', False)
    models_dir = kwargs.get('models_dir', 'models/')
    output_dir = kwargs.get('output_dir', 'data/output/')
    
    # Set the randomizer seed so results are the same each time.
    np.random.seed(0)

    # load some files
    train_labels = pd.read_csv('data/raw/TrainLabels.csv')
    submission = pd.read_csv('data/output/SampleSubmission.csv')

    # load data
    train_data = []
    test_data = []

    # test data assumptions?

    if cached_input:
        print("Using cached data...")
        train_data = pd.read_pickle('data/input/train_data_eog_artifact_removed.pkl')
        test_data = pd.read_pickle('data/input/test_data_eog_artifact_removed.pkl')
    else:
        # build list of training and testing csv(s)

        # build dataframe for training
        for i, f in enumerate(training_files):
            # extract and data transformations + feature engineering
            pass

        # build dataframe for testing
        for i, f in enumerate(testing_files):
            # extract and data transformations + feature engineering
            pass

        train_data = pd.DataFrame(train_data)
        test_data = pd.DataFrame(test_data)

        # cache input locally
        train_data.to_pickle('data/input/train_data_eog_artifact_removed.pkl')
        test_data.to_pickle('data/input/test_data_eog_artifact_removed.pkl')

    # Test features are uniform and standardized

    # DO feature selection (decision tree for classification?)

    """
    models = ['some_model']
    model_performance = {}

    for i, model in enumerate(models):
        from model import *

        # train model
        # optimize parameters for optimal model 
        # return optimal parameters and performance
        # save optimal model and model output into wip

    if ensemble == 'bag':
        pass
    elif ensemble == 'boost':
        pass
    else:
        # reutnr max performance model
        # delete all wip data and other models
        pass
    """
    # Test output values make sense
    # Test submission file matches up

    # Log and serialize
    gbm = sklearn.ensemble.GradientBoostingClassifier(n_estimators=500,learning_rate=0.05, max_features=0.25)
    gbm.fit(train_data, train_labels.values[:,1].ravel())
    scores = cross_validation.cross_val_score(gbm, train_data, train_labels.Prediction.values, cv=10)
    print("Accuracy: {0} (+/- {1})".format(scores.mean(), scores.std() / 2))

    preds = gbm.predict_proba(test_data)[:,1]
    submission['Prediction'] = preds
    submission.to_csv('data/output/gbm_artifact_removed.csv',index=False)


if __name__ == '__main__':
    # Parse command line parameters
    parser = argparse.ArgumentParser(description='Train models locally for Kaggle INRIA Challene')
    parser.add_argument('--train', required=True, help="Name of directory for training csv(s).")
    parser.add_argument('--test', required=True, help="Name of directory for testing csv(s).")
    parser.add_argument('--cached-input', default=False, help="Name of directory where input matrix is cachedd.")
    parser.add_argument('--models-dir', default='models/', help="Name of directory to save output models.")
    parser.add_argument('--output-dir', default='data/output/', help="Name of directory for saving output csv(s).")
    args = parser.parse_args()

    # python main.py --train='data/raw/train/' --test='data/raw/test/' --cached-input=True
    results = run(args.train, args.test, cached_input=args.cached_input, models_dir=args.models_dir, output_dir=args.output_dir)
