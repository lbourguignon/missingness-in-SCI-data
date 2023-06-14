################################################################################
# SCI - Handling missing data project
# L. Bourguignon & L.P. Lukas
# First version : 04.03.2022
# Last update : 04.03.2022
# ------------------------------------------------------------------------------
# Compute metrcis for mice imputations
################################################################################

################################################################################
# Packages and seed
################################################################################

import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Import features for grid search cross-validation
import random
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

# Import metrics' implementations
from sklearn.metrics import mean_absolute_error
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
from sklearn.metrics import cohen_kappa_score
from sklearn.metrics import matthews_corrcoef

# Import models' implementations
from sklearn.impute import KNNImputer
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neighbors import KNeighborsRegressor
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sklearn.svm import SVC
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import RandomForestRegressor

random.seed(9)

################################################################################
# Functions
################################################################################

def plot_confusion2(y_true, y_imp):

    kappa = cohen_kappa_score(y_true, y_imp)
    m_cor = matthews_corrcoef(y_true, y_imp)

    kv = [
        ('cohen_kappa_score', kappa),
        ('matthews_corrcoef', m_cor)
    ]

    return(dict(kv))

def calculate_metrics(y_true, y_pred, prefix=None):

    rmse = mean_squared_error(y_true, y_pred, squared=False)
    mae = mean_absolute_error(y_true, y_pred)
    r2 = r2_score(y_true, y_pred)

    kv = [
        ('rmse', rmse),
        ('mae', mae),
        ('r2', r2)
    ]

    if prefix is not None:
        kv = [(f'{prefix}_{key}', value) for key, value in kv]

    return dict(kv)

################################################################################
# Main
################################################################################

if __name__ == '__main__':
    # Load the dataframe when the original data and data with missing values introduced

    parser = argparse.ArgumentParser()

    parser.add_argument(
        '-sub', '--subset',
        type=int,
        help='Number of the subset to impute',
        required=True,
    )

    parser.add_argument(
        '--scenario',
        type=str,
        help='Name of the population used for subsetting based on AIS grade proportion',
        required=True,
    )

    parser.add_argument(
        '--outcome',
        type=str,
        help='Name of the outcome variable',
        required=True,
    )

    args = parser.parse_args()

    num = args.subset
    scenario = args.scenario
    outcome = args.outcome
    path = "/cluster/scratch/blucie/NA_SCI/subsets_imputed/" + scenario + "/" + outcome + "/subset_" + str(num) + '_imputed_mice_n500.csv'
    data = pd.read_csv(path)

    # Harmonise the NAs signature to numpy format
    data = data.replace('nan', np.nan)

    for pattern in ['MCAR', 'MAR', 'MNAR']: #for each pattern of missingness
        for var in ['ais1', 'lower01', outcome]: #for each variable in which missingness was introduced
            case = var + '_' + pattern
            if var == 'ais1':
                model_name = 'polr'
                test_metrics = plot_confusion2(np.ravel(data.loc[:, data.columns == var]), np.ravel(data.loc[:, data.columns == case+'_'+model_name]))
                my_dict = {'metrics_testset': test_metrics}

                with open('/cluster/scratch/blucie/NA_SCI/metrics/' + scenario + "/" + outcome + '/subset_'+ str (num) + '_' + case + '_' + model_name + '_n500.csv', 'w') as f:
                    for key in my_dict.keys():
                        f.write("%s,%s\n"%(key,my_dict[key]))
            else:
                for model_name in ['pmm', 'rf', 'norm.predict']:
                    test_metrics = calculate_metrics(np.ravel(data.loc[:, data.columns == var]), np.ravel(data.loc[:, data.columns == case+'_'+model_name]))

                    my_dict = {'metrics_testset': test_metrics}

                    with open('/cluster/scratch/blucie/NA_SCI/metrics/' + scenario + "/" + outcome + '/subset_'+ str (num) + '_' + case + '_' + model_name + '_n500.csv', 'w') as f:
                        for key in my_dict.keys():
                            f.write("%s,%s\n"%(key,my_dict[key]))
