
################################################################################
# SCI - Handling missing data project
# L. Bourguignon & L.P. Lukas
# First version : 19.12.2021
# Last update : 14.06.2023
# ------------------------------------------------------------------------------
# SINGLE IMPUTATION
################################################################################

################################################################################
# Packages and seed
################################################################################

import argparse
#import argsparse
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

#import warnings
#warnings.filterwarnings('ignore', 'Solver terminated early.*')

random.seed(9)

################################################################################
# Functions
################################################################################

def knn_imputer(var, k, outcome, weights='uniform'):

    imputer = KNNImputer(n_neighbors=k, weights=weights)

    if outcome in var:
        __temp__ = data[['age', 'sexcd', 'level', 'ais1', 'lower01', var]].copy()
        __temp__["ais1"].replace({"AIS A": 1, "AIS B": 2, "AIS C": 3, "AIS D": 4}, inplace=True)
    elif 'lower01' in var:
        __temp__ = data[['age', 'sexcd', 'level', 'ais1', var]].copy()
        __temp__["ais1"].replace({"AIS A": 1, "AIS B": 2, "AIS C": 3, "AIS D": 4}, inplace=True)
    elif 'ais1' in var:
        __temp__ = data[['age', 'sexcd', 'level', 'lower01', var]].copy()
        __temp__[var].replace({"AIS A": 1, "AIS B": 2, "AIS C": 3, "AIS D": 4}, inplace=True)
    else:
        print('Error in variable name.')

    __temp__["level"].replace({"cervical": 1, "thoracic": 2}, inplace=True)

    __temp__ = imputer.fit_transform(__temp__)
    __temp__ = pd.DataFrame(__temp__)

    if outcome in var:
        output = __temp__[5]
    else:
        output = __temp__[4]
        if 'ais1' in var:
            output = round(output)
            output.replace({1: "AIS A", 2: "AIS B", 3: "AIS C", 4: "AIS D"}, inplace=True)

    return (output)

def single_imputation(df, var, idx_NA, model, outcome, param_grid):
    bad_df = df.index.isin(idx_NA)

    data = df[['age', 'sexcd', 'level', 'ais1', 'lower01', outcome]].copy()
    data['level'].replace({"cervical": 1, "thoracic": 2}, inplace=True)
    data['ais1'].replace({"AIS A": 1, "AIS B": 2, "AIS C": 3, "AIS D": 4}, inplace=True)

    test = data[bad_df]
    train = data[~bad_df]

    if 'ais1' in var:
        y_train = np.ravel(train.loc[:, train.columns == 'ais1'])
        y_test = np.ravel(test.loc[:, test.columns == 'ais1'])
        data = data.drop(columns=['ais1'])
        data[var] = df[var].replace({"AIS A": 1, "AIS B": 2, "AIS C": 3, "AIS D": 4})
        df[var] = df[var].astype("category")
        scoring = 'balanced_accuracy'
        refit = 'balanced_accuracy'
    elif 'lower' in var:
        scoring = 'neg_root_mean_squared_error'
        refit = True
        data[var] = df[var]
        if 'lower01' in var:
            y_train = np.ravel(train.loc[:, train.columns == 'lower01'])
            y_test = np.ravel(test.loc[:, test.columns == 'lower01'])
            data = data.drop(columns=['lower01'])
        elif outcome in var:
            y_train = np.ravel(train.loc[:, train.columns == outcome])
            y_test = np.ravel(test.loc[:, test.columns == outcome])
            data = data.drop(columns=[outcome])

    test = data[bad_df]
    train = data[~bad_df]

    if outcome in var:
        X_train = train.drop(columns=[var])
        X_test = test.drop(columns=[var])
    else:
        X_train = train.drop(columns=[var, outcome])
        X_test = test.drop(columns=[var, outcome])

    pipeline = Pipeline(steps=[('scaler', None), ('model', model)])

    grid_search = GridSearchCV(
        pipeline,
        param_grid=param_grid,
        cv=5,
        scoring=scoring,
        n_jobs=-1,
        verbose=1,
        refit=refit
    )

    grid_search.fit(X_train.values, y_train)

    y_pred = grid_search.predict(X_test.values)

    if 'ais1' in var:
        df = pd.DataFrame()
        df['y_pred'] = y_pred
        df['y_pred_rounded'] = round(df['y_pred'])
        df['y_test'] = y_test
        df.replace({1: "AIS A", 2: "AIS B", 3: "AIS C", 4: "AIS D"}, inplace=True)

        test_metrics = plot_confusion2(np.array(df['y_test']), np.array(df['y_pred_rounded']), var)

    elif 'lower' in var:
        test_metrics = calculate_metrics(y_test, y_pred)

    best_param = grid_search.best_params_

    return y_pred, test_metrics, best_param

def plot_confusion2(y_true, y_imp, var):

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
        '--outcome',
        type=str,
        help='Name of the variable to be used as an outcome variable',
        required=True,
    )

    parser.add_argument(
        '--scenario',
        type=str,
        help='Name of the population used for subsetting based on AIS grade proportion',
        required=True,
    )

    args = parser.parse_args()
    
    num = args.subset
    scenario = args.scenario
    outcome = args.outcome

    path = "/cluster/scratch/blucie/NA_SCI/subsets_withNA/" + scenario  + "_outcome" + outcome + "/subset_" + str(num) + '_n500.csv'
    data = pd.read_csv(path)

    # Harmonise the NAs signature to numpy format
    data = data.replace('nan', np.nan)

    for pattern in ['MCAR', 'MAR', 'MNAR']: #for each pattern of missingness
        for var in ['ais1', 'lower01', outcome]: #for each variable in which missingness was introduced

            case = var + '_' + pattern
            idx = np.where(data[case].isna())[0] #get the indices of the missing values

            list_models_name = ['lr', 'knn', 'RF', 'SVM_linear', 'SVM_rbf', 'mean']
            if var == outcome:
                #if outcome == 'lower26':
                #    list_models_name = []

                list_models_name = list_models_name + ['last_obs']

            for model_name in list_models_name: #for each imputation model
            #for model_name in ['mean']:
                # Define the parameters grid for grid search cross validation
                # Define the model used for imputation
                if model_name == 'knn':

                    param_grid = [{
                        'scaler': ['passthrough', StandardScaler()],
                        'model__n_neighbors': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                        'model__weights': ['uniform', 'distance']
                    }]

                    # Imputation of AIS grade (categorical) and LEMS (continuous) should rely on
                    # classification and regression implementations of the models, respectively:

                    if var == 'ais1':
                        # The default metric is minkowski, and with p=2 is equivalent to the standard Euclidean metric
                        model = KNeighborsClassifier(algorithm='ball_tree', metric='minkowski', p=2)
                    else:
                        model = KNeighborsRegressor(algorithm='ball_tree', metric='minkowski', p=2)

                elif model_name == 'lr':

                    param_grid = [{
                        'scaler': ['passthrough', StandardScaler()]
                    }]

                    if var == 'ais1':
                        param_grid[0]['model__class_weight'] = [None, 'balanced']
                        param_grid[0]['model__multi_class'] = ['auto', 'ovr', 'multinomial']
                        model = LogisticRegression(penalty='none', max_iter=50000)
                    else:
                        model = LinearRegression()

                elif model_name == 'RF':

                    param_grid = [{
                        'scaler': ['passthrough', StandardScaler()],
                        'model__bootstrap': [True],
                        'model__oob_score': [True, False],
                        'model__n_estimators': [15, 25, 50, 75, 100],
                        'model__max_features': ['auto', 'sqrt', 'log2']
                    }]

                    if var == 'ais1':
                        model = RandomForestClassifier(random_state=9)
                        param_grid[0]['model__criterion'] = ['gini', 'entropy']
                        param_grid[0]['model__class_weight'] = [None, 'balanced', 'balanced_subsample']
                    else:
                        model = RandomForestRegressor(random_state=9)
                        param_grid[0]['model__criterion'] = ['mse', 'mae']

                elif model_name == 'SVM_linear':

                    param_grid = [{
                        'scaler': ['passthrough', StandardScaler()],
                        'model__C': 10.0 ** np.arange(-5, 4)
                    }]

                    if var == 'ais1':
                        model = SVC(kernel='linear', max_iter=50000)
                        param_grid[0]['model__class_weight'] = [None, 'balanced']
                    else:
                        model = SVR(kernel='linear', max_iter=50000)

                elif model_name == 'SVM_rbf':

                    param_grid = [{
                        'scaler': ['passthrough', StandardScaler()],
                        'model__C': 10.0 ** np.arange(-5, 4),
                        'model__gamma': ['scale', 'auto']
                    }]

                    if var == 'ais1':
                        model = SVC(kernel='rbf', max_iter=50000)
                        param_grid[0]['model__class_weight'] = [None, 'balanced']
                    else:
                        model = SVR(kernel='rbf', max_iter=50000)
                
                if model_name == 'last_obs':
                    if not var == outcome:
                        print('Last observation carried forward can only be applied for missing data in the outcome variable.')
                    
                    if outcome == 'lower52':
                        data[case+'_'+model_name]=data[case].fillna(data['lower26'])
                    elif outcome == 'lower26':
                        data[case+'_'+model_name]=data[case].fillna(data['lower16'])
                    
                    bad_df = data.index.isin(idx)
                    test = data[bad_df]
                    init_size = test.shape[0]
                    test = test.dropna(subset=[case+'_'+model_name])
                    print("Remaining NAs after last observation carried forward:", init_size-test.shape[0])
                    train = data[~bad_df]
                    test_metrics = calculate_metrics(np.ravel(test.loc[:, test.columns == var]), np.ravel(test.loc[:, test.columns == case+'_'+model_name]))
                    my_dict = {'metrics_testset': test_metrics}

                elif model_name == 'mean':
                    #bad_df = data.index.isin(idx)
                    #test = data[bad_df]
                    #train = data[~bad_df]
                    if var == 'ais1':
                        data[case+'_'+model_name]=data[case].fillna(data[case].mode().iloc[0])
                        #data[case+'_'+model_name] = np.select([data[case+'_'+model_name] == 1, data[case+'_'+model_name] == 2, data[case+'_'+model_name] == 3, data[case+'_'+model_name] == 4],
                        #           ["AIS A", "AIS B", "AIS C", "AIS D"],
                        #           data[case+'_'+model_name])
                        bad_df = data.index.isin(idx)
                        test = data[bad_df]
                        train = data[~bad_df]
                        #print(np.ravel(test.loc[:, test.columns == var]))
                        #print(np.ravel(test.loc[:, test.columns == case+'_'+model_name]))                      
                        test_metrics = plot_confusion2(np.ravel(test.loc[:, test.columns == var]), np.ravel(test.loc[:, test.columns == case+'_'+model_name]), var)
                    else:
                        data[case+'_'+model_name]=data[case].fillna(data[case].mean())
                        bad_df = data.index.isin(idx)
                        test = data[bad_df]
                        train = data[~bad_df]
                        test_metrics = calculate_metrics(np.ravel(test.loc[:, test.columns == var]), np.ravel(test.loc[:, test.columns == case+'_'+model_name]))
                    my_dict = {'metrics_testset': test_metrics}

                else:
                    y_pred, test_metrics, best_params = single_imputation(data, case, idx, model, outcome, param_grid=param_grid)
                    
                    if var == 'ais1':
                        y_pred = np.select([y_pred == 1, y_pred == 2, y_pred == 3, y_pred == 4],
                                   ["AIS A", "AIS B", "AIS C", "AIS D"],
                                   y_pred)
                    data[case+'_'+model_name] = data[case]
                    nulls = data[pd.isnull(data[case+'_'+model_name])]

                    for i, ni in enumerate(nulls.index[:len(y_pred)]):
                        data[case+'_'+model_name].loc[ni] = y_pred[i]
                    
                    my_dict = {'metrics_testset': test_metrics, 'best parameters': best_params}
                
                with open('/cluster/scratch/blucie/NA_SCI/metrics/' + scenario + '/' + outcome + '/subset_'+ str (num) + '_' + case + '_' + model_name + '_n500.csv', 'w') as f:
                    for key in my_dict.keys():
                        f.write("%s,%s\n"%(key,my_dict[key]))

        path_save = '/cluster/scratch/blucie/NA_SCI/subsets_imputed/' + scenario + '/' + outcome + '/subset_' + str(num) + '_imputed_n500.csv'
        data.to_csv(path_save)
