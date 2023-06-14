################################################################################
# SCI - Handling missing data project
# L. Bourguignon & L.P. Lukas
# First version : 04.03.2022
# Last update : 14.06.2023
# ------------------------------------------------------------------------------
# Combine all metrics into 1 dataframe for plotting
################################################################################


################################################################################
# Packages and seed
################################################################################

import pandas as pd
import argparse

################################################################################
# Argument loading
################################################################################

parser = argparse.ArgumentParser()

parser.add_argument(
    '--outcome',
    type=str,
    help='Name of the variable to be used as an outcome variable',
    required=True,
)

args = parser.parse_args()
outcome = args.outcome

models = ['mean', 'lr', 'knn', 'RF', 'SVM_linear', 'SVM_rbf', 'last_obs']

path = '/cluster/scratch/blucie/NA_SCI/metrics/'

################################################################################
# Main
################################################################################

#for sim in ['SygenAll', 'SygenCompleteNAanalysis', 'EMSCI', 'balanced']:
for sim in ['SygenAll', 'balanced']:
    cols = ['subset','pattern', 'model_imputation', 'metric', 'value']
    df_ais = pd.DataFrame(columns=cols)
    df_lems01 = pd.DataFrame(columns=cols)
    df_lems52 = pd.DataFrame(columns=cols)

    for sub in range(1,501):
        for pat in ['MCAR', 'MAR', 'MNAR']:
            for model in models:
                id_mod = pat + '_' + model
                sub = str(sub)
                
                if not outcome == 'lower26': 
                    if not model == 'last_obs':
                        ais = pd.read_csv(path + sim + '/' + outcome + '/subset_' + sub + '_ais1_' + id_mod + '_n500.csv')
                        lems01 = pd.read_csv(path + sim + '/' + outcome + '/subset_' + sub + '_lower01_' + id_mod + '_n500.csv')
                    
                        #print(ais.columns[1].split(": ",1)[1])
                        #print(ais.columns[2].split(": ",1)[1])
                        df_ais.loc[len(df_ais)] = [sub, pat, model, 'kappa', ais.columns[1].split(": ",1)[1]]
                        df_ais.loc[len(df_ais)] = [sub, pat, model, 'MCC', ais.columns[2].split(": ",1)[1]]

                        df_lems01.loc[len(df_lems01)] = [sub, pat, model, 'RMSE', lems01.columns[1].split(": ",1)[1]]
                        df_lems01.loc[len(df_lems01)] = [sub, pat, model, 'MAE', lems01.columns[2].split(": ",1)[1]]
                        df_lems01.loc[len(df_lems01)] = [sub, pat, model, 'R2', lems01.columns[3].split(": ",1)[1]]
            
                lems52 = pd.read_csv(path + sim + '/' + outcome + '/subset_' + sub + '_' + outcome + '_' + id_mod + '_n500.csv')
                df_lems52.loc[len(df_lems52)] = [sub, pat, model, 'RMSE', lems52.columns[1].split(": ", 1)[1]]
                df_lems52.loc[len(df_lems52)] = [sub, pat, model, 'MAE', lems52.columns[2].split(": ", 1)[1]]
                df_lems52.loc[len(df_lems52)] = [sub, pat, model, 'R2', lems52.columns[3].split(": ", 1)[1]]
            
            for mice in ['pmm', 'norm.predict', 'rf', 'polr']:
                id_mod = pat + '_' + mice
                sub = str(sub)
                if not mice == 'polr':
                    if not outcome == 'lower26':
                        lems01 = pd.read_csv(path + sim + '/' + outcome + '/subset_' + sub + '_lower01_' + id_mod + '_n500.csv')
                        df_lems01.loc[len(df_lems01)] = [sub, pat, mice, 'RMSE', lems01.columns[1].split(": ",1)[1]]
                        df_lems01.loc[len(df_lems01)] = [sub, pat, mice, 'MAE', lems01.columns[2].split(": ",1)[1]]
                        df_lems01.loc[len(df_lems01)] = [sub, pat, mice, 'R2', lems01.columns[3].split(": ",1)[1]]
                    lems52 = pd.read_csv(path + sim + '/' + outcome + '/subset_' + sub + '_' + outcome + '_' + id_mod + '_n500.csv')
                    df_lems52.loc[len(df_lems52)] = [sub, pat, mice, 'RMSE', lems52.columns[1].split(": ", 1)[1]]
                    df_lems52.loc[len(df_lems52)] = [sub, pat, mice, 'MAE', lems52.columns[2].split(": ", 1)[1]]
                    df_lems52.loc[len(df_lems52)] = [sub, pat, mice, 'R2', lems52.columns[3].split(": ", 1)[1]]
                if (mice == 'polr' and outcome != 'lower26'):
                    ais = pd.read_csv(path + sim + '/' + outcome + '/subset_' + sub + '_ais1_' + id_mod + '_n500.csv')
                    df_ais.loc[len(df_ais)] = [sub, pat, mice, 'kappa', ais.columns[1].split(": ",1)[1]]
                    df_ais.loc[len(df_ais)] = [sub, pat, mice, 'MCC', ais.columns[2].split(": ",1)[1]]
  
    if not outcome == 'lower26':
        df_ais["value"] = pd.to_numeric(df_ais["value"].str.replace('}', ''))
        df_lems01["value"] = pd.to_numeric(df_lems01["value"].str.replace('}', ''))
        df_ais['rank'] = df_ais.groupby(['subset', 'pattern', 'metric'])['value'].rank(ascending=False, method='max')
        df_lems01['rank'] = df_lems01.groupby(['subset', 'pattern',  'metric'])['value'].rank(ascending=True, method='max')
        df_ais.to_csv(path + sim  + '_ais1_metrics_withmice_n500_' + outcome + '.csv')
        df_lems01.to_csv(path + sim  + '_lower01_metrics_withmice_n500_' + outcome + '.csv')
    
    df_lems52["value"] = pd.to_numeric(df_lems52["value"].str.replace('}', ''))
    df_lems52['rank'] = df_lems52.groupby(['subset', 'pattern', 'metric'])['value'].rank(ascending=True, method='max')
    df_lems52.to_csv(path + sim + '_' + outcome + '_metrics_withmice_n500.csv')
