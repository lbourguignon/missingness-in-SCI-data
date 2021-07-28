# Study of missingness in spinal cord injury data

## Description

The following describes the structure of the file `missing_data_code.R`.

## Dependencies

- requires `finalfit` library
- runs in `R version 4.0.2`

## Skeleton

- Packages and seed
- Data loading
- Functions
	- `subset_col(vec, df)` : subset columns in `vec` from dataframe `df`; output is a dataframe
	- `vector_var(vec, week, order)` : add string `week` to all elements of `vec` as pre- or suffix based on `order` (take values 'before' or 'after'); output is a vector
	- `generate_missing_col_CAR(data, prop, col)` : NAs introduced **completely at random** from column `col` in dataframe `df`; output is a vector
	- `generate_missing_col_AR(data, prop, col)` : NAs introduced **at random** (more missingness in male population) from column `col` in dataframe `df`; output is a dataframe with columns `ptid` and `paste0(col, '_MAR')`
	- `generate_missing_col_NAR(data, col)` : NAs introduced **not at random** (more AIS D are missing) from column `col` in dataframe `df`; output is a vector --> more work needed on this function, proportions not included at the moment
- Report number of missing data depending on different scenarii in Sygen data
- Visualise missing data in raw Sygen cohort
- Filter out missing values to use only complete cases in subsequent analyses 
- Introduce missing data (one column per variable per pattern of NAs)
	- Create columns with 30% of data missing completely at random
		- AIS grade : `ais1_MCAR`
		- LEMS at baseline : `lower01_MCAR`
		- LEMS at week 52 : `lower52_MCAR`
	- Create columns with 30% of data missing at random
		- AIS grade : `ais1_MAR`
		- LEMS at baseline : `lower01_MAR`
		- LEMS at week 52 : `lower52_MAR`
	- Create columns with 30% of data missing not at random
		- AIS grade : `ais1_MNAR`
		- LEMS at baseline : `lower01_MNAR`
		- LEMS at week 52 : `lower52_MNAR`
	- Convert the different to the correct type : factor or numeric variables
	- Visual inspection of missingness introduced
		- 0 -- baseline
		- 1A -- MCAR in a confounding variable (`ais1_MCAR`)
		- 1B -- MAR in a confounding variable (`ais1_MAR`)
		- 1C -- MNAR in a confounding variable (`ais1_MNAR`)
		- 2A -- MCAR in a explanatory variable (`lower01_MCAR`)
		- 2B -- MAR in a explanatory variable (`lower01_MAR`)
		- 2C -- MNAR in a explanatory variable (`lower01_MNAR`)
		- 3A -- MCAR in a outcome variable (`lower52_MCAR`)
		- 3B -- MAR in a outcome variable (`lower52_MAR`)
		- 3C -- MNAR in a outcome variable (`lower52_MNAR`)
- Imputation step
- Statistical analyses and comparison