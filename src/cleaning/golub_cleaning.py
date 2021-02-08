import pandas as pd
import numpy as np
import os

os.chdir('..')
os.chdir('..')

#read the data
datadir = os.getcwd()+'/data/raw/'
outpath = os.getcwd()+'/data/cleaned/'
actual_df = pd.read_csv(datadir + 'actual.csv')
test_df_raw = pd.read_csv(datadir + 'data_set_ALL_AML_independent.csv')
train_df_raw = pd.read_csv(datadir + 'data_set_ALL_AML_train.csv')

#rename columns for ease of access
train_df_raw.columns = train_df_raw.columns.str.replace('.','_').str.replace(' ','_')
test_df_raw.columns = test_df_raw.columns.str.replace('.','_').str.replace(' ','_')

#drop 'call' columns
to_drop = train_df_raw.columns.str.contains('call')
train_df = train_df_raw.loc[:,~to_drop]
to_drop = test_df_raw.columns.str.contains('call')
test_df = test_df_raw.loc[:,~to_drop]

#reorder train_df columns
to_map = train_df.loc[:,~train_df.columns.str.contains('Gene')].columns.to_series()
to_map = to_map.astype(int).sort_values().astype(str)
to_map = pd.Series(['Gene_Description','Gene_Accession_Number']).append(to_map)
train_df = train_df[to_map.values]

#reorder test_df columns
to_map = test_df.loc[:,~test_df.columns.str.contains('Gene')].columns.to_series()
to_map = to_map.astype(int).sort_values().astype(str)
to_map = pd.Series(['Gene_Description','Gene_Accession_Number']).append(to_map)
test_df = test_df[to_map.values]

#combine
golub = pd.merge(train_df, test_df, on=['Gene_Description','Gene_Accession_Number'])

#export as csv's
golub.to_csv(path_or_buf = outpath+'/golub_combined.csv', index=False)
train_df.to_csv(path_or_buf = outpath+'/train.csv', index=False)
test_df.to_csv(path_or_buf = outpath+'/test.csv', index=False)