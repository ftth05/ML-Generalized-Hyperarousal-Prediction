import pandas as pd
import numpy as np

from statistics import stdev

# import statsmodels
# import statsmodels.tsa.stattools as stats

df = pd.read_csv("../../data/forecasting_markers_of_habitual_driving_behaviors/data_1.csv")
# print(df.head())
print(df.shape)




df = df[df['Perinasal'].notna()]
df['perinasal_treatment_mean'] = df.groupby(['Subject', 'Drive'])['Perinasal'].transform('mean')
# df['perinasal_treatment_mean'] = df['Perinasal'].groupby(df[['Subject', 'Drive']]).transform('mean')

# print(df.head())
# print(df.tail())


new_df = pd.DataFrame()

for subj in df['Subject'].unique():
  temp_df = df.copy()[df.Subject == subj]
  perinasal_rb_mean = temp_df['perinasal_treatment_mean'].iloc[0]
  # print(subj, perinasal_rb_mean)
  
  temp_df['perinasal_diff'] = temp_df.Perinasal - perinasal_rb_mean
  temp_df['arousal'] = np.where(temp_df['perinasal_diff'] <= 0, 0, 1)

  new_df=new_df.append(temp_df)
  
  
df = new_df
df.to_csv("../../data/forecasting_markers_of_habitual_driving_behaviors/test/test_data_1.csv", sep=',')







df = df.dropna()
# print(df.head())
print(df.shape)









sum_of_squares = lambda x: sum(x**2)


df['perinasal_mean'] = df.Perinasal.rolling(10, min_periods=1).mean()
df['perinasal_median'] = df.Perinasal.rolling(10, min_periods=1).median()
df['perinasal_sd'] = df.Perinasal.rolling(10, min_periods=2).apply(stdev)
df['perinasal_ss'] = df.Perinasal.rolling(10, min_periods=1).apply(sum_of_squares)



# df.Perinasal.rolling(10, min_periods=1).apply(stats.acf)
# df[['column_new_1', 'column_new_2', 'column_new_3']] = df.Perinasal.rolling(10, min_periods=1).apply(stats.acf)





df['hr_mean'] = df.Heart.rolling(10, min_periods=1).mean()
df['hr_median'] = df.Heart.rolling(10, min_periods=1).median()
df['hr_sd'] = df.Heart.rolling(10, min_periods=2).apply(stdev)
df['hr_ss'] = df.Heart.rolling(10, min_periods=1).apply(sum_of_squares)


df['br_mean'] = df.Breathing.rolling(10, min_periods=1).mean()
df['br_median'] = df.Breathing.rolling(10, min_periods=1).median()
df['br_sd'] = df.Breathing.rolling(10, min_periods=2).apply(stdev)
df['br_ss'] = df.Breathing.rolling(10, min_periods=1).apply(sum_of_squares)


df['palm_mean'] = df.Palm.rolling(10, min_periods=1).mean()
df['palm_median'] = df.Palm.rolling(10, min_periods=1).median()
df['palm_sd'] = df.Palm.rolling(10, min_periods=2).apply(stdev)
df['palm_ss'] = df.Palm.rolling(10, min_periods=1).apply(sum_of_squares)


print(df.head())


df.to_csv("../../data/forecasting_markers_of_habitual_driving_behaviors/data_2.csv", sep=',')

