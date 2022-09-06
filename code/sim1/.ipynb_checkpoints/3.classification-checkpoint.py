import pandas as pd
import numpy as np

from sklearn.model_selection import train_test_split, RandomizedSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score


df = pd.read_csv("../../data/forecasting_markers_of_habitual_driving_behaviors/data_2.csv")
# print(df.head())
print(df.columns)

print(df.shape)
df = df.dropna()
# print(df.head())
print(df.shape)

df = df[df.Drive != 0]


def get_splitted_data(df, feature_list, predict_col, random = False):
  if random:
    msk = np.random.rand(len(df)) < 0.75
    train_df = df[msk].copy()
    test_df = df[~msk].copy()
  else:
    split = np.int32(0.8 * len(df))

    train_df = df[:split].copy()
    test_df = df[split:].copy()

  #     print(len(train_df))
  #     print(len(test_df))

  X_train = train_df[feature_list]
  y_train = np.int32(train_df[[predict_col]])

  X_test = test_df[feature_list]
  y_test = np.int32(test_df[predict_col])

  return X_train, y_train, X_test, y_test


feature_list = ['Breathing', 'Heart', 'Palm', 'Perinasal',
  'perinasal_mean', 'perinasal_median', 'perinasal_sd', 'perinasal_ss',
  'hr_mean', 'hr_median', 'hr_sd', 'hr_ss',
  'br_mean', 'br_median', 'br_sd', 'br_ss',
  'palm_mean', 'palm_median', 'palm_sd', 'palm_ss']


X_train, y_train, X_test, y_test = get_splitted_data(df, feature_list, 'arousal', random=True)
model = RandomForestClassifier(n_estimators = 200,
                                   max_features = 'auto',
                                   bootstrap = True)
model.fit(X_train, y_train)
y_pred = model.predict(X_test)


result_df = pd.DataFrame({
        'actual': y_test,
        'prediction': y_pred,
        'err_diff': abs(y_test-y_pred),
        'err': y_test == y_pred
    }).sort_values('err_diff', ascending = False)


# print('Accuracy = {:0.2f}%.'.format(100 - 100 * np.mean(result_df.err_diff / y_test)))
# print('Accuracy = {:0.2f}%.'.format(100 - 100 * np.mean(result_df.err_diff / y_test)))

result_df.to_csv("../../data/forecasting_markers_of_habitual_driving_behaviors/test/test_result.csv", sep=',')
print(accuracy_score(y_test, y_pred))
