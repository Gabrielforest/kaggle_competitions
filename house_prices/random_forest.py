import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
import seaborn as sns
import sklearn
import os

# diretório
os.getcwd()
os.listdir()
os.chdir('C:\\Users\\OppenSocial\\Desktop\\kaggle_competitions\\house_prices')

# importando os dados
submission = pd.read_csv('./submission.csv')
train = pd.read_csv('./train.csv')
test = pd.read_csv('./test.csv')
train.head()

# agrupando os dados
ntrain = train.shape[0]
ntest = test.shape[0]
y_train = train.SalePrice.values
all_data = pd.concat((train, test)).reset_index(drop = True)
# retirando a coluna y
all_data.drop(['SalePrice'], axis = 1, inplace = True)

# nomes das variáveis
all_cols = all_data.columns

# nomes das colunas numéricas
num_cols = all_data._get_numeric_data().columns

# nomes das colunas categóricas
cat_cols = list(set(all_cols) -  set(num_cols))

# contagem de missing
df_missing = all_data.isnull().sum(axis = 0).reset_index()
df_missing.columns = ['column_name', 'missing']
print(df_missing[df_missing['missing'] > 0])

# input de medianos em NAs (num_cols)
for cols in num_cols:
    all_data[cols] = all_data[cols].fillna(all_data[cols].median())

# criando categoria para NAs = missing (cat_cols)
for cols in cat_cols:
    all_data[cols] = all_data[cols].fillna('missing')

# conferindo se ainda existe algum NA
all_data.isnull().values.sum()

# categórica para números
from sklearn.preprocessing import LabelEncoder
le = LabelEncoder()
for cols in cat_cols:
    all_data[cols] = le.fit_transform(all_data[cols])

# dividindo em dados de teste e treino novamente:
X_train = all_data[:train.shape[0]]
X_test = all_data[train.shape[0]:]

# Random Forest
from sklearn.ensemble import RandomForestRegressor
regressor = RandomForestRegressor(n_estimators = 150, random_state = 0, oob_score = True)
regressor.fit(X_train, y_train)

# previsão:
y_pred = regressor.predict(X_test)
# y de teste e y predito
plt.figure(figsize = (12))
plt.plot(y_test, color = 'red')
plt.plot(y_pred, color = 'blue')
plt.show()

s = pd.DataFrame({"id":test['Id'], "SalePrice":y_pred})
s.to_csv("submission_python.csv", index = False)
