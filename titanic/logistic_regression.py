from matplotlib.cbook import boxplot_stats
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
from sklearn.linear_model import LinearRegression
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn import metrics

# diretório
os.getcwd()
os.chdir('C:\\Users\\OppenSocial\\Desktop\\kaggle_competitions\\titanic')

# importando os dados
submission = pd.read_csv("gender_submission.csv")
train = pd.read_csv("train.csv")
test = pd.read_csv("test.csv")

# retirando as colunas de ID
train.drop(['PassengerId'], axis = 1, inplace = True)
test.drop(['PassengerId'], axis = 1, inplace = True)

# agrupando os dados
ntrain = train.shape[0]
ntest = test.shape[0]
y_train = train.Survived.values
all_data = pd.concat((train, test)).reset_index(drop = True)

# retirando a coluna y
all_data.drop(['Survived'], axis = 1, inplace = True)

# alterando tipo da variável Pclass
all_data['Pclass'] = all_data['Pclass'].astype('category')

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

# input de medianos em NaN coluna Fare:
all_data['Fare'] = all_data['Fare'].fillna(all_data['Fare'].median())

# input da maioria para os valores faltantes de Embarked:
all_data.Embarked.value_counts()
all_data['Embarked'] = all_data['Embarked'].fillna('S')

# verificando outliers Age:
_, bp = pd.DataFrame.boxplot(all_data, return_type='both')
plt.show()
whiskers = [whiskers.get_ydata()[1] for whiskers in bp["whiskers"]]
lower_whisker = whiskers[0]
upper_whisker = whiskers[1]

# filtrando outliers 
outlier_filter = all_data[(all_data['Age'] > lower_whisker) & (all_data['Age'] < upper_whisker)]
y_lm = outlier_filter['Age']

# criando dummies:
x_lm = outlier_filter[['Pclass', 'Sex', 'SibSp', 'Parch', 'Embarked']]
x_lm = pd.get_dummies(data= x_lm, drop_first=True)

# prevendo Age:
model = LinearRegression().fit(x_lm, y_lm)

# lógico age == nan
notnans = all_data['Age'].notnull()

# tabela onde age == nan
df_nans = all_data.loc[~notnans].copy()

# retirando colunas que não serão usadas:
df_nans.drop(['Name', 'Cabin', 'Ticket', 'Fare', 'Age'], axis = 1, inplace = True)

# transofrmado em dummies:
df_nans = pd.get_dummies(data = df_nans, drop_first=True)

# predição dos valores NA
df_nans['Age'] = model.predict(df_nans)

# substituindo NA pelos valores preditos:
all_data['Age'] = all_data['Age'].fillna(df_nans['Age'])
all_data.isnull().sum()
list(all_data.columns)

# checando correlação:
cor = np.corrcoef(all_data["Age"].array, all_data["Fare"].array, all_data["SibSp"].array, all_data["Parch"].array)

# dividindo em dados de teste e treino novamente:
X_train = all_data[:train.shape[0]]
X_test = all_data[train.shape[0]:]

# dividindo dados de treino e validação:
X_train, X_val, y_train, y_val = train_test_split(X_train, y_train, 
    test_size=0.25, random_state= 8)

x_lg = X_train[['Age', 'Sex', 'SibSp', 'Pclass']]
x_lg = pd.get_dummies(data= x_lg, drop_first=True)

x_val = X_val[['Age', 'Sex', 'SibSp', 'Pclass']]
x_val = pd.get_dummies(data= x_val, drop_first=True)

# modelo de regressão logística:
logreg = LogisticRegression().fit(x_lg, y_train)
y_pred = logreg.predict(x_val)

# matriz de confusão
confusion_matrix = confusion_matrix(y_val, y_pred)
print(confusion_matrix)

# acurácia, 0.79:
(120 + 57)/ (120 + 19 + 27 + 57)

# previsão final:
x_test = X_test[['Age', 'Sex', 'SibSp', 'Pclass']]
x_test = pd.get_dummies(data= x_test, drop_first=True)
y_final = logreg.predict(x_test)

s = pd.DataFrame({"id":submission['PassengerId'], "Survived":y_final})
s.to_csv("submission_python.csv", index = False)

