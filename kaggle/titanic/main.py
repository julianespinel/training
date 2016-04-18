# https://www.kaggle.com/c/titanic
import pandas
import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.cross_validation import KFold

titanic = pandas.read_csv("train.csv")
print(titanic.describe())

ageMedian = titanic["Age"].median()
print(ageMedian)
titanic["Age"] = titanic.fillna(ageMedian)

print(titanic["Sex"].unique())
titanic.loc[titanic["Sex"] == "male", "Sex"] = 0
titanic.loc[titanic["Sex"] == "female", "Sex"] = 1
print(titanic["Sex"].unique())

embarked = "Embarked"
print(titanic[embarked].unique())
titanic[embarked] = titanic[embarked].fillna("S")
titanic.loc[titanic[embarked] == "S", embarked] = 0
titanic.loc[titanic[embarked] == "C", embarked] = 1
titanic.loc[titanic[embarked] == "Q", embarked] = 2
print(titanic[embarked].unique())

predictors = ["Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked"]
algorithm = LinearRegression()
crossValSet = KFold(titanic.shape[0], n_folds=3, random_state=1)
predictions = []
for train, test in crossValSet:
    train_predictors = (titanic[predictors].iloc[train, :])
    train_target = titanic["Survived"].iloc[train]
    algorithm.fit(train_predictors, train_target)
    test_predictions = algorithm.predict(titanic[predictors].iloc[test, :])
    predictions.append(test_predictions)

predictions = np.concatenate(predictions, axis=0)
predictions[predictions > 0.5] = 1
predictions[predictions <= 0.5] = 0

correctPredictions = [i for i, j in zip(predictions, titanic["Survived"]) if i == j]
numberOfPassangers = len(predictions)
accuracy = len(correctPredictions) / numberOfPassangers
print(accuracy)
