# https://www.kaggle.com/c/titanic
import pandas

titanic = pandas.read_csv("train.csv")
print(titanic.describe())
