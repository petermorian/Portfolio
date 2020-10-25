import matplotlib.pyplot as plt
import numpy as np 
import pandas as pd
import seaborn as sns
import random
import time
import sklearn
from sklearn.neural_network import MLPClassifier
from sklearn.neural_network import MLPRegressor
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
from math import sqrt
from sklearn.metrics import r2_score
from sklearn.metrics import classification_report,confusion_matrix
from sklearn.metrics import roc_curve
from sklearn.metrics import roc_auc_score

### LOAD DATA
# train_data.txt is from https://archive.ics.uci.edu/ml/datasets/Parkinson+Speech+Dataset+with++Multiple+Types+of+Sound+Recordings
df = pd.read_csv('./train_data.txt', sep=",", header=None)
df.columns = ["Subject ID",
              'Jitter (local)','Jitter (local, absolute)','Jitter (rap)','Jitter (ppq5)','Jitter (ddp)',
             'Shimmer (local)','Shimmer (local, dB)','Shimmer (apq3)','Shimmer (apq5)', 'Shimmer (apq11)','Shimmer (dda)',
             'AC','NTH','HTN',
              'Median pitch','Mean pitch','Standard deviation','Minimum pitch','Maximum pitch',
              'Number of pulses','Number of periods','Mean period','Standard deviation of period', 
              'Fraction of locally unvoiced frames','Number of voice breaks','Degree of voice breaks',
              'UPDRS', 'class information']

### ANALYSE DATA
print("======================================")
print("======== STEP 1: ANALYSE DATA ========")
print("======================================")
print("")
print("first few rows of raw data")
print(df.head())
print("")
print("Dimensions of raw data")
print(df.shape)
print("")
print("Summary Stats of raw data")
print(df.describe().T.rename(columns={"mean": "Mean", "50%": "Median (50%)"}).T)

### CLEAN DATA, DEFINE TARGET, & PREDICTORS
df.drop(['Subject ID'],axis=1,inplace=True) # drop Subject ID 
df.drop(['UPDRS'],axis=1,inplace=True) # drop UPDRS
target_column = ['class information'] # define target column
predictors = list(set(list(df.columns))-set(target_column))
df[predictors] = df[predictors]/df[predictors].max() #normalise values 
print("first few rows of cleaned data")
print(df.head())
print("")
print("Dimensions of cleaned data")
print(df.shape) #all numeric
print("")
print(" Data Types of cleaned data ")
print(df.dtypes)
print("")
print("Summary Stats of cleaned data")
print(df.describe().T.rename(columns={"mean": "Mean", "50%": "Median (50%)"}).T)
print("")
print("Target variable - class information")
print(df.iloc[:,  -1 ].value_counts())
print(df.iloc[:,  -1 ].value_counts(normalize=True))

# correlation matrix of columns
f = plt.figure(figsize=(27, 27))
plt.matshow(df.corr(), fignum=f.number)
plt.xticks(range(df.shape[1]), df.columns, fontsize=14, rotation=45)
plt.yticks(range(df.shape[1]), df.columns, fontsize=14)
cb = plt.colorbar()
cb.ax.tick_params(labelsize=14)
plt.title('Correlation Matrix', fontsize=16)
plt.savefig('./correlation_matrix.jpeg')

# density plot visualisation of columns
dfm = df.melt(var_name='columns')
g = sns.FacetGrid(dfm, col='columns', col_wrap=5)
g = (g.map(sns.distplot, 'value'))
g.savefig('./density_plots.pdf') #save plots as PDF

# SAVED CLEANED DATASET AS CSV
df.to_csv(r'./classificatiodata.csv',index = False, header=True)


print("")
print("======================================")
print("======== STEP 2: BUILD MODEL ========")
print("======================================")
print("")
### CREATE 60/40 TEST/TEST SPLIT
X = df[predictors].values
y = df[target_column].values
X_train, X_test, y_train, y_test = train_test_split(X, 
                                                    y, 
                                                    test_size=0.4, 
                                                    random_state=40)
print("")
print("Dimensions of training data - ", "X:", X_train.shape, "Y:",  y_train.shape)  
print("Dimensions of test data - ", "X:",  X_test.shape, "Y:",  y_test.shape)
print("")

# TASK 2 - SGD vs ADAM
mlp_gs = MLPClassifier(max_iter=100, #number of epochs
                       hidden_layer_sizes=(8,8,8),
                       learning_rate='adaptive',
                       activation='logistic', #sigmoid 
                      momentum=0.01,
                      learning_rate_init=0.01) 
parameter_space = {
    'solver': ['sgd', 'adam'], # best solver - task 2, 
}
clf = GridSearchCV(mlp_gs, parameter_space, n_jobs=-1, cv=5, scoring='accuracy')
clf.fit(X_train, y_train) 
print("")
print("SGD vs ADAM")
print('Best parameters found:\n', clf.best_params_)
means = clf.cv_results_['mean_test_score']
stds = clf.cv_results_['std_test_score']
lower_ci = means - 1.96*stds
upper_ci = means + 1.96*stds
run_i = list(range(1, len(means)+1))
for run_i, mean, std, lower_ci, upper_ci, params in zip(run_i, means, stds, lower_ci, upper_ci, clf.cv_results_['params']):
     print("Run %0.3i - mean: %0.3f, sd: %0.03f, CI: [%0.3f,%0.3f] for \n %r" % (run_i, mean, std, lower_ci, upper_ci, params))

# TASK 3 - LEARNING RATES
mlp_gs = MLPClassifier(max_iter=100, #number of epochs
                       hidden_layer_sizes=(8,8,8),
                       solver='sgd',
                       learning_rate='adaptive',
                       activation='logistic', #sigmoid
                      momentum=0.01)  
parameter_space = {
    'learning_rate_init': [0.01, 0.02, 0.03], #best learn rate - task 3
}
clf = GridSearchCV(mlp_gs, parameter_space, n_jobs=-1, cv=5, scoring='accuracy')
clf.fit(X_train, y_train) 
print("")
print("LEARNING RATES")
print('Best parameters found:\n', clf.best_params_)
means = clf.cv_results_['mean_test_score']
stds = clf.cv_results_['std_test_score']
lower_ci = means - 1.96*stds
upper_ci = means + 1.96*stds
run_i = list(range(1, len(means)+1))
for run_i, mean, std, lower_ci, upper_ci, params in zip(run_i, means, stds, lower_ci, upper_ci, clf.cv_results_['params']):
     print("Run %0.3i - mean: %0.3f, sd: %0.03f, CI: [%0.3f,%0.3f] for \n %r" % (run_i, mean, std, lower_ci, upper_ci, params))

# TASK 3 - MOMEMTUM RATES       
mlp_gs = MLPClassifier(max_iter=100, #number of epochs
               hidden_layer_sizes=(8,8,8),
               solver='sgd',
               learning_rate='adaptive',
               activation='logistic', #sigmoid 
              learning_rate_init=0.01) 
parameter_space = {
    'momentum': [0.01, 0.02, 0.03], #best learn rate - task 3
}
clf = GridSearchCV(mlp_gs, parameter_space, n_jobs=-1, cv=5, scoring='accuracy')
clf.fit(X_train, y_train)
print("")
print("MOMENTUM RATES")
print('Best parameters found:\n', clf.best_params_)
means = clf.cv_results_['mean_test_score']
stds = clf.cv_results_['std_test_score']
lower_ci = means - 1.96*stds
upper_ci = means + 1.96*stds
run_i = list(range(1, len(means)+1))
for run_i, mean, std, lower_ci, upper_ci, params in zip(run_i, means, stds, lower_ci, upper_ci, clf.cv_results_['params']):
     print("Run %0.3i - mean: %0.3f, sd: %0.03f, CI: [%0.3f,%0.3f] for \n %r" % (run_i, mean, std, lower_ci, upper_ci, params))
 

# TASK 4 - NUMBER OF HIDDEN LAYERS (1-4)
mlp_gs = MLPClassifier(max_iter=100, #number of epochs
                       learning_rate='adaptive',
                       solver='sgd',
                       activation='logistic',#sigmoid 
                      momentum=0.01,
                      learning_rate_init=0.01) 
parameter_space = {
    'hidden_layer_sizes':[(4), (4,4), (4,4,4), (4,4,4,4)] # best number of hidden layers - task 4
}
clf = GridSearchCV(mlp_gs, parameter_space, n_jobs=-1, cv=5, scoring='accuracy')
clf.fit(X_train, y_train) 
print("")
print("NUMBER OF HIDDEN LAYERS")
print('Best parameters found:\n', clf.best_params_)
means = clf.cv_results_['mean_test_score']
stds = clf.cv_results_['std_test_score']
lower_ci = means - 1.96*stds
upper_ci = means + 1.96*stds
run_i = list(range(1, len(means)+1))
for run_i, mean, std, lower_ci, upper_ci, params in zip(run_i, means, stds, lower_ci, upper_ci, clf.cv_results_['params']):
     print("Run %0.3i - mean: %0.3f, sd: %0.03f, CI: [%0.3f,%0.3f] for \n %r" % (run_i, mean, std, lower_ci, upper_ci, params))


# TASK 5 - NUMBER OF NEURONS (1-25)
mlp_gs = MLPClassifier(max_iter=100, #number of epochs
                       learning_rate='adaptive',
                       solver='sgd',
                       activation='logistic',#sigmoid 
                      momentum=0.01,
                      learning_rate_init=0.01) 
parameter_space = {
    'hidden_layer_sizes':[(5),(10),(15),(20),(25)] # best number of hidden layers - task 4
}
clf = GridSearchCV(mlp_gs, parameter_space, n_jobs=-1, cv=5, scoring='accuracy')
clf.fit(X_train, y_train)
print("")
print("NUMBER OF NEURONS")
print('Best parameters found:\n', clf.best_params_)
means = clf.cv_results_['mean_test_score']
stds = clf.cv_results_['std_test_score']
lower_ci = means - 1.96*stds
upper_ci = means + 1.96*stds
run_i = list(range(1, len(means)+1))
for run_i, mean, std, lower_ci, upper_ci, params in zip(run_i, means, stds, lower_ci, upper_ci, clf.cv_results_['params']):
     print("Run %0.3i - mean: %0.3f, sd: %0.03f, CI: [%0.3f,%0.3f] for \n %r" % (run_i, mean, std, lower_ci, upper_ci, params))


# BUILD BEST MODEL
mlp_gs = MLPClassifier(max_iter=100, #number of epochs
                       activation='logistic') #sigmoid 
#list all possible parameters
parameter_space = {
    'hidden_layer_sizes': [(8,8,8), (5,10,5), (25,4), (3,1,5,8), (15), (25)], #best hidden layers and neuron combos - task 4 & 5
    'solver': ['sgd', 'adam'], # best solver - task 2
    'learning_rate_init': [0.01, 0.02, 0.03], #best learn rate - task 3
    'learning_rate': ['constant','adaptive'],
    'momentum': [0.01, 0.02, 0.03], # best momentum - task 3
}
clf = GridSearchCV(mlp_gs, parameter_space, n_jobs=-1, cv=5, scoring='accuracy')
clf.fit(X_train, y_train) 
print("")
print("FINAL MODEL")
print('Best parameters found:\n', clf.best_params_)
means = clf.cv_results_['mean_test_score']
stds = clf.cv_results_['std_test_score']
lower_ci = means - 1.96*stds
upper_ci = means + 1.96*stds
run_i = list(range(1, len(means)+1))
for run_i, mean, std, lower_ci, upper_ci, params in zip(run_i, means, stds, lower_ci, upper_ci, clf.cv_results_['params']):
     print("Run %0.3i - mean: %0.3f, sd: %0.03f, CI: [%0.3f,%0.3f] for \n %r" % (run_i, mean, std, lower_ci, upper_ci, params))
clf=clf.fit(X_train, y_train) #DEFINE BEST MODEL

print("")
print("RESULTS ON TRAINING DATA")
predict_train = clf.predict(X_train)
CM_train = confusion_matrix(y_train,predict_train)
class_names = ['Class_0', 'Class_1']
CM_Dataframe_train = pd.DataFrame(CM_train, 
                            index=class_names, 
                            columns=class_names)
print('Confusion Matrix:')
print(CM_Dataframe_train)
print('Report:')
print(classification_report(y_train,predict_train))
print("")
print("RESULTS ON TEST DATA")
predict_test = clf.predict(X_test)
CM_test = confusion_matrix(y_test, predict_test)
CM_Dataframe_test = pd.DataFrame(CM_test, 
                            index=class_names, 
                            columns=class_names)
print('Confusion Matrix:')
print(CM_Dataframe_test)
print('Report:')
print(classification_report(y_test,predict_test))

#ROC & AUC
ns_probs = [0 for _ in range(len(y_test))]
lr_probs = clf.predict_proba(X_test)
lr_probs = lr_probs[:, 1]
ns_auc = roc_auc_score(y_test, ns_probs)
lr_auc = roc_auc_score(y_test, lr_probs)
print("")
print("ROC & AUC")
print('No Skill: ROC AUC=%.3f' % (ns_auc))
print('Logistic: ROC AUC=%.3f' % (lr_auc))
ns_fpr, ns_tpr, _ = roc_curve(y_test, ns_probs)
lr_fpr, lr_tpr, _ = roc_curve(y_test, lr_probs)
plt.plot(ns_fpr, ns_tpr, linestyle='--', label='No Skill')
plt.plot(lr_fpr, lr_tpr, marker='.', label='Neural Network')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC & AUC for Neural Network')
plt.legend()
plt.savefig('./ROC.jpeg')