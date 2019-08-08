from sklearn.svm import SVC
from sklearn.model_selection import StratifiedKFold
from sklearn.feature_selection import RFECV
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from sklearn import svm
from sklearn.datasets import samples_generator
from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import f_regression
from sklearn.feature_selection import f_classif
from sklearn.feature_selection import chi2
from sklearn.feature_selection import mutual_info_classif
from sklearn.pipeline import Pipeline
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import cross_val_predict
import csv
from sklearn import datasets
from sklearn.externals.six.moves import xrange
from sklearn.mixture import GaussianMixture as GMM
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split
import itertools
from sklearn.metrics import accuracy_score


dataset_ids = ['foodstudy','SHED9','SHED10','Victoria','Vancouver','Taxi']

def get_class_label(row):
    return dataset_ids.index(row['dataset'])


def plot_confusion_matrix(cm, cross_valid_mean_std,
                          normalize=False,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
    classes = ['FSD','S9','S10','VIC','VAN','TAXI']
    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
        print("Normalized confusion matrix")
    else:
        print('Confusion matrix, without normalization')

    print(cm)
    title = "K = " + str(cross_valid_mean_std[0]) + ", mean = " + str(round(cross_valid_mean_std[1],4)) + ", std = " + str(round(cross_valid_mean_std[2],4))
    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    # plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, ['FSD','S9','S10','VIC','VAN','TAXI'])
    plt.yticks(tick_marks, ['FSD','S9','S10','VIC','VAN','TAXI'])

    fmt = '.2f' if normalize else 'd'
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, format(cm[i, j], fmt),
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    # plt.text(0.5,0.5,'', fontsize=12)
    plt.tight_layout()


def gmm_classifier():
    # Break up the dataset into non-overlapping training (75%) and testing
    # (25%) sets.
    data = pd.read_csv("/Users/ruizhang/Dropbox (Personal)/dimensionality_activity_space_0326/Features-1.0/results/all_datasets/feature_data.csv")
    y = data['dataset'].values
    X = data.iloc[:,1:].values
    skf = StratifiedKFold(n_splits=4)
    skf.get_n_splits(X, y)
    n_classes = len(np.unique(y))

    for train_index, test_index in skf.split(X, y):
        # print("TRAIN:", train_index, "TEST:", test_index)
        X_train, X_test = X[train_index], X[test_index]
        y_train, y_test = y[train_index], y[test_index]

        # Try GMMs using different types of covariances.
        classifiers = dict((covar_type, GMM(n_components=n_classes,
                                            covariance_type=covar_type, max_iter=20))
                           for covar_type in ['spherical', 'diag', 'tied', 'full'])

        n_classifiers = len(classifiers)
        for index, (name, classifier) in enumerate(classifiers.items()):
            # Since we have class labels for the training data, we can
            # initialize the GMM parameters in a supervised manner.
            print(classifier)
            classifier.means_ = np.array([X_train[y_train == i].mean(axis=0)
                                          for i in xrange(n_classes)])

            # Train the other parameters using the EM algorithm.
            classifier.fit(X_train)

            y_train_pred = classifier.predict(X_train)
            print(y_train.ravel())
            print(y_train_pred.ravel())
            train_accuracy = np.mean(y_train_pred.ravel() == y_train.ravel()) * 100
            print('Train accuracy: %.1f', train_accuracy)

            y_test_pred = classifier.predict(X_test)
            test_accuracy = np.mean(y_test_pred.ravel() == y_test.ravel()) * 100
            print('Test accuracy:  %.1f', test_accuracy)


if __name__ == '__main__':
    results_dir = "/Users/ruizhang/Dropbox (Personal)/dimensionality_activity_space_0326/Features-1.0/results/all_datasets/"
    normalized_data_path = results_dir + "normalized_features_all_datasets.csv"
    feature_columns = ['dataset', 'user_id', 'convex_hull','buffer_area','N10','dim','C1','C2','C3','C4',
                       'C5']
    features_dataframe = pd.read_csv(normalized_data_path, usecols=feature_columns)
    X = np.array(features_dataframe.iloc[:, 2:])
    y = np.array(features_dataframe.apply(get_class_label, axis=1))
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=42)
    anova_filter = SelectKBest(f_classif, k=9).fit(X_train,y_train)
    clf = svm.SVC(kernel='linear', gamma='auto', class_weight={0:28, 1:5, 2:7, 3:4, 4:10, 5: 1})
    # clf = svm.SVC(kernel='linear', gamma='auto')
    anova_svm = Pipeline([('anova', anova_filter), ('svc', clf)])
    # scores = cross_val_score(anova_svm, X_train, y_train, scoring='accuracy', cv=5)
    # print("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
    # prediction = anova_svm.predict(X)
    # print(anova_svm.score(X, y))
    # print(anova_svm.named_steps['anova'].get_support())
    score_means = list()
    score_stds = list()
    bestN = (1, 2, 3, 4, 5, 6, 7, 8, 9)
    predict_results = []
    all_conf_mats = []
    for n in bestN:
        anova_svm.set_params(anova__k=n)
        # Compute cross-validation score using 1 CPU
        this_scores = cross_val_score(anova_svm, X_train, y_train, scoring='accuracy', cv=5)
        anova_svm.fit(X_train,y_train)
        y_pred = anova_svm.predict(X_test)
        print(accuracy_score(y_test, y_pred))
        conf_mat = confusion_matrix(y_test, y_pred)
        all_conf_mats.append(conf_mat)
        score_means.append(this_scores.mean())
        score_stds.append(this_scores.std())
        predict_results.append([n, this_scores.mean(), this_scores.std()])
        print(anova_svm.named_steps['anova'].get_support())
    print(predict_results)


    # for i in range(1,10):
    #     plt.figure()
    #     plot_confusion_matrix(all_conf_mats[i-1], normalize=True, cross_valid_mean_std=predict_results[i-1],
    #                           title='Normalized confusion matrix')
    #     plt.savefig("confusion_matrix_"+str(i)+".png", dpi=300)
    #
    # np.savetxt("prediction_results_new.csv", predict_results, delimiter=",", fmt='%s')
    # with open("prediction_results_new.csv", 'w') as myfile:
    #     wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
    #     wr.writerow(predict_results)

    # plt.errorbar(bestN, score_means, np.array(score_stds))
    #
    # plt.title(
    #     'Performance of the SVM-Anova varying the number of features selected')
    # plt.xlabel('Number of selected features')
    # plt.ylabel('Prediction rate')
    #
    # plt.axis('tight')
    # plt.show()

    # gmm_classifier()
