"""
This script reads in data that has been scored with anomaly detection algorithms and then creates explanations
with SHAP values.

@author Bobby Nelson
@date 5/8/2022
"""

import pandas as pd
from xgboost import XGBRegressor
from sklearn.metrics import r2_score
import shap


def fitRegression(X, y):
    """
    Fit an XGBoost regressor to the data.
    Parameters:
        X (Pandas dataframe): features for the regression.
        y (numpy array): targets for the regression.
    Returns:
        reg (XGBRegressor): fitted regression model between X and y.
    """
    reg = XGBRegressor(n_estimators=1000,
                       max_depth=10)
    reg.fit(X.to_numpy(), y)
    print("R2:", r2_score(reg.predict(X.to_numpy()), y))
    return reg


def generateSHAP(reg, X):
    """
    Generates dataframe of SHAP values based on a fitted regression object and feature values.
    Parameters:
        reg (XGBRegressor): fitted XGBoost regression model.
        X (Pandas dataframe): features to be explained.
    Returns:
        shap_df (Pandas Dataframe): dataframe of SHAP values for the regression object and feature values.
    """
    shap_explainer = shap.Explainer(reg, feature_names=X.columns)
    shap_values = shap_explainer(X)
    shap_df = pd.DataFrame(columns=X.columns, index=range(X.shape[0]))
    for i in range(X.shape[0]):
        shap_df.iloc[i] = shap_values[i].values
    shap_df["baseline"] = shap_values[0].base_values
    return shap_df


def getFromS3(bucket_name, object_key):
    """
    Retrieve a data frame from a CSV stored on AWS S3.

    Parameters:
        bucket_name (str): name of the S3 bucket.
        object_key (str): object key for the AWS S3 object.
    Returns:
        df (Pandas Dataframe): pandas dataframe of the CSV.
    """
    path = "s3://{}/{}".format(bucket_name, object_key)

    df = pd.read_csv(path)
    return df


def writeToS3(df, bucket_name, object_key):
    """
        Writes a pandas dataframe to AWS S3.

        Parameters:
            df (Pandas Dataframe): pandas dataframe to write to a CSV.
            bucket_name (str): name of the S3 bucket.
            object_key (str): object key for the AWS S3 object.
        """
    path = "s3://{}/{}".format(bucket_name, object_key)

    df.to_csv(path, index=False)


def main():
    # pull data and split to X and y
    # TODO: change these resource strings to reflect where you saved the scored data.
    df = getFromS3(bucket_name="xstream-capstone",
                   object_key="data/EnrichedScoreData.csv")

    anomaly_methods = ["xstream", "iforest", "lof", "ocsvm"]

    score_dict = {}
    for method in anomaly_methods:
        score_dict[method] = df[method + "_scores"]

    X = df.iloc[:, :-len(anomaly_methods)]

    # fit regression to data
    reg_dict = {}
    for method in anomaly_methods:
        reg_dict[method] = fitRegression(X, score_dict[method])

    # get SHAP values
    SHAP_dict = {}
    for method in anomaly_methods:
        SHAP_dict[method] = generateSHAP(reg_dict[method], X)

    # write to file
    for method in anomaly_methods:
        # TODO: change these resource strings to reflect where you want to save the SHAP values.
        writeToS3(df=SHAP_dict[method],
                  bucket_name="xstream-capstone",
                  object_key="data/SHAP" + method + ".csv")


if __name__ == '__main__':
    main()
