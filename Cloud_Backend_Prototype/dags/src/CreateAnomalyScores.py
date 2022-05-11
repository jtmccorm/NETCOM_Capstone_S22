"""
This script reads in pre-processed data and generates anomaly scores. The result of
the scores are saved in an AWS bucket.

@author Blake Jacobs
@author Bobby Nelson
@date 5/8/2022
"""

# import packages
import pandas as pd
import os
import sys

# import models and performance metrics
from sklearn.ensemble import IsolationForest
from pyod.models.iforest import IForest
from pyod.models.lof import LOF
from pyod.models.ocsvm import OCSVM


# import xstream package here
sys.path.append(os.path.join(sys.path[0], "xstreamPython/"))
from xstreamPython.Chains import Chains


def fit_xstream(X, k=50, nchains=50, depth=10):
    """
    Create xStream instance and fit to the data.
    Parameters:
        X (Pandas Dataframe): Dataframe to detect anomalies in.
        k (int): number of components
        nchains (int): number of chains
        depth (int): depth of chains
    Returns:
        cf (Chains): fitted xStream model.
    """
    cf = Chains(k=k, nchains=nchains, depth=depth)
    cf.fit(X)
    return cf


def score_xstream(cf, X):
    """
    Generate anomaly scores from the fitted xStream model.
    Parameters:
        cf (xStream::Chains): fitted xStream model
        X (Pandas Dataframe): Dataframe to generate scores on.
    Returns:
        anomalyscores (double): xStream anomaly scores.
    """
    anomalyscores = -cf.score(X)
    return anomalyscores


def score_iforest(X):
    """
    Generate anomaly scores with isolation forest.
    Parameters:
        X (Pandas Dataframe): Dataframe to generate scores on.
    Returns:
        anomalyscores (double): isolation forest anomaly scores.
    """
    cf = IsolationForest()
    cf.fit(X)
    anomalyscores = -cf.decision_function(X)
    return anomalyscores


def score_ocsvm(X):
    """
    Generate anomaly scores with a one-class support vector machine.
    Parameters:
        X (Pandas Dataframe): Dataframe to generate scores on.
    Returns:
        anomalyscores (double): ocsvm anomaly scores.
    """
    cf = OCSVM()
    cf.fit(X)
    anomalyscores = -cf.decision_function(X)
    return anomalyscores


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
    # TODO: change these resource strings to reflect where you saved the pre-processed data.
    df_cleaned = getFromS3(bucket_name="xstream-capstone",
                           object_key="data/CleanEnrichedData.csv")
    X_df = df_cleaned.drop(df_cleaned.filter(regex="Type").columns, axis=1)

    ##hardcoded best hyperparameters
    # xstream score
    cf = fit_xstream(X_df, k=100, nchains=50, depth=20)
    df_cleaned['xstream_scores'] = score_xstream(cf, X_df)

    # iforest score
    clf = IForest(n_estimators=100,
                  max_samples=50,
                  max_features=20)
    clf.fit(X_df)
    df_cleaned['iforest_scores'] = clf.decision_function(X_df)

    # lof scores
    clf = LOF(n_neighbors=100)
    clf.fit(X_df)
    df_cleaned['lof_scores'] = clf.decision_function(X_df)

    # ocsvm scores
    clf = OCSVM(kernel='linear',
                nu=0.005,
                gamma=0.1)
    clf.fit(X_df)
    df_cleaned['ocsvm_scores'] = clf.decision_function(X_df)

    # TODO: change these resource strings to reflect where you want to save the scored data.
    writeToS3(df=df_cleaned,
              bucket_name="xstream-capstone",
              object_key="data/EnrichedScoreData.csv")

    # note that df_cleaned has normalized features and is missing some original features.
    # optionally we can save the scores back to the original data frame with non-normalized scores
    # TODO: change these resource strings to reflect the output of NULLFOX.
    df_original = getFromS3(bucket_name="xstream-capstone",
                            object_key="data/enriched_altIP.csv")
    df_original["xstream_scores"] = df_cleaned["xstream_scores"]
    df_original["iforest_scores"] = df_cleaned["iforest_scores"]
    df_original["lof_scores"] = df_cleaned["lof_scores"]
    df_original["ocsvm_scores"] = df_cleaned["ocsvm_scores"]
    # TODO: change these resource strings to reflect where you want to save the scored results.
    writeToS3(df=df_original,
              bucket_name="xstream-capstone",
              object_key="data/enriched_altIP_with_scores.csv")


if __name__ == '__main__':
    main()
