# Cloud Back-end

## NETCOM CAPSTONE - Spring 2022

----  

Our Cloud Back-End was designed to integrate into NETCOM DSD's existing data pipeline on Apache Airflow and AWS. The primary script is `anomaly_detection_dag.py` which operates Apache Airflow and sequences the source code of the anomaly detection modelling:

- `CleanEnrichedData.py` : This script reads in the enriched domain data from NULLFOX and then pre-processes it for anomaly scoring. The result of the pre-processing are saved to an AWS S3 bucket.
- `CreateAnomalyScores.py` : This script reads in pre-processed data and generates anomaly scores. The result of the scores are saved in an AWS bucket.
- `CreateSHAP.py` : This script reads in data that has been scored with anomaly detection algorithms and then creates explanations
with SHAP values.