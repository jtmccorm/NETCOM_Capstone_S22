# @author Bobby Nelson
# @author Blake Jacobs
# Last Edit: 5/8/2022

from airflow import DAG
from airflow.operators.bash import BashOperator
from datetime import datetime as dt

default_args = {
    'owner': 'NETCOM Capstone 2022',
    'start_date': dt(2022, 4, 13, 4),
    'schedule_interval': '0 4 * * *',
    'catchup': False
}

dag = DAG('anomoly_scores', default_args=default_args)

# Pull the NULLFOX data and pre-process it
clean_data = BashOperator(
    task_id='clean_data',
    bash_command='python3 /usr/local/airflow/dags/src/CleanEnrichedData.py',
    dag=dag)

# Short sleep between src to give AWS S3 objects time to properly close before being opened again.
# Needed to add these to have the whole DAG run continuously without error.
sleep_1 = BashOperator(
    task_id='sleep_1',
    bash_command='sleep 30s',
    dag=dag)

# Run anomaly detection models on data to generate scores.
anomaly_scores = BashOperator(
    task_id='calc_scores',
    bash_command='python3 /usr/local/airflow/dags/src/CreateAnomalyScores.py',
    dag=dag)

sleep_2 = BashOperator(
    task_id='sleep_2',
    bash_command='sleep 30s',
    dag=dag)

# Generate SHAP values
shap_values = BashOperator(
    task_id='post_hoc',
    bash_command='python3 /usr/local/airflow/dags/src/CreateSHAP.py',
    dag=dag)

# Set order of execution
clean_data >> sleep_1 >> anomaly_scores >> sleep_2 >> shap_values
