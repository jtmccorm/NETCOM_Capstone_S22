# NETCOM CAPSTONE

## Anomaly Detection Web Application using Dynamic Ensembles and SHAP Explanations

### Heinz College - Spring 2022

----

The purpose of our project was to help NETCOM understand the effectiveness of anomaly detection
models in use cases pertinent to the Command’s mission and effectively incorporate these models
into its current workflow. 

We accomplished this through an extension to NETCOM’s existing
Apache Airflow malicious URL detection pipeline. We developed an additional Directed Acyclic
Graph to engineer features, run four anomaly detection algorithms (xStream, isolation forests, local
outlier factor, and one-class spanning vector machine), and generate Shapley Additive Explanations
for the data. We then present these results and explanations in a web-interface of our own design.
Finally, we encapsulate our work within a sample risk framework to show how it can contribute to
cyber risk management practices both on the front line and across the Command.

**See the deployed app [here](https://jtmccorm.shinyapps.io/Anomaly_Scoring_for_NIDS/)!**

Integral to development of the final product was a series of case studies we conducted to determine the utility of anomaly detection in the cyber security space. Ultimately we found that though anomaly detection algorithms were successful in the space, it was difficult to determine without labeled data which model would perform best. This analysis informed our decision to ensemble our anomaly detection in the final deployed product and provide dynamic weighting controls to the end-user. Moving forward NETCOM will be able to dynamically evaluate the performance of anomaly detection and how it relates with their overall risk mitigation strategy.

### Contributors

> Lydia Barit
>
>* Project Manager
>* Risk Response Strategist


> [Katy Dula](https://github.com/katkod)
> 
>* Data Scientist
>* Process Organizer


>Blake Jacobs
>
>* Back-end Developer
>* Chief Systems Administrator


>Harrison Leinweber
>
>* Cyber Security Analyst
>* Promotional Content Director


> [Bobby Nelson](https://github.com/nelsonrg)
>
>* Machine Learning Engineer
>* Financial Manager

> [John T McCormick](https://github.com/jtmccorm)
>
>* Data Scientist
>* Front-end Developer

