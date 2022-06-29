# Model Evaluation Case Studies

## NETCOM CAPSTONE - Spring 2022

---- 

Anomaly Detection is an unsupervised machine learning paradigm which aims to identify observations that do not meet the standard statistical distribution of the rest of the data. When applied to cyber security, this task may or may not overlap with detecting malicious traffic. Moreover, NETCOM DSD was interested in applying novel algorithms that had not been tested in this space.

**As such, we conducted several model evaluation case studies to see how well each of our tentative models would perform and how applicable anomaly detection was to the domain space.** This included:

* *CIC-IDS-2017* : a data set consisting of network packets, designed to simulate 16 different cyber security attack types.
* *Malicious and Benign URLS* : a classification data set containing thousands of real-world internet domains.

> Ultimately we found that though anaomaly detection algorithms were successful in the space, it was difficult to determine without labeled data which model would perform best. As such we decided to offer NETCOM a dynamic ensembling methodology that could adequately meet their current and future need.