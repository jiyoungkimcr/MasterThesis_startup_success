# MasterThesis_startup_success
This is a repository of codes used for the experiments in Master's thesis "Analysis on Factors contributing to Success of Startups: Unsupervised and Supervised Learning Approach to Predict Success".

***About this Thesis*** 

**Author:** Jiyoung Kim

**Abstract**

Startups have emerged as important drivers of economic development and increase in employment, and also play as a vital stimulus for disruptive innovation all around the world. Even in the era of the COVID-19 crisis, many newly founded startups took the opportunity out of risk. Unlike the large influx of startups in the market, only a few of them manage to survive and become successful. In general, the biggest reason behind failure is a lack of financial capital to sustain the business. This situation provides enough necessity to conduct an advanced quantitative analysis of a startup's success.

This thesis aims to identify the factors contributing to the startup's success and predict the success in machine learning approach. The dataset used for the experiment is taken from the Crunchbase database which offers the largest and most consistent data related to startups. Raw data which consists of 17 individual tables are transformed into one final dataset through extensive data preprocessing. Most importantly, the target variable 'success' is more broadly defined as the company which did an IPO or acquisition or is operating and received the funding until series C funding. For explanatory variables, they consist of the ones related to funding and investor factors.

This research featured Latent Dirichlet Allocation, an unsupervised algorithm, to cluster the startups into 14 representative industries, 3 supervised learning algorithms such as Logistic Regression, Random Forest, XGBoost, and 10 sub-models built to predict the success of startups using those algorithms but applied with several feature selection methods and hyperparameter tuning method to improve the performance. Considering the class imbalance in the data, all models are compared based on the evaluation metrics such as F1 score, ROC AUC, and AUPRC (Area Under Precision-Recall Curve) score which, for the best model, were 66%, 88%, and 77% respectively. This best outcome is from the XGBoost model tuned with hyperparameters selected via GridSearchCV. On top of that, the best model is tested on new additional input data of recently founded startups (2017~2020) to check how well our model predicts real data that has never been introduced to the model. The result showed 56% of the F1 score, 81% of the ROC AUC score, and 49% of the AUPRC score. Lastly, Logistic Regression coefficients are interpreted in detail and feature importance is checked for Random Forest and XGBoost to understand significant success factors.

Findings in this research can contribute to the growth of startup ecosystem all around the world and especially, can be useful for potential or current founders of startups to identify factors to be improved, for investors to better distinguish potentially successful or non-successful startups and for policymakers to come up with regulations or incentives that can benefit early stage startups.

**Key-words**: Startup, Success, Predictive Modeling, Topic Modeling, Clustering, LDA, Logistic Regression, Random Forest, XGBoost, Hyperparameter Tuning, Crunchbase
