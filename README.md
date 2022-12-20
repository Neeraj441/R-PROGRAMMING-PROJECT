# R-PROGRAMMING-PROJECT
QMST 5334 STATISTICAL METHODS PROJECT

QMST 5334 Final Report
Analysis of hospital readmissions for patients with diabetes
Neeraj Kumar Reddy Panta, Ruchi Dilip Kukde
1.0 Introduction
The prediction of hospital readmission is a significant healthcare research area from data analytics and information 
systems perspective. It aims to develop and analyze models using historical medical data to predict probability of a patient 
returning to hospital in a certain period, e.g., 30 or 90 days, after the discharge (Wang & Zhu, 2022). Prediction of hospital 
readmission is a complex research problem due to the intricate nature of various diseases and healthcare eco-systems (Hospital 
Readmissions, 2018). As data scientists, we can provide solutions to the healthcare sector for optimizing resources and reducing 
the readmissions and associated costs using technology and various analytical tools in hand. Apart from the tangible outcomes 
associated with these solutions, the development and implementation of analytical models for hospital readmissions is 
significant from humanitarian point of view: in a way that helps patients with better treatment, care, and support (Healthstream, 
2021). The motivation of choosing this study is to apply data analytics, specifically statistical analytical tools to identify 
underlying causes for readmissions, to achieve meaningful and transparent predictions for effective decision making. The report 
is divided into various sections describing purpose and description of dataset, basic statistics, descriptive plots, linear 
regression, analysis of residuals and analysis of variance.
The dataset used for this project focuses on hospital readmissions data in United States. The data was collected in the 
form of comprehensive clinical records across hospitals throughout United States by Health Facts database – Cerner 
Corporation, Kansas City. The data was submitted to UCI Machine Learning Repository (UCI Machine Learning Repository, 
2014) in 2014 on behalf of the Center for Clinical and Translational Research, Virginia Commonwealth University, a recipient 
of NIH CTSA grants UL1 TR00058 and a recipient of the CERNER data (Strack, et al., 2014). The dataset represents 10 years 
(1999-2008) of clinical care at 130 US hospitals and integrated delivery networks. It includes 55 features representing patient 
and hospital outcomes. Appendix A of the report presents the data dictionary for the variables in the dataset. Information was 
extracted from the database for encounters that satisfied the following criteria:
i. It is an inpatient encounter (a hospital admission).
ii. It is a diabetic encounter, that is, one during which any kind of diabetes was entered to the system as a diagnosis.
iii. The length of the stay was at least 1 day and at most 14 days.
iv. Laboratory tests were performed during the encounter.
v. Medications were administered during the encounter.
