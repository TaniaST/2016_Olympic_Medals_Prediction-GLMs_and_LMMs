# 2016 Olympic Medals Prediction-GLMs and LMMs
This repository contains code for the 2016 Olympic medals prediction using:

* **generalised linear models (GLMs)** - quasi-poisson, negative binomial, normal linear models;
* **linear mixed models (LMMs)** - linear mixed model with random intercept, with uncorrelated and correlated intercept and slope random effects.

The data set (rioolympics.csv) file contains observations for each of the countries - participants of 2016 Rio Olympics with the following variables (all the YY variables are for 2000, 2004, 2008, 2012 and 2016 years):

* **gdpYY**: the country’s GPD in millions of US dollars during year YY, 
* **popYY**: the country’s population in thousands in year YY, 
* **goldYY**: number of gold medals won in the YY Olympics,
* **totYY**: total number of medals won in the YY Olympics, 
* **totgoldYY**: overall total number of gold medals awarded in the YY Olympics, 
* **totmedalsYY**: overall total number of all medals awarded in the YY Olympics, 
* **bmi**: average BMI (not differentiating by gender), 
* **altitude**: altitude of the country’s capital city, 
* **athletesYY**: number of athletes representing the country in the YY Olympics.
* **country**: the country’s name,
* **country.code**: the country’s three-letter code,
* **soviet**: 1 if the country was part of the former Soviet Union, 0 otherwise, 
* **comm**: 1 if the country is a former/current communist state, 0 otherwise, 
* **muslim**: 1 if the country is a Muslim majority country, 0 otherwise, 
* **oneparty**: 1 if the country is a one-party state, 0 otherwise, 
* **host**: 1 if the country has hosted/is hosting/will be hosting the Olympics, 0 otherwise.


## Installation & Usage
To reproduce the results:

* Clone a copy of this repository to your computer
* Extract all the files from the zip folder
* Install all the packages mentioned in requirements.txt file
* Run Predicting_2016_Olympic_Medals_Script.R file

## Results
To see the results of the analysis, open the Olympic_medals_prediction_report.md file in this repository.
