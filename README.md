# Predicting Aviation Accident Severity using Machine Learning

This project explores the use of machine learning techniques to predict the severity of aviation accidents using real-world data from the National Transportation Safety Board (NTSB). Through a combination of data preprocessing, exploratory data analysis, and modeling, the project identifies key factors influencing crash severity and builds predictive models with high accuracy.

## 📝 Project Overview

- **Duration:** August 2022 – December 2022
- **Goal:** Predict aviation accident severity based on pilot, weather, and flight-related factors
- **Data Source:** NTSB database (12 sheets, 150+ variables)

## 🔍 Key Steps

### 1. **Feature Engineering**
- Extracted and merged custom datasets from the NTSB database using SQL queries guided by literature review.
- Preprocessed data including:
  - Handling missing values
  - Addressing class imbalance
  - Selecting ~45 features from 150+ original variables

### 2. **Exploratory Data Analysis (EDA)**
- Performed correlation analysis and statistical tests to explore relationships between variables.
- Visualized trends in crash severity based on:
  - Pilot experience
  - Weather conditions
  - Flight phases
- Identified most predictive features to inform modeling.

### 3. **Machine Learning Modeling**
- Developed multiple predictive models:
  - Logistic Regression
  - Gradient Boosting Classifier
  - Neural Networks
- Achieved up to **89% accuracy** on test data.
- Found pilot experience and weather conditions as the strongest predictors.

## 🚀 Results
The final model achieved high predictive accuracy and provided actionable insights into key factors affecting aviation accident severity. This work demonstrates the potential of machine learning in aviation safety analytics.

## 📁 Repository Structure
├── data/ # Processed datasets
├── notebooks/ # Jupyter Notebooks for EDA and modeling
├── src/ # Python scripts for data processing and ML
├── visuals/ # Plots and visualizations
├── README.md
└── requirements.txt # Project dependencies
