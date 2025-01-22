# Machine Learning and Statistical Analysis with R

This repository contains a comprehensive collection of R scripts for various machine learning and statistical analysis techniques. The project includes implementations of decision trees, neural networks, regression analysis, time series forecasting, and more.

## Table of Contents

- [Features](#features)
- [Project Structure](#project-structure)
- [Installation](#installation)
- [Usage](#usage)
- [Models and Techniques](#models-and-techniques)
- [Requirements](#requirements)
- [Results](#results)

## Features

- Decision tree implementations (CART, C5.0, CHAID)
- Neural network analysis with parameter optimization
- Association rule mining
- Bayesian network analysis
- Clustering and image feature extraction
- Regression analysis (linear and logistic)
- Time series forecasting
- Data visualization using ggplot2

## Project Structure

```
.
├── association_rules_basic.R      # Basic association rule mining
├── association_rules_income.R     # Income-based association analysis
├── bayesian_networks_analysis.R   # Bayesian network implementation
├── clustering_and_image_features.R # Clustering algorithms
├── decision_trees_comparison.R    # Decision tree models comparison
├── neural_networks_analysis.R     # Neural network implementation
├── regression_and_time_series.R   # Regression and time series analysis
├── ggplot2_basics.R              # Basic data visualization
├── ggplot2_annotations.R         # Advanced plotting techniques
└── ggplot2_maps_demographics.R   # Geographical visualization
```

## Installation

1. Clone this repository:
```bash
git clone [repository-url]
```

2. Install required R packages:
```R
install.packages(c("arules", "arulesViz", "MASS", "RSNNS", "rpart", "C50", 
                  "randomForest", "ggplot2", "forecast", "TSA", "car", 
                  "lmtest", "RoughSets"))
```

## Usage

Each R script is self-contained and can be run independently. Here are some examples:

### Decision Trees
```R
source("decision_trees_comparison.R")
```

### Neural Networks
```R
source("neural_networks_analysis.R")
```

### Association Rules
```R
source("association_rules_basic.R")
```

## Models and Techniques

### Decision Trees
- CART (Classification and Regression Trees)
- C5.0
- CHAID
- Random Forest

### Neural Networks
- Multi-layer Perceptron
- Parameter optimization
- Performance comparison across different architectures

### Association Rules
- Apriori algorithm
- Support and confidence analysis
- Rule visualization

### Regression Analysis
- Linear regression with diagnostics
- Logistic regression
- Time series forecasting with ARIMA

### Data Visualization
- Basic plots using ggplot2
- Advanced annotations
- Geographical visualizations
- Demographic analysis

## Requirements

- R >= 4.0.0
- Required packages:
  - arules (>= 1.6-8)
  - arulesViz (>= 1.5-1)
  - MASS (>= 7.3-54)
  - RSNNS (>= 0.4-14)
  - rpart (>= 4.1-15)
  - C50 (>= 0.1.6)
  - randomForest (>= 4.6-14)
  - ggplot2 (>= 3.3.5)
  - forecast (>= 8.16)
  - TSA (>= 1.3)
  - car (>= 3.0-12)
  - lmtest (>= 0.9-40)
  - RoughSets (>= 1.3-7)
- Sufficient memory for large datasets

## Results

### Decision Tree Performance
- CART (cp=0.03): Training accuracy 83.5%, Test accuracy 75.6%
- C5.0: Training accuracy 81.5%, Test accuracy 73.5%
- CHAID: Training accuracy 75.5%, Test accuracy 78.9%

### Neural Network Performance
- Best performance with 12-13 hidden neurons
- Learning rate optimization between 0.001-0.1
- Cross-validated accuracy metrics

### Regression Analysis
- Linear regression model diagnostics show good fit
- Logistic regression achieves optimal performance at 0.4-0.5 threshold
- Time series forecasting shows strong predictive capability

---

# Detailed Script Documentation

### association_rules_basic.R & association_rules_income.R
- Implements association rule mining using the Apriori algorithm
- Uses `arules` and `arulesViz` packages
- Works with IncomeESL dataset to discover patterns
- Features:
  - Support and confidence thresholds configuration
  - Rule visualization with multiple metrics
  - Subset rule filtering based on lift and confidence
  - Interactive rule exploration

### bayesian_networks_analysis.R
- Implements both Naive Bayes and Tree-augmented Naive Bayes (TAN)
- Uses `bnlearn` package for Bayesian network modeling
- Works with Pima diabetes dataset
- Features:
  - Data preprocessing and discretization
  - Model structure visualization
  - Prediction accuracy evaluation
  - Cross-validation testing

### clustering_and_image_features.R
- Implements various clustering algorithms
- Uses `EBImage` for image feature extraction
- Includes both hierarchical and k-means clustering
- Features:
  - Haralick texture feature extraction
  - Multiple distance metrics (euclidean, manhattan, minkowski)
  - Different linkage methods comparison
  - Within/between cluster sum of squares analysis

### decision_trees_comparison.R
- Comprehensive comparison of different decision tree algorithms
- Implements CART, C5.0, and CHAID methods
- Features:
  - Cost-complexity pruning for CART
  - Accuracy comparison across methods
  - Variable importance analysis
  - Cross-validation error estimation
  - Tree visualization and interpretation

### neural_networks_analysis.R
- Multi-layer perceptron implementation
- Parameter optimization framework
- Features:
  - Hidden layer size optimization (12-16 neurons)
  - Learning rate testing (0.001-0.1)
  - Weight matrix visualization
  - Iterative error plotting
  - Model evaluation metrics

### regression_and_time_series_analysis.R
- Comprehensive regression analysis tools
- ARIMA time series modeling
- Features:
  - Linear regression diagnostics
  - Residual analysis
  - Time series decomposition
  - Forecasting with confidence intervals
  - Model validation tests

### ggplot2_*.R Scripts
- Complete data visualization toolkit
#### ggplot2_basics.R
- Basic plotting fundamentals
- Common chart types implementation
#### ggplot2_annotations.R
- Advanced plot customization
- Text and label annotations
#### ggplot2_maps_demographics.R
- Geographical data visualization
- Demographic data analysis
- Features:
  - Map creation and customization
  - Demographic data overlay
  - Interactive visualizations
  - Population-weighted analysis

Each script includes detailed comments explaining:
- Function parameters and usage
- Data preprocessing steps
- Model training procedures
- Evaluation metrics calculation
- Result interpretation guidelines

### rough_sets_analysis.R
- Implements Rough Set Theory for data analysis
- Uses `RoughSets` package
- Features:
  - Decision table creation
  - Indiscernibility relation computation
  - Lower and upper approximation calculation
  - Rule induction with configurable support
  - Alpha and gamma coefficient calculation

For specific implementation details, refer to the inline documentation within each script.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is available for academic and educational purposes.

## Reference

簡禎富、許嘉裕（2018），**大數據分析與資料挖礦**
