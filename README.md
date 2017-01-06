# IMDb Score Prediction

Data Source: 
IMDB 5000 Movie Dataset. https://www.kaggle.com/deepmatrix/imdb-5000-movie-dataset

Dataset Details:
28 variables (12 factor & 16 numerical variables), 5043 row of movies. 
Overall, the movies span across 100 years in 66 countries. There are 2399 unique director names, and 30K actors/actresses. 

# Model 

Profound Question:
- Can we build a model to predict a movie’s score on IMDB before the movie’s release date?

Goal:
- Discover the common features of highly-rated movies.
- Choose the best data-mining technique to predict imdb scores based on mean square errors.

Method:
- Plot Decision Trees. 
- Develop clusters using K-Means Clustering method.
- Predict score by applying Neural Network, K Nearest Neighbor and Random Forest algorithms.

Procedure:
- Data understanding:
  - correlation analysis
  - exploratory data analysis
- Data preparation:
  - variable reduction
  - data cleaning
  - data normalization
  - data division
- Modeling
  - CART
  - k-means clustering
  - neural network
  - k nearest neighbor
  - random forest
- Evaluation
- Deployment

# Analysis
- corr.png: Correlation analysis
- critic vs score.png, score vs year.png, hist_score.png, voted vs score.png: Exploratory data analysis (EDA) of dependent variable imdb_score
- missing data.png: Number of missing values of each numerical attribute
- tree1,cp=0008.png, tree2,cp=0004.png: Decision tree method
- NN.png: Neural network method
- rf.png: Random forest method, variable importance
- result compare.png: real vs. predicted imdb score using three methods: Random Forest, Neural Network and K Nearest Neighbor


