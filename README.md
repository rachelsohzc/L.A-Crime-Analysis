# Investigating the nature of severe crimes in L.A

### Overview and motivation
Predictive policing is becoming an important part for fair policing today. We wanted to find out if we could predict which areas would have crimes based on factors like victim's age, sex, descent, premise types etc. using decision trees and logistic regression models. The final product is a list of predictors that heavily influence severe crime in Los Angeles. This list could be used to create a heat map to predict which areas are more prone to crime. This is an academic project for ST309.

### Description
For this project, we used the 2020 - Present dataset which can be found here: [L.A crime dataset](https://www.google.com/url?q=https://www.kaggle.com/sumaiaparveenshupti/los-angeles-crime-data-20102020&sa=D&source=docs&ust=1643133381116420&usg=AOvVaw0W60bzck7ApMThjB2a5D1W). 

### How to run
- Make sure you have the packages in the first line of the code installed before running
- Download the dataset from here, and name the files as: "Crime_Data_from_2010_to_2019" and "Crime_Data_from_2020_to_Present" when reading the csv files
- Run the [code](https://github.com/RS201918703/ST309-R-Project/blob/main/Full%20Code.R) on an R Script.

### Improvements
- Categorical data: Categorical data is harder to interpret at times. For instance, when we transformed the premise description column, we only took the top 10 premises and classified the remaining under the ‘OtherPremise’ category. It is possible that doing so affected our analysis.
- More modelling: After going through our analysis, using bagging and bootstrapping may have given us more confidence in our results.
- Analysis limitations: Since we only used the 2020 - Present dataset, this could have affected our results. A merged dataset may have given us higher accuracy rates.

### Conclusion
The results from our analysis showed that the factors Weapon, Sidewalk, Street, Female and Age have a strong link to severe crimes. However, it is also worth noting that these models were created based on training data. Previous predictive policing programs like PredPol have failed because the past data of crime records had race biases. These models may only further magnify these biases and lead to inaccuracy. A more accurate analysis would include a dataset that is free of bias.

### Team members
- Rachel Soh: https://github.com/RS201918703
- Rafay Butt: https://github.com/raf201920011

### References
1. [Decision and classification trees](https://www.datacamp.com/community/tutorials/decision-trees-R)
2. [Predictive policing - part 1](https://www.liberties.eu/en/stories/predictive-policing/43679)
3. [Predictive policing - part 2](https://www.rand.org/content/dam/rand/pubs/research_briefs/RB9700/RB9735/RAND_RB9735.pdf)
4. [Crime forecasting](https://vciba.springeropen.com/articles/10.1186/s42492-021-00075-z#:~:text=Crime%20forecasting%20refers%20to%20the,record%20some%20unusual%20illegal%20activity.)
5. [Crime trends in California](https://www.ppic.org/publication/crime-trends-in-california/)
6. [ROC Curves](https://www.displayr.com/what-is-a-roc-curve-how-to-interpret-it/)
7. [LAPD Patrol Area Maps](https://www.qsl.net/n6uru/lapd-maps.htm)
8. [Introduction to Random Forests](https://towardsdatascience.com/random-forest-3a55c3aca46d)
9. [Random Forests Classifiers](https://www.datacamp.com/community/tutorials/random-forests-classifier-python)
10. [Logistic regression](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3936971/)
