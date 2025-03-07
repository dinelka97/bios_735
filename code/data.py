## -- script to directly import data from UCI, and save as csv

from ucimlrepo import fetch_ucirepo 
  
# fetch dataset 
heart_disease = fetch_ucirepo(id=45) 
  
# data (as pandas dataframes) 
X = heart_disease.data.features 
y = heart_disease.data.targets 
  
# metadata 
#print(heart_disease.metadata) 
  
# variable information 
#print(heart_disease.variables) 

heart_disease.variables.to_csv("data/var_info.csv", index=False)
X.to_csv("data/features.csv", index=False)
y.to_csv("data/target.csv", index=False)
