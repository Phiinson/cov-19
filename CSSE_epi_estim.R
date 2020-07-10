### BEFORE RUNNING THIS SCRIPT YOU WILL NEED TO CHANGE YOUR FILEPATHS 
### ALSO, MAKE SURE YOU HAVE THE FILE 'CombinedCSSE.csv' IN YOUR WORKING
### DIRECTORY

#install.packages('EpiEstim') #uncomment on first time use
library('EpiEstim')

data1 = read.csv('C:/COVID-RESEARCH/CombinedCSSE.csv') #read in the data
setwd('C:/COVID-RESEARCH/') #Set your working directory

########################################################################
########################################################################

data = data1
final = data[0:1,] #Creating an empty dataframe to input results into
final$R = 0
final = final[0,]
countries <- unique(data$Country) #geting a list of the countries in the dataset
data = data[data$NewCases > -1,] #Taking out rows where NewCases has a negative value 
#negative values in  NewCases does not work with the EpiEstim method
data = data[data$Country != 'US',] #Taking out data from the United States for speed
for(name in countries){ #Looping over all countries
  temp = data[data$Country == name,] #creating a new dataframe with just values from the country
  temp_ests = estimate_R(temp$NewCases, #Calling te method parametrically with mean_si = 5 and std = 3.4
                         method='parametric_si',
                         config = make_config(list(
                           mean_si = 5,
                           std_si = 3.4
                         )))
  temp_ests = temp_ests$R$`Mean(R)` #getting the mean R values calculated from the method
  temp_ests[length(temp_ests):length(temp_ests)+7] = 0 #making the result's length match the new dataframe
  temp$R = temp_ests #adding a new column to the new dataframe with the R(t) estimates
  final = rbind(final,temp) #binding the new dataframe to the result dataframe
}
final <- final[!is.na(final$R),]
write.csv(final,'GlobalCSSE_EpiEstim.csv') #output file as a csv 
final1 <- final[final$R == 5,]
write.csv(final1, 'GlobalCSSE_EpiEstim_Condensed.csv')

data = data1
final1 = data1[0:1,] #Creating an empty dataframe to input results into
final1$R = 0
final1 = final1[0,]
us_data <- data[data$Country == 'US',]
us_data <- us_data[us_data$NewCases >= 0,]
keys <- unique(us_data$CombinedKey)


for(key in keys){
  temp = us_data[us_data$CombinedKey == key,]
  temp_ests = estimate_R(temp$NewCases,
                         method ='parametric_si',
                         config = make_config(list(
                           mean_si = 5,
                           std_si = 3.4
                         )))
  temp_ests = temp_ests$R$`Mean(R)`
  temp_ests[length(temp_ests):length(temp_ests)+7] = 0
  temp$R = temp_ests
  final1 = rbind(final1,temp)
  print(key)
}


write.csv(final1,'US_CSSE_EpiEstim.csv') #output file as a csv 
final = final[final1$R == 5,]
write.csv(final1, 'US_CSSE_EpiEstim_Condensed.csv')
print('done')
