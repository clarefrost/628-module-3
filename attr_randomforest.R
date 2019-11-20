##data(iris)
#install.packages("randomForest")
#library("randomForest")
#attr=read.csv("attr_and_stars.csv")

#set.seed(71)
#attr.rf <- randomForest(stars_business ~ ., data=attr, importance=TRUE,
                        #proximity=TRUE)
#print(attr.rf)
## Look at variable importance:
#round(importance(attr.rf), 2)
#print(attr.rf.err.rate)
#plot(attr.rf) 


#install.packages("rpart.plot")
require(rpart)
require(rpart.plot)
df <- read.csv('attr_and_stars.csv', header = T, na.strings=c())

#The missing rate of all the attributes is around 0.5

#After deleting columns with missing rate > 0.5, the total missing rate reduces to 0.165
sum(is.na(df)) / (7107*36)
tree <- rpart(stars_business ~ ., method = 'anova', data = df,
              control = list(maxdepth = 4, cp = 0))
rpart.plot(tree)
summary(tree)

fact=c("Caters","BusinessParking_street","GoodForMeal_breakfast","Alcohol","GoodForKids",
        "NoiseLevel","OutdoorSeating","RestaurantsPriceRange2","WiFi","RestaurantsReservations")

for (i in fact) {
  df[,i] <- relevel(df[,i], ref ="None" )
  print(i)
}

m1=lm(stars_business ~Caters+BusinessParking_street+GoodForMeal_breakfast+Alcohol+GoodForKids+
        NoiseLevel+OutdoorSeating+RestaurantsPriceRange2+WiFi+RestaurantsReservations, data=df)
summary(m1)

m2=lm(stars_business ~Caters+BusinessParking_street+GoodForMeal_breakfast+Alcohol+GoodForKids+
        NoiseLevel+OutdoorSeating+RestaurantsPriceRange2, data=df)
summary(m2)


# Give suggestions

attr_name=c("Caters","BusinessParking_street","GoodForMeal_breakfast","Alcohol","GoodForKids",
              "NoiseLevel","OutdoorSeating","RestaurantsPriceRange2")
suggest=rep("",7107)
for (i in dim(df)[1]) {
  if(df[i,"Caters"]!="True"){
    suggest[i]=paste("Provide cater will increase 0.2 star for you.", sep = " ")
  }
  
  if(df[i,"BusinessParking_street"]!="True"){
    suggest[i]=paste( suggest[i], "Provide BusinessParking will increase 0.25 star for you.",sep = " ")
  }
  
  if(df[i,"GoodForMeal_breakfast"]!="True"){
    suggest[i]=paste( suggest[i], "Provide good breakfast will increase 0.05 star for you.",sep = " ")
  }
  
  if(df[i,"Alcohol"]!="beer_and_wine"){
    suggest[i]=paste( suggest[i], "Provide wine and beer will increase 0.15 star for you.",sep = " ")
  }
  
  if(df[i,"GoodForKids"]="True"){
    suggest[i]=paste( suggest[i], "Kids being noisy may decrease 0.2 star for you.",sep = " ")
  }
  
  if(df[i,"NoiseLevel"]!="quiet"){
    suggest[i]=paste( suggest[i], "Loud noise may decrease up to 0.5 star for you! So please keep quiet.",sep = " ")
  }
  
  
  
  
}











