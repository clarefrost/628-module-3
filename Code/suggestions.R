sug <- c("The waiting time for customers may be too long. Shorten that time benefits your business! ",
        "Parking lots create convinience for your customers. More parking places can increase your rating!",
        "Train your waiters to be more friendly.",
        "Train your waiters to be more courteous.",
        "Sports games make customers excited. It would be great if you can play the sport games on your TV!",
        "Making better coffee is a good way to increase rating.",
        "Fresh food is essential for your business. Keep the materials fresh!",
        "Train your waiters to be more warm to customers.",
        "Make your customers feel welcomed.",
        "Loud enviroment can drive people crazy. Try to control the noise in your restaurant.",
        "More TVs can make your place attractive.",
        "Since your location may be difficult to find, a marked sign can help a lot.",
        "Having happy hour in your restaurant can make your restaurant more popular.",
        "Having table for large group can attract more customers.",
        "Reasonable Price should be considered.",
        "Make your table more clean.",
        "Train your bartender better.",
        "Keep your kitchen clean.",
        "Being a nice place for family benefits your business.",
        "Delicious waffle attracts customers.",
        "Clear and extensive menu suits customers well.",
        "Adjusting the decoration may be a good choice.",
        "Having a great patio would make your customers happier.",
        "Crisp flavor attracts customers.",
        "The tip may be a little high.",
        "Eating with music is a fantastic experiience. Give your customers great background music.",
        "Do better in the cocktail. More kinds or more tasty.",
        "Make the meat tasted more juicy.",
        "Do better in the salmon.",
        "Desserts can be better.",
        "NULL")
coeff <- c(-7.044,-0.81,4.58,2.01,0.59,0.57,3.12,-7.31,3.16,-1.96,0.27,-1.23,0.91,0.74,
           2.35,-3.97,-0.9,-2.1,1.63,1.06,1,1.37,1.37,1.72,-3.58,1.39,1.89,3,0.85,2.04)
selected <- c(1,4,5,9,11,12,14,15,16,18,19,27,28,30,31,32,34,35,37,51,54,55,56,57,58,60,63,64,62,39)
X <- X[,selected]
colnames(X) <- NULL
X_new <- cbind(business,X)
X_new <- X_new[,2:32]
ave <- c()
for(i in levels(business$business_id)){
  index <- which(X_new$business_id==i)
  ave <- rbind(ave,apply(X[index,],FUN = mean,MARGIN = 2))
}
rownames(ave) <- levels(business$business_id)
mean_all <- apply(X, MARGIN = 2,FUN = mean)
suggestions <- c()
for(i in levels(business$business_id)){
  index_neg <- intersect(which(ave[i,]>mean_all),which(coeff<0))
  index_pos <- intersect(which(ave[i,]<mean_all),which(coeff>0))
  score <- abs(ave[i,c(index_neg,index_pos)]-mean_all[c(index_neg,index_pos)])*abs(coeff[c(index_neg,index_pos)])
  index <- order(score[score>0],decreasing = T)
  if(length(index)>=3){
    index <- index[1:3]
  }
  else if(length(index)==2){
    index <- c(index,31)
  }
  else if(length(index)==1){
    index <- c(index,31,31)
  }
  else{
    index <- c(31,31,31)
  }
  suggestions <- rbind(suggestions,sug[index])
}
suggestions <- cbind(levels(business$business_id),suggestions)
colnames(suggestions) <- c("business_id","sug1","sug2","sug3")
write.csv(suggestions,"suggestions.csv",row.names = F,col.names = T)
