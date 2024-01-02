### Setup ###
library(randomForest)
library(MASS)
library(klaR)
library(ggplot2)
chocolate = read.csv("chocolate.csv") # Update this to your local path
df <- chocolate
attach(df)

# dropping columns with corrupted data
df [,2] <- NULL # dropping column with corrupted data (mix of bean types and brand names)
df [,2] <- NULL # dropping ID column (now in position 2)

# renaming some columns
colnames(df)[1] <- "Company"
colnames(df)[2] <- "RevYear"
colnames(df)[3] <- "Cocoa"
colnames(df)[4] <- "HQ"
colnames(df)[6] <- "Bean"
colnames(df)[7] <- "Origin"

attach(df)

# transforming coca percentage column to operate as numerical 

C <- gsub("%", "", Cocoa)
C <- as.numeric(C)
C <- C / 100 
df$Cocoa <- C
attach(df)

### Uncomment to Perform EDA ###

# ## Numerical Variables
# 
# # Summary stats
# summary(RevYear)
# summary(Cocoa)
# sd(Cocoa)
# 
# # Boxplots
# boxplot(RevYear, col="chocolate4", main="Boxplot - Year of Review", data = df)
# 
# boxplot(RevYear, col="chocolate4", main="Boxplot - Year of Review")
# boxplot(Cocoa, col="chocolate4", main="Boxplot - Cocoa Percentages")
# 
# 
# # Histograms
# hist(Cocoa, breaks = 25, col = "chocolate4", main = "Histogram - Cocoa Percentages", xlab = "Cocoa Percentage", ylab = "Frequency")
# 
# hist(RevYear, breaks=10, col="chocolate4", main = "Histogram - Year of Review", xlab = "Year", ylab = "Frequency")
# 
# hist(Rating, breaks=15, col="chocolate4", main = "Histogram - Ratings", xlab = "Score Between 1 and 5",ylab = "Frequency" )
# 
# 
# # Scatterplots against rating
# plot(Cocoa,Rating, col="chocolate4", main = "Scatterplot - Cocoa Percentage and Rating", xlab = "Cocoa Percentage", ylab = "Rating" )
# plot(RevYear, Rating,col="chocolate4", main = "Scatterplot - Review Year and Rating", xlab = "Review Year", ylab = "Rating")
# 
# 
# ## Categorical Variables
# length(unique(df$Company))
# sort(table(df$Company), decreasing = TRUE)
# length(unique(df$HQ))
# sort(table(df$HQ), decreasing = TRUE)
# length(unique(df$Bean))
# sort(table(df$Bean), decreasing = TRUE)
# length(unique(df$Origin))
# sort(table(df$Origin), decreasing = TRUE)


### DATA TRANSFORMATIONS ###
df_2<- df
detach(df)
attach(df_2)
View(df_2)



excl_bean <- c("Trinitario, Forastero", "Forastero, Trinitario", "Criollo, Trinitario", "Criollo, Forastero", "Blend-Forastero,Criollo",
               "Trinitario, Criollo", "Trinitario, Nacional", "Forastero (Nacional)", "Nacional (Arriba)", "Nacional",
               "Beniano", "EET", "Matina", "Amazon mix", "Amazon, ICS", "CCN51", "Amazon", "Blend")

trinitario <- c("Trinitario", "Trinitario (Amelonado)",
                 "Trinitario (Scavina)", "Trinitario, TCGA")

forastero <- c("Forastero", "Forastero (Arriba)", "Forastero (Parazinho)","Forastero (Arriba) ASS", "Forastero (Catongo)",
               "Forastero(Arriba, CCN)", "Forastero (Amelonado)", "Forastero (Arriba) ASSS")

criollo <- c("Criollo", "Criollo (Porcelana)", "Criollo (Amarru)", "Criollo (Ocumare 61)", "Trinitario (85% Criollo)", 
             "Criollo (Ocumare 67)", "Criollo (Ocumare 77)", "Criollo (Ocumare)", "Criollo (Wild)", "Criollo, +")


# Create binary columns for each list
df_2$Trinitartio <- ifelse(df_2$Bean %in% trinitario, 1, 0)
df_2$Forastero <- ifelse(df_2$Bean %in% forastero, 1, 0)
df_2$Criollo <- ifelse(df_2$Bean %in% criollo, 1, 0)
df_2$Excl_Bean <- ifelse(df_2$Bean %in% excl_bean, 1, 0) # excluding beans which are not purely trinitario, forastero, or criollo
# Create a Null_Bean column, stores all the null value records
df_2$Null_Bean <- ifelse(rowSums(df_2[, c("Trinitartio", "Forastero","Criollo","Excl_Bean")]) == 0, 1, 0)


# Uncomment for category frequencies
sum(df_2$Trinitartio)
sum(df_2$Forastero)
sum(df_2$Criollo)

# drop records with null bean value, then the column itself
null_bean_indices = which(df_2$Null_Bean == 1)
df_2 <- df_2[-null_bean_indices, ]
# drop bean types excluded from analysis
excl_bean_indices = which(df_2$Excl_Bean == 1)
df_2 <- df_2[-excl_bean_indices, ]

df_2 [,12] <- NULL # dropping the null_bean column  
df_2 [,11] <- NULL # dropping the excl_bean column  
attach(df_2)

# Creating column holding all classes
df_2$Bean_Family <- NA
bean_cols <- c("Trinitartio", "Forastero", "Criollo")
if(all(bean_cols %in% colnames(df_2))) {
  df_2$Bean_Family <- bean_cols[max.col(df_2[, bean_cols])]
} else {
  stop("Dummy cols don't exist")
}
attach(df_2)


### QDA MODELING - BEAN TYPE ###
df_2$Bean_Family=as.factor(df_2$Bean_Family)
attach(df_2)

beanqda=qda(Bean_Family ~ Cocoa + Rating)
beanqda

partimat(Bean_Family~Cocoa+Rating, method="qda",image.colors=c("darkgoldenrod3", "burlywood4", "chocolate"),
         main = "Bean Family Partition Matrix")


### UNCOMMENT TO COMPARE LDA MODELING - BEAN TYPE ###
# beanlda=lda(Bean_Family ~ Cocoa + Rating)
# beanlda
# 
# partimat(Bean_Family~Cocoa+Rating, method="lda",image.colors=c("darkgoldenrod3", "burlywood4", "chocolate"),
#          main = "Bean Family Partition Matrix - LDA")

### UNCOMMENT FOR QDA PREDICTIONS (IN SCENARIO OF LOWER ERROR RATE) ###
# predict(beanqda,data.frame(Rating=4.5, Cocoa=0.8))
# predict(beanqda,data.frame(Rating=3, Cocoa=0.7))
# predict(beanqda,data.frame(Rating=2, Cocoa=0.9))
