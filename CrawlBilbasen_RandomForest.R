#Crawl bilbasen
library('ggplot2') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('randomForest') # classification algorithm

# Source the web scraper for bilbasen.dk
source("Crawlbilbasen.R")

#Remove rows with n.a. data
df <- na.omit(df)

#Randomize order for us to split data set into training and test sets
set.seed(666)
dfmodel <- df[sample(nrow(df),nrow(df)),c("URL", "Model", "Year", "Engine", "Horsepower", "Doors" ,"Price","Mileage","Consumption", "Region")]

#Train using two thirds of the dataset, test using the last third
trainingData <- dfmodel[1:ceiling(nrow(df)*(2/3)),]
testData <- dfmodel[1:floor(nrow(df)*(1/3)),]

# Build the model using the specified variables
rfModelPrice <- randomForest(Price ~ Year + Engine + Mileage + Consumption + Doors + Horsepower + Model + Region, 
                             data = trainingData, 
                             proximity = TRUE)

# Get importance of variables
importance    <- importance(rfModelPrice)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'IncNodePurity'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 

# Predict using the test set
predictionResult <- data.frame(PredictedPrice = predict(rfModelPrice, testData), 
                               ActualPrice = testData$Price,
                               Model = testData$Model)
predictionResult$Error <- predictionResult$ActualPrice - predictionResult$PredictedPrice

# The car is cheap if its actual price is lower than the predicted
predictionResult$CheapCar <- predictionResult$PredictedPrice > predictionResult$ActualPrice

# Plot actual vs. predicted price
ggplot(predictionResult, aes(x=ActualPrice, y=PredictedPrice, color=CheapCar)) +
        geom_point(alpha = 0.2) + 
        geom_abline(slope = 1, linetype="dashed") + 
        geom_text(aes(label = Model), check_overlap = TRUE, vjust = 1) + 
        theme(aspect.ratio = 1) + 
        coord_fixed(ratio=1) +
        theme_minimal()

# Boxplots of errors vs. model 
ggplot(predictionResult, aes(x=reorder(Model, Error, fun=median),y=Error)) +
  geom_jitter(color="tomato", alpha = 0.3) + 
  geom_boxplot(outlier.alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype="dashed", color="blue") +
  xlab("Model") +
  ylab("Error") +
  coord_flip() +
  theme_minimal()

#Best VW Golf to buy
idx <- which(predictionResult$Model == "Passat")
bestbuy <- idx[which.min(predictionResult$Error[idx])]
testData[bestbuy,]

#Open URL 
browseURL(paste("http://www.bilbasen.dk", testData[bestbuy,"URL"], sep=""),
                browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")