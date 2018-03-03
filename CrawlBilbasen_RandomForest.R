#Crawl bilbasen
library('ggplot2') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('randomForest') # classification algorithm
library('rvest') # web scraper

#Clean up
rm(list=ls())
closeAllConnections()

#Get data from URL
URLtoCrawl <- "https://www.bilbasen.dk/brugt/bil/vw?fuel=0&yearfrom=2010&yearto=2018&pricefrom=0&priceto=10000000&mileagefrom=-1&mileageto=10000001&zipcode=0000&includeengroscvr=false&includesellforcustomer=true&includewithoutvehicleregistrationtax=false&includeleasing=false&page=1"
website <- read_html(URLtoCrawl)
NumberOfHits <- website %>% html_nodes(".scrambledtab") %>% html_text(trim = TRUE)
NumberOfHits <- as.numeric(gsub(".+\\((\\d+).","\\1",gsub("\\.","",NumberOfHits[1])))

#Initialize
CarmakeDetail <- NULL
CarURL <- NULL
CarConsumption <- NULL
CarMileage <- NULL
CarYear <- NULL
CarRegion <- NULL
CarPrice <- NULL

#Cookie set to display 32 results per page
NumberOfPages <- ceiling(NumberOfHits/32)

for(i in 1:NumberOfPages) {
  
  # Set URL to equal increasing page number denoted by i
  URLtoCrawl <- gsub(paste("page=",i-1,sep=""),paste("page=",i,sep=""),URLtoCrawl)
  website <- read_html(URLtoCrawl)
  
  #Detailed description of car make
  CarmakeDetail <- c(CarmakeDetail, website %>% html_nodes(".darkLink") %>% html_text(trim = TRUE))
  CarURL <- c(CarURL, website %>% html_nodes(".darkLink") %>% html_attr("href"))
  
  #Car price
  CarPrice <- c(CarPrice, website %>% html_nodes(".listing-price")  %>% html_text(trim = TRUE))
  
  #Car region
  CarRegion <- c(CarRegion, website %>% html_nodes(".listing-region")  %>% html_text(trim = TRUE))
  
  #Car metrics
  CarListingData <- website %>% html_nodes(".listing-data")  %>% html_text(trim = TRUE)
  CarConsumption <- c(CarConsumption, CarListingData[1:4==2])
  CarMileage <- c(CarMileage, CarListingData[1:4==3])
  CarYear <- c(CarYear, CarListingData[1:4==4])
}


#Create dataframe
df = data.frame(CarmakeDetail,
                URL = CarURL,
                Year = CarYear,
                Model = factor(gsub("^\\w+{1}[[:blank:]](.+)[[:blank:]]\\d,.*$","\\1",CarmakeDetail)),
                MakeModel = gsub("^(.+)[[:blank:]]\\d,.*$","\\1",CarmakeDetail),
                Region = CarRegion, 
                Make = factor(gsub("^((\\w+){1}\\w+).*$","\\1",CarmakeDetail)),
                Engine = as.numeric(gsub(",",".",gsub("^.+[[:blank:]](\\d,\\d+).*$","\\1",CarmakeDetail))),
                Doors = gsub("^.+[[:blank:]](\\w{2,})$","\\1",CarmakeDetail),
                Horsepower = as.numeric(gsub("^.+[[:blank:]](\\d{2,}).*$","\\1",CarmakeDetail)),
                YearNumeric = as.numeric(CarYear),
                Price = as.numeric(gsub("\\.","",gsub(" kr.", "", CarPrice))),
                Mileage = as.numeric(gsub("\\.","",CarMileage)),
                Consumption = as.numeric(gsub(",",".",(gsub(".*?([0-9]+,[0-9]).*", "\\1", CarConsumption))))
                )

#Remove rows with n.a. data
df <- na.omit(df)

# Replace 

#Randomize order for us to split data set into training and test sets
set.seed(666)
dfmodel <- df[sample(nrow(df),nrow(df)),c("URL", "Model", "Year", "Engine", "Horsepower", "Doors" ,"Price","Mileage","Consumption", "Region")]

#Train using two thirds of the dataset, test using the last third
trainingData <- dfmodel[1:ceil(nrow(df)*(2/3)),]
testData <- dfmodel[1:floor(nrow(df)*(1/3)),]

# Build the model using the specified variables
rfModelPrice <- randomForest(Price ~ Year + Engine + Mileage + Consumption + Doors + Horsepower + Model + Region, data = trainingData)

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