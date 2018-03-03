#Crawl bilbasen

library(data.table)
library(Hmisc)
#library(hexbin)
#library(factoextra)
#library(FactoMineR)
library('ggplot2') # visualization
#library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('randomForest') # classification algorithm

library(rvest)
#Test
#Clean up
rm(list=ls())
closeAllConnections()


#Get data from URL
URLtoCrawl <- "https://www.bilbasen.dk/brugt/bil/vw?fuel=0&yearfrom=2010&yearto=2010&pricefrom=0&priceto=10000000&mileagefrom=-1&mileageto=10000001&zipcode=0000&includeengroscvr=false&includesellforcustomer=true&includewithoutvehicleregistrationtax=false&includeleasing=false&page=1"
website <- read_html(URLtoCrawl)
NumberOfHits <- website %>% html_nodes(".scrambledtab") %>% html_text(trim = TRUE)
NumberOfHits <- as.numeric(gsub(".+\\((\\d+).","\\1",gsub("\\.","",NumberOfHits[1])))

#Initialize
CarmakeDetail <- NULL
CarConsumption <- NULL
CarMileage <- NULL
CarYear <- NULL
CarRegion <- NULL
CarPrice <- NULL

#Cookie set to display 32 results per page
NumberOfPages <- ceiling(NumberOfHits/32)


for(i in 1:NumberOfPages) {
  
  URLtoCrawl <- gsub(paste("page=",i-1,sep=""),paste("page=",i,sep=""),URLtoCrawl)
  
  #Detailed description of car make
  CarmakeDetail <- c(CarmakeDetail,website %>% html_nodes(".darkLink") %>% html_text(trim = TRUE))

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
                Year = CarYear,
                Model = factor(gsub("^\\w+{1}[[:blank:]](\\w+).*$","\\1",CarmakeDetail)),
                MakeModel = gsub("^(\\w+{1}[[:blank:]]\\w+{1}).*$","\\1",CarmakeDetail),
                Region = CarRegion, 
                Make = factor(gsub("^((\\w+){1}\\w+).*$","\\1",CarmakeDetail)),
                Engine = as.numeric(gsub(",",".",gsub("^.+[[:blank:]](\\d,\\d+).*$","\\1",CarmakeDetail))),
                YearNumeric = as.numeric(CarYear),
                Price = as.numeric(gsub("\\.","",gsub(" kr.", "", CarPrice))),
                Mileage = as.integer(gsub("\\.","",CarMileage)),
                Consumption = as.numeric(gsub(",",".",(gsub(".*?([0-9]+,[0-9]).*", "\\1", CarConsumption))))
                )

#Remove duplcates rows and data with n.a. data
df <- na.omit(df)

YearBreaks <- c(2000, 2005, 2010, 2018)
PriceBreaks <- c(0, 250000, 500000, 1e6) 

df$PriceInterval <- cut(df$Price, breaks = PriceBreaks, right = TRUE)
df$Decade <- cut(df$YearNumeric, breaks = YearBreaks, right=TRUE, dig.lab=4)

#Calculate frequencies
df <- setDT(df)[, freqMake := .N, by = .(Make)]
df <- setDT(df)[, freqMakeModel := .N, by = .(MakeModel)]

#Randomize order
dfmodel <- df[sample(nrow(df),nrow(df)),c("MakeModel", "Year", "Engine","Price","Mileage","Consumption", "Region")]

#Train using two thirds of the dataset, test using the last third
train <- dfmodel[1:ceil(nrow(df)*(2/3)),]
test <- dfmodel[1:floor(nrow(df)*(1/3)),]


# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(MakeModel) ~ Year + Engine + Price + Mileage + Consumption, data = train)
rf_model_price <- randomForest(Price ~ Year + Engine + Mileage + Consumption + MakeModel + Region, data = train)

# Show model error
plot(rf_model)
legend('topright', colnames(rf_model_price$err.rate), col=1:3, fill=1:19)

# Get importance
importance    <- importance(rf_model_price)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

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
  coord_flip() + 
  theme_few()

# Predict using the test set
#res <- data.frame(PredictedMakeModel = predict(rf_model, test), ActualMakeModel = test$MakeModel)
resPrice <- data.frame(PredictedPrice = predict(rf_model_price, test), ActualPrice = test$Price)

plot(resPrice$ActualPrice - resPrice$PredictedPrice)

plot(resPrice)



# #Fit linear model
# df.lm <- subset(df, select=c("Price", "Mileage"))
# res.lm <- lm(Price ~ Mileage, df.lm)
# 
# 
# #Boxplots of most popular makes and Price
# pltdta <- df[which(df$freqMake>10)]
# pltmst <- ggplot(pltdta,aes(x=reorder(pltdta$Make, pltdta$Price, fun=Median),y=Price))
# pltmst + geom_boxplot() + geom_jitter(alpha = 0.3, colour="tomato") + coord_flip() + theme_minimal()
# 
# pltdta <- df[which(freqMakeModel>median(df$freqMakeModel))]
# pltmst <- ggplot(pltdta, aes(x=reorder(pltdta$MakeModel, pltdta$Price, fun=Median),y=Price))
# pltmst +  geom_boxplot() + geom_jitter(alpha = 0.3, color = "tomato") + coord_flip() + theme_minimal()
# 
# #Plot value loss for a specific make and model
# MakeAndModel = "VW Polo"
# pltmst <- ggplot(na.omit(df[which(df$MakeModel==MakeAndModel)]),aes(x=Mileage,y=Price, color=factor(Engine)))
# pltmst + geom_point(aes(color=factor(Engine))) + geom_smooth(method="lm") + theme_minimal() + ggtitle(MakeAndModel)
# 
# #Plots value loss for top brands
# pltmst <- ggplot(na.omit(df[which(df$freqMake>100)]),aes(x=Mileage,y=Price))
# pltmst + geom_point(alpha = 0.3) + geom_smooth(method="lm") + facet_wrap(~ Make) + theme_minimal() + geom_abline(intercept = res.lm$coefficients[1], slope = res.lm$coefficients[2], colour = "red") +
#   coord_cartesian(xlim = c(0,250e3))
# 
# #Plot of regional price differences
# pltmst <- ggplot(df[which(Region!="")], aes(x=reorder(Region, Price, fun=Median),y=Price))
# pltmst + geom_boxplot() + geom_jitter(alpha = 0.1, color = "tomato") + theme_minimal() + coord_cartesian(xlim=c(0,1e6)) + coord_flip() 
# 
# #Test plots
# pltdta <- df[which(Make == "VW" & freqMakeModel > 10)]
# pltmst <- ggplot(pltdta,aes(x=Mileage, y=Price, color=MakeModel)) + geom_point() + geom_smooth() 
# pltmst + geom_abline(intercept = res.lm$coefficients[1], slope = res.lm$coefficients[2])
# 

# #Do PCA
# df.pca <- df[which(Make=="Mercedes"),c(7:11)]
# #Prepare dataframe for principal analysis
# res.pca <- prcomp(df.pca, scale=TRUE)
# res.eig <- get_eigenvalue(res.pca)
# res.var <- get_pca_var(res.pca)
# res.var$contrib
# fviz_eig(res.pca)
# #fviz_pca_var(res.pca,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
# fviz_pca_biplot(res.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),alpha.ind = 0.2,label=c("var"))
# # # Contribution to the first dimension
# # fviz_contrib(res.pca, "var", axes = 1)
# # # Contribution to the second dimension
# # fviz_contrib(res.pca, "var", axes = 2

# #Do FAMD
# df.famd <- df[which(Year=="2017"),c(5,7,8,9,10,11)]
# #df.famd <- df[which(Year=="2017"),c(2,4,5,7,9,10,11)]
# res.famd <- FAMD(df.famd, ncp = 5, sup.var= NULL, ind.sup=NULL, graph=FALSE)
# # Plot of variables
# fviz_famd_var(res.famd, repel = TRUE)
# # Contribution to the first dimension
# fviz_contrib(res.famd, "var", axes = 1)
# # Contribution to the second dimension
# fviz_contrib(res.famd, "var", axes = 2)
# 
# fviz_screeplot(res.famd)
# fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#               repel = TRUE)
# 
# 
# fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#Remove obsolete variables
#rm(CarYear, CarRegion, CarPrice, CarMileage, CarConsumption)
#save(df,file="BilbasenData.rda")
