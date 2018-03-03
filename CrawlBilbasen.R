# A web scraper for bilbasen.dk
# by Frederik Holmelund Kjærskov
# March 2018

# Load web scraping library
library('rvest') # web scraper

#Get data from URL
URLtoCrawl <- "https://www.bilbasen.dk/brugt/bil/vw?fuel=0&yearfrom=2010&yearto=2010&pricefrom=0&priceto=10000000&mileagefrom=-1&mileageto=10000001&zipcode=0000&includeengroscvr=false&includesellforcustomer=true&includewithoutvehicleregistrationtax=false&includeleasing=false&page=1"
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