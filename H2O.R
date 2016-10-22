library(h2o)
h2o.init()
# importing csv file
raw <- h2o.importFile(path = "/Users/hinagandhi/desktop/airlines_all.csv", parse=FALSE)
# to parse the data
setup <- h2o.parseSetup(raw)
# setting Airtime and AirDelay column names as numeric
setup$column_types[which(setup$column_names %in% "AirTime")]  <- "Numeric"
setup$column_types[which(setup$column_names %in% "AirDelay")] <- "Numeric"
# 
airlines.hex <- h2o.parseRaw(raw, col.types=setup$column_types)

## Look at the distribution of flights per Year, per Month
h2o.hist(airlines.hex$Year)

## Flight by Month calculated using H2O's fast groupby
print("Splitting data into groups of 12 month and aggregating on two columns...")
flightByMonth   <- h2o.group_by(data = airlines.hex, by = "Month",  sum("Cancelled"))
flightByMonth.R <- as.data.frame(flightByMonth)

## Set Column Type for Enumerator or Factor Columns
airlines.hex$Year      <- as.factor(airlines.hex$Year)
airlines.hex$Month     <- as.factor(airlines.hex$Month)
airlines.hex$DayOfWeek <- as.factor(airlines.hex$DayOfWeek)
airlines.hex$Cancelled <- as.factor(airlines.hex$Cancelled)
airlines.hex$IsDepDelayed <- as.factor(airlines.hex$IsDepDelayed)
## calculating arrival time in minutes
hour1 <- airlines.hex$CRSArrTime %/% 100
mins1 <- airlines.hex$CRSArrTime %% 100
arrTime <- hour1*60+mins1
## calculating departure time in minutes
hour2 <- airlines.hex$CRSDepTime %/% 100
mins2 <- airlines.hex$CRSDepTime %% 100
depTime <- hour2*60+mins2
## calculating travel time = arrrival time - departure time
travelTime <- ifelse(arrTime - depTime > 0, arrTime - depTime, NA)
# setting Travel Time 
airlines.hex$TravelTime <- travelTime
scatter_plot(airlines.hex, "Distance", "TravelTime")

#####################################################################################################################

## Create test/train split
data.split <- h2o.splitFrame(data = airlines.hex, ratios = 0.75)
data.train <- data.split[[1]]
data.test <- data.split[[2]]

# Set predictor and response variables
myY <- "IsDepDelayed"
myX <- c("Origin", "Dest", "Year", "UniqueCarrier", "DayOfWeek",  "Distance", "FlightNum")

## Build GBM Model
start    <- Sys.time()
data.gbm <- h2o.gbm(y = myY, x = myX, balance_classes = T, training_frame = data.train, validation_frame = data.test,
                    ntrees = 100, max_depth = 5, model_id = "gbm_model", distribution = "bernoulli", learn_rate = .1,
                    min_rows = 2)
h2o.rmse(data.glm)
gbm_time <- Sys.time() - start
print(paste("Took", round(gbm_time, digits = 2), units(gbm_time), "to build a GBM model."))

## Build Random Forest Model
start    <- Sys.time()
data.drf <- h2o.randomForest(y = myY, x = myX, training_frame = data.train, validation_frame = data.test, ntrees = 150,
                             max_depth = 5, model_id = "drf_model", balance_classes = T)
h2o.rmse(data.drf)
drf_time <- Sys.time() - start
print(paste("Took", round(drf_time, digits = 2), units(drf_time), "to build a Random Forest model."))

