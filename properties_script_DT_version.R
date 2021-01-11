# Explore the Philadelphia Properties Dataset
# Predict home values with regression techniques
# Load packages
library(data.table) # Fast data manipulation
library(tidyverse) # plotting, functional programming, factor and date functions
library(caret) # modeling 
library(sf) # GIS tools
# 1 - Data Cleaning --------------------------------------------------------------------------------
# Load 
# Properties contains the housing data, crime contains the crime data, District contains school catchment
properties_SF.Trim <- fread('Econometrics Project\\opa_properties_23.10.19.csv')
crime_dt <- fread('Econometrics Project\\arrest_data_daily_by_district_csv.csv')
school_dt <- fread('Econometrics Project\\School.Data.Trimmed.csv')


# Focusing on Properties first
# Filter the data
properties_SF.Trim <- properties_SF.Trim[category_code_description == 'Single Family']

# Will attempt to see if there is any visible trend between a given variable and market value (the response variable)
# Fist will see the class of the variables
variable_class <- properties_SF.Trim[, lapply(.SD, class)]
variable_class <- variable_class %>% 
  gather(key = 'Variable', value = 'Class')

# Most columns are characters. Many categorical data columns are listed as integers, not factors
# For a description on why an individual variable was removed, see project notes
# This is a first pass - may remove more later as I get further in the analysis
Delete_Cols <- variable_class$Variable[c(2, 3, 4, 10, 11, 12, 14, 15, 18, 19, 22, 
                                         24, 25, 26, 29, 30, 31, 32, 33, 34, 
                                         36, 41, 42, 44, 46, 47, 48, 49, 50, 51, 52,
                                         53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 
                                         66, 67, 68, 69)] 
# Remove The Columns
properties_SF.Trim <- properties_SF.Trim[, !..Delete_Cols]

# Crimes Data
# classify crimes and aggregate by police district. 
# Using FBI's UCR Standard 
crime_dt[, Type1.Crimes := Homicide + Rape + `Robbery/Gun` + `Robbery/Other` + 
           `Aggravated Assault/Gun` + `Aggravated Assault/Other` + `Burglary/Residential` +
           `Burglary/Commercial` + `Theft of Motor Vehicle Tag` + `Theft from Person` +
           `Theft from Auto` + `Retail Theft` + Theft + `Auto Theft`][, 
            Type2.Crimes := `Drug Possession` + `Drug Sales` + DUI + `All Other Offenses`][, 
            'date_formatted' := as.Date(date_value, 
            format = '%m/%d/%Y')][, 'Year.Formatted' := lubridate::year(date_formatted)]

# Load the PPD Shapefile
# Will use this to assign properties to their Police District
PPD.Districts <- read_sf('Econometrics Project\\PPD.Boundaries')
PPD.Districts <- st_as_sf(PPD.Districts)
PPD.Districts <- st_transform(PPD.Districts, crs = 4326)

# Two districts have since been merged - accounting for that here
crime_dt <- crime_dt[, dc_district := as_factor(dc_district)][, 
           dc_district := fct_collapse(dc_district, `3` = '4', `22` = '23')][
           !(dc_district %in% c(71))]

# Aggregate and Summarise by Year and District
crimeSummary <- crime_dt[, .(Total.Crimes = sum(Type1.Crimes, Type2.Crimes),
                Total.Type1 = sum(Type1.Crimes), Total.Type2 = sum(Type2.Crimes)), 
                by = .(Year.Formatted, dc_district)]

# Compute Rates
crimeSummary.Melt <- crimeSummary %>% 
  pivot_wider(names_from = Year.Formatted, values_from = c(Total.Crimes, Total.Type1, Total.Type2))
crimeSummary.Melt <- setDT(crimeSummary.Melt)
crimeDailyRates <- crimeSummary.Melt[, .(Police.District = dc_district, 
                                         `2014Type1.Rate` = Total.Type1_2014 / 365, 
                                         `2015Type1.Rate` = Total.Type1_2015 / 365,
                                         `2016Type1.Rate` = Total.Type1_2016 / 366,
                                         `2017Type1.Rate` = Total.Type1_2017 / 365,
                                         `2018Type1.Rate` = Total.Type1_2018 / 365,
                                         `2019Type1.Rate` = Total.Type1_2019 / 296,
                                         `2014Type2.Rate` = Total.Type2_2014 / 365,
                                         `2015Type2.Rate` = Total.Type2_2015 / 365,
                                         `2016Type2.Rate` = Total.Type2_2016 / 366,
                                         `2017Type2.Rate` = Total.Type2_2017 / 365,
                                         `2018Type2.Rate` = Total.Type2_2018 / 365,
                                         `2019Type2.Rate` = Total.Type2_2019 / 296)
                                     ][, c('Type1.WeightedAvg', 'Type2.WeightedAvg') :=
                                         .(0.05 * `2014Type1.Rate` + 0.1 * `2015Type1.Rate` +
                                           0.15 * `2016Type1.Rate` + 0.2 * `2017Type1.Rate` + 
                                           0.225 * `2018Type1.Rate` + 0.275 * `2019Type1.Rate`, 
                                           0.05 * `2014Type2.Rate` + 0.1 * `2015Type2.Rate` +
                                           0.15 * `2016Type2.Rate` + 0.2 * `2017Type2.Rate` + 
                                           0.225 * `2018Type2.Rate` + 0.275 * `2019Type2.Rate`
                                           )
                                     ]

# Reduce to just the variables going into the properties dataset
crimeDailyRates <- crimeDailyRates[, .(Police.District, Type1.WeightedAvg, Type2.WeightedAvg)]

# School Data
ES.Catchment.Area <- read_sf('Econometrics Project\\Catchment_ES_2017-18')

# ST_as_SF ensures that the Catchment is an SF object
# Will enable merging later
ES.Catchment.Area <- st_as_sf(ES.Catchment.Area, crs = 4326)
ES.Catchment.Area <- st_transform(ES.Catchment.Area, crs = 4326)

# Trim the School Performance Data
# This includes more than just public schools, but all that's needed are the performance metrics
school_dt <- school_dt[, c(3, 22)]


# 2 - EDA ----------------------------------------------------------------------------------------
# Distribution of Market Values is Strongly Skewed to the Right
# Taking the log may normalize the data - will add that and compare the distribution
properties_SF.Trim[, Log.MktVal := log(market_value)]


# Taking the log resulted in some -Inf's. Inspection revealed that these observations had market values of 0
# Will isolate them here
Zero.MktVal <- properties_SF.Trim[Log.MktVal == -Inf]

# There are 69 properties here with market values of 0
# Will remove these
properties_SF.Trim <- properties_SF.Trim[!is.infinite(properties_SF.Trim$Log.MktVal)]


# Adding a variable for Percentile rank will help with EDA
properties_SF.Trim[, 
  c('Percentile.Rank', 
    'Log.Percentile.Rank') := .(ntile(market_value, n = 100), ntile(Log.MktVal, n = 100))]

# Removing market value outliers - these were clearly mistakes
properties_SF.Trim <- properties_SF.Trim[market_value < 100000000][
  !is.na(market_value)
]

# Will look at the other variables now
Numeric.Quantiles <- sapply(select_if(properties_SF.Trim, is.numeric), 
  quantile, probs = seq(0, 1, 0.02), na.rm = TRUE)


# Number of rooms 
# Almost none of these align with the number of bathrooms + number of bedrooms
# Unclear how a 'room' is defined here
properties_SF.Trim[, number_of_rooms := NULL]

# Now depth
# Will remove this variable 
properties_SF.Trim[, depth := NULL]

# Garage Spaces
# This variable misrepresents garage spaces for condo's
# Will list a condo as having 30 spaces, as though it were a 30 car garage
properties_SF.Trim[, garage_spaces := NULL]
properties_SF.Trim[, garage_type := NULL]

# Number of Bathrooms and Bedrooms
# Both have 100K observations with 0 bedrooms or bathrooms
properties_SF.Trim <- properties_SF.Trim[!(location %in% c('900 S 12TH ST', '812 S 13TH ST'))]
properties_SF.Trim[, c('number_of_bathrooms', 'number_of_bedrooms') := NULL]

# Number of Stories
# 100K observations with 0 stories
properties_SF.Trim[, number_stories := NULL]

# Basements
# 1/3rd missing values, will remove
properties_SF.Trim[, basements := NULL]

# Total Area
# Some properties that have a listed area of zero
# Isolating on those reveals that many observations have total area of zero but non zero total livable area
# Will remove total area
properties_SF.Trim[, total_area := NULL]

# Total Livable Area
# 526 observations that show as having an area of zero
# Will remove these
# Some properties with very high total livable areas are not single family homes
# Many are housing for religious societies (Convents)
# Others are clearly mistakes
properties_SF.Trim <- properties_SF.Trim[total_livable_area > 0]
properties_SF.Trim <- properties_SF.Trim[
  !owner_1 %in% c('REV DENNIS J DOUGHERTY', 'DOUGHERTY DENNIS J', 'PHILADELPHIA UNIVERSITY', 
                  'DENNIS J DOUGHERTY	', 'TALMUDICAL YESHIVA OF PHI', "SAINT JOSEPH'S UNIVERSITY",
                  'SAINT JOSEPHS UNIVERSITY', 'CHURCH CHRISTIAN COMPASSI', 'CHRIST EMPOWERMENT TEMPLE',
                  'ST JAMES CATHOLIC', 'CARMELITE CONVENT OF', '	1439-61 NORTH 31ST STREET', 
                  'UNITY MISSION CHURCH', 'KWAN UM SA BUDDHIST', 'SECOND BAPTIST CHURCH',
                  'UNITED COMMUNITIES', 'SOMERSET LLC', 'INTERCOMMUNITY ACTION INC', 
                  'REV DENNIS DOUGHERTY', 'INDONESIA BETHEL CHURCH B', 'DARE TO IMAGINE CHURCH IN',
                  'FRIENDS BEHAVORIAL HEALTH', 'CHOICE ACADEMICS INC', 'FRIENDS REHABILITATION PR',
                  'ARCHBISHOP OF PHILADELPHI', 'HOLMESBURG BAPTIST CHURCH', 'DOMINICAN FATHERS & BROTH',
                  'TALMUDICAL YESHIVA OF PHI', 'JAMESON EVANGELISTIC', '1439-61 NORTH 31ST STREET')]
                  

# Removing Philadelphia Housing Authority, Redevelopment Authority, and Land Bank houses
properties_SF.Trim <- properties_SF.Trim[!(owner_1 %in% 
                                       c('PHILADELPHIA HOUSING AUTH', 'PHILA HOUSING AUTHORITY', 
                                       'PHILADELPHIA LAND BANK', 'PHILADELPHIA HOUSING', 
                                       'PHILADELPHIA REDEVELOPMEN', 'PHILA REDEVELOPMENT AUTH',
                                       'PHILA HOUSING AUTH', 'PHILA REDEVELOPMENT'))]

# Removing Condo Parking Spaces and Residential Air Rights
properties_SF.Trim <- properties_SF.Trim[
  !(building_code_description) %in% c('CONDO PARKING SPACE', 'AIR RIGHTS RESIDENTIAL')]

# Removing NA's and 0's in Exterior and Interior Condition
properties_SF.Trim <- properties_SF.Trim[
  !is.na(exterior_condition)][
  !is.na(interior_condition)][!(interior_condition %in% 0)][
  !(exterior_condition %in% 0)]

# Removing Observations with Year Built of '0'
properties_SF.Trim <- properties_SF.Trim[!(year_built) %in% c('0')]

# Removing Observations with Missing Lat/Long
properties_SF.Trim <- properties_SF.Trim[!is.na(lng)]

# Most of the remaining non ID variables will go in the model

# 3 - Transformation ----------------------------------------------------------------------------
# Recode Categorical Vars as factors/numeric where applicable
# Exterior Condition
properties_SF.Trim[, exterior_condition := as_factor(exterior_condition)]
properties_SF.Trim[, exterior_condition := fct_recode(exterior_condition, 
                                     '7' = '1', '6' = '2', '5' = '3',
                                     '4' = '4', '3' = '5', 
                                     '2' = '6', '1' = '7')]
properties_SF.Trim[,
    exterior_condition := as.numeric(as.character(properties_SF.Trim$exterior_condition))]

# Interior Condition
properties_SF.Trim[, interior_condition := as_factor(interior_condition)][
  , interior_condition := fct_recode(interior_condition,
                                     '7' = '1', '6' = '2', '5' = '3',
                                     '4' = '4', '3' = '5', 
                                     '2' = '6', '1' = '7')]
properties_SF.Trim[,
    interior_condition := as.numeric(as.character(properties_SF.Trim$interior_condition))]

# View Type
properties_SF.Trim[, c('view_type') := 
                     lapply(.SD, as_factor), .SDcols = c('view_type')]

# Add the distance variable - Using City Hall as a proxy for 'downtown' distance
Properties.Long.Lat <- properties_SF.Trim[, .(objectid, lng, lat)]
# Compute Distance
properties_SF.Trim[, 
  City.Hall.Distance := geosphere::distHaversine(Properties.Long.Lat[, c(2:3)], 
                                                 c(-75.1635112, 39.952335), r = 3963.1905919)]

# To add in Police District and School Catchment, I will create an SF object with the Long.Lat DT above
Properties.Long.Lat <- st_as_sf(Properties.Long.Lat, coords = c('lng', 'lat'), crs = 4326)

# Apply Point in Polygon Algorithm to See which Police District each address belongs to
PD.Intersection <- st_intersection(Properties.Long.Lat, PPD.Districts)
st_geometry(PD.Intersection) <- NULL
PD.Intersection <- PD.Intersection[, c(1, 5)]

# Merge
properties_SF.Trim <- merge(properties_SF.Trim, PD.Intersection, by = 'objectid')
# This appears to have worked 
# Rename and set as factor
setnames(properties_SF.Trim, old = 'DISTRICT_', new = 'Police.District')
properties_SF.Trim[, Police.District := as_factor(Police.District)]

# Add in the Crime Data
properties_SF.Trim <- merge(properties_SF.Trim, crimeDailyRates, by = 'Police.District')

# Repeating For Elementary Schools
ES.Intersection <- st_intersection(Properties.Long.Lat, ES.Catchment.Area)
st_geometry(ES.Intersection) <- NULL
ES.Intersection <- ES.Intersection[, c(1, 2, 4)]

# Merge with Intersection DT
properties_SF.Trim <- merge(properties_SF.Trim, ES.Intersection, by = 'objectid')

# Merge with Performance Data
# Repeating the ULCS code to make merging easier
colnames(school_dt) <- c('ES_ID', 'School.Score')
school_dt[, ES_ID := as.character(school_dt$ES_ID)]
properties_SF.Trim <- merge(properties_SF.Trim, school_dt, by = 'ES_ID')
properties_SF.Trim[, School.Score := as.numeric(properties_SF.Trim$School.Score)]


# 4 - Model -------------------------------------------------------------------------------
# Running Descriptive Stats
# Measures of Centrality
MktVal.Centrality.Final <- data.table(
  Mean = mean(properties_SF.Trim$market_value, na.rm = TRUE),
  `Trimmed Mean 5%` = mean(properties_SF.Trim$market_value, na.rm = TRUE, trim = 0.05),
  `Trimmed Mean 10%` = mean(properties_SF.Trim$market_value, na.rm = TRUE, trim = 0.1),
  Median = median(properties_SF.Trim$market_value, na.rm = TRUE)
)
Log.MktVal.Centrality.Final <- data.table(
  Mean = mean(properties_SF.Trim$Log.MktVal, na.rm = TRUE),
  `Trimmed Mean 5%` = mean(properties_SF.Trim$Log.MktVal, na.rm = TRUE, trim = 0.05),
  `Trimmed Mean 10%` = mean(properties_SF.Trim$Log.MktVal, na.rm = TRUE, trim = 0.1),
  Median = median(properties_SF.Trim$Log.MktVal, na.rm = TRUE)
)
# Exponentiate back the log tranformed centrality measures
Exp.Centrality.Final <- map_df(Log.MktVal.Centrality.Final, exp)

# Combine Results
Centrality.DT.Final <- bind_rows(
  MktVal.Centrality.Final,
  Log.MktVal.Centrality.Final,
  Exp.Centrality.Final
) %>% 
  map_df(~round(., 2)) %>% 
  mutate(Distribution = c('Market Values', 'Log Transformed Market Values', 'Exponentiated Log Values')) %>% 
  select(5, 1:4)

# Measures of variability
MktVal.Variability.Final <- data.table(
  `Standard Deviation` = sd(properties_SF.Trim$market_value, na.rm = TRUE),
  `Median Absolute Deviation` = mad(properties_SF.Trim$market_value, na.rm = TRUE),
  `Interquartile Range` = IQR(properties_SF.Trim$market_value, na.rm = TRUE),
  Range = range(properties_SF.Trim$market_value, na.rm = TRUE)[2] - range(properties_SF.Trim$market_value, na.rm = TRUE)[1]
) %>% map_df(~round(., 2))

# Split Data into Test and Train
properties.Model <- properties_SF.Trim[
  , .(objectid, exterior_condition, interior_condition, total_livable_area, year_built, view_type,
      Log.MktVal, City.Hall.Distance, Type1.WeightedAvg, Type2.WeightedAvg, ES_Short, School.Score)][
  , total_livable_area := log(total_livable_area)
      ]

in.train <- createDataPartition(properties.Model$year_built, p = 0.8, list = FALSE, times = 1)

properties.Train <- properties.Model[in.train]
properties.Test <- properties.Model[-in.train]

# Extracting the original market values and object ID's to merge back and calculate residuals
Market.Value.DT <- properties_SF.Trim[, .(objectid, market_value)]

# Test correlations
Numeric.Correlation <- data.frame(Correlation = sapply(select_if(properties.Model, is.numeric), 
       cor, y = properties.Model$Log.MktVal, use = 'complete.obs'))

# Linear Model
set.seed(11202019)
LM.Params <- trainControl(method = "boot", number = 25)
Linear.Model <- train(
  x = properties.Train[, c(2, 3, 4, 6, 8, 9, 10, 11, 12)],
  y = properties.Train$Log.MktVal,
  method = 'lm',
  trControl = LM.Params
)

# Add in Residuals
properties.Train <- properties.Train %>% 
  modelr::add_predictions(Linear.Model, var = 'Log.LM.Predictions') %>% 
  left_join(y = Market.Value.DT, by = 'objectid') %>% 
  mutate(Scaled.LM.Predictions = exp(Log.LM.Predictions),
         LM.Residuals = market_value - Scaled.LM.Predictions)


# Random Forest
RF.Model <- ranger::ranger(Log.MktVal ~ total_livable_area + exterior_condition + interior_condition +
                           City.Hall.Distance + Type1.WeightedAvg + Type2.WeightedAvg + School.Score,
                           data = properties.Train,
                           num.trees = 500, mtry = 4)


# Add predictions and residuals
setDT(properties.Train)
properties.Train[, c('Log.RF.Predictions') := 
                   RF.Model$predictions,][,
  c('Scaled.RF.Predictions') := 
      .(exp(Log.RF.Predictions))][,
  RF.Residuals := market_value - Scaled.RF.Predictions
  ]


# Consolidate Training Results in a Table for Presentation
Training.Preds <- data.table(
  objectid = properties.Train$objectid,
  Scaled.LM.Predictions = properties.Train$Scaled.LM.Predictions,
  Sclaed.RF.Predictions = properties.Train$Scaled.RF.Predictions
)


Training.Preds <- merge(Training.Preds, Market.Value.DT, by = 'objectid')

Training.Accuracy <- data.frame(
  RMSE = map_dbl(Training.Preds[, c(2:3)], ~ RMSE(., Training.Preds$market_value)),
  MAE = map_dbl(Training.Preds[, c(2:3)], ~ MAE(., Training.Preds$market_value))
) %>% 
  map_dfc(~round(.)) %>% 
  mutate(
    Model = c('Linear Model', 'Random Forest')
  ) %>% 
  select(3, 1, 2)


# Consolidate and calculate residuals for Test Accuracy
# Add in Market Value Test DT
# Test Predictions
properties.Test <- merge(properties.Test, Market.Value.DT, by = 'objectid')
properties.Test[,
 ':=' ('Log.LM.Predictions' = predict(Linear.Model, properties.Test), 
      'Log.RF.Predictions' = predict(RF.Model, data = properties.Test)$predictions)][, 
      c('Scaled.LM.Predictions', 'Scaled.RF.Predictions') 
      := lapply(.SD, exp), 
      .SDcols = c('Log.LM.Predictions', 'Log.RF.Predictions')][, 
      c('LM.Resid', 'RF.Resid') := market_value - .SD, 
    .SDcols = c('Scaled.LM.Predictions', 'Scaled.RF.Predictions')]


# Calculate Accuracy
Test.Accuracy <- data.frame(
  RMSE = map_dbl(properties.Test[, c(16, 17)], ~ RMSE(., properties.Test$market_value)),
  MAE = map_dbl(properties.Test[, c(16, 17)], ~ MAE(., properties.Test$market_value))
) %>% 
  map_dfc(~round(.)) %>% 
  mutate(Model = c(
    'Linear Model', 'Random Forest')) %>% 
  select(3, 1:2)



