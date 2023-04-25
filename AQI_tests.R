# setting the appropriate working directory and reading the csv file
setwd("LING 460/Final Project")  ## this will be different for whoever is using this file
aqi <- read.csv("income_mortality_aqi.csv", header = TRUE, sep = ",")



##################################### SETTING UP THE SAMPLES ##################################################

# find the average number of Unhealthy.Days, Very.Unhealthy.Days, Hazardous.Days, 
# Median.AQI, deaths (rate_100k), and Max.AQI for each state (called "region" in the data)
aqiByState <- aggregate(cbind(Unhealthy.Days, Very.Unhealthy.Days, Hazardous.Days, Median.AQI, Max.AQI, rate_100k) ~ region, data = aqi, mean)
head(aqiByState)  # checking the aggregation - seems to have worked fine

# make lists of states in each region of the US - these will be used later to test for aqi by region
# information about the regions of the US are from this site: https://www.mappr.co/political-maps/us-regions-map/
west = c("California", "Colorado", "Nevada", "Hawaii", "Alaska", "Oregon", "Utah", "Idaho", "Montana", "Wyoming", "Washington")
midwest = c("Minnesota", "Wisconsin", "Illinois", "Ohio", "Indiana", "Michigan", "Missouri", "Iowa", "Kansas", "Nebraska", "North Dakota", "South Dakota")
southwest = c("New Mexico", "Arizona", "Oklahoma", "Texas")
southeast = c("Georgia", "North Carolina", "South Carolina", "Virginia", "West Virginia", "Kentucky", "Tennessee", "Mississippi", "Alabama", "Delaware", "Maryland", "Florida", "Louisiana", "Arkansas")
northeast = c("Massachusetts", "Rhode Island", "Connecticut", "Vermont", "New Hampshire", "Maine", "Pennsylvania", "New Jersey", "New York")

# getting subsets of the aqiByState data frame using the region vectors made above
aqiW <- aqiByState[aqiByState$region %in% west, ]
aqiMW <- aqiByState[aqiByState$region %in% midwest, ]
aqiSW <- aqiByState[aqiByState$region %in% southwest, ]
aqiSE <- aqiByState[aqiByState$region %in% southeast, ]
aqiNE <- aqiByState[aqiByState$region %in% northeast, ]

# checking one of the regions (all use the same formula so should be fine) - seems to have worked
head(aqiW)



################################# DOING THE ACTUAL TESTS #####################################################

# Using the above sub frames, we can perform a variety of t-tests
# sample sizes and variances are different, so I'm using Welch t-tests (implied by not setting a value for var.equal)
# I only listed a few t-tests here to run, but we can easily change up the arguments to test for different metrics in different regions

# comparing the number of hazardous days between the west and midwest regions
t.test(aqiW$Hazardous.Days, aqiMW$Hazardous.Days)
# p-value = 0.1269   ->  not statistically significant
# the differences in number of hazardous days between the two regions is not caused by spatial disparity

# comparing the number of deaths between the southwest and northeast regions
t.test(aqiSW$rate_100k, aqiNE$rate_100k)
# p-value = 0.1971   ->  not statistically significant
# the differences in number of deaths between the two regions is not caused by spatial disparity

# comparing the median aqi between the west and southeast regions
t.test(aqiW$Median.AQI, aqiSE$Median.AQI)
# p-value = 0.4825   ->  not statistically significant
# the differences in median air quality between the two regions is not caused by spatial disparity



############################### FINAL THOUGHTS ##############################################################
# Honestly, there does not seem to a significant difference in any of the fields across the regions.
# We can maybe make some conclusions saying that the lack of significant variance is probably because most 
# regions of the US have a similar number of big cities, farmland, etc, so any drastic differences in smaller
# sub-regions gets blurred when we look at only the five big regions.
# We could do a similar thing with smaller regions if we want to take a closer look


