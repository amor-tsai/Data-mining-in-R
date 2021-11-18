load("rain_small.RData")

head(rain)

rain2 <- na.omit(rain)
head(rain2)

# remove duplicated rows
 
#idxDup <- which(duplicated(rain2)) # returns index of rows with duplicates
#rain3 <- rain2[-idxDup] #remove duplicated data

which(duplicated(rain2))

hist(rain2$Expected,
     xlab = "Rain Amount (mm/hr)",
     ylab = "Frequency",
     main = "Histogram for Rainfall Amount",
     ylim = c(0,1000))

summary(rain2$Expected)

# Remove rows with "Expected" larger than 305 mm/hr
idxInvalid <- which(rain2$Expected > 305)
idxInvalid 

rain3 <- rain2[-idxInvalid,]

# Aggregates rows -> 1 row(observation) per ID
?aggregate
rain4 <- aggregate(. ~ Id, rain3, FUN = median)

# Categorize Expected by the rainfall amount
rainType <- cut(rain4$Expected, 
    c(0,4,10,305),
    labels = c("moderate","heavy","violent"))

rainType[1:10]

# bar chart for rain type
rainGroup <- table(rainType)
rainGroup

barplot(rainGroup, 
        main = "Frequency per Rain Type")

# box plot for each rain type
boxplot(rain4$Expected ~ rainType,
        ylab = "Rainfall Range",
        ylim = c(0,60))

# resampling
# randomly select 100 instances fro each rain type

#install.packages("sampling")
library(sampling)

?strata

rain5 <- cbind(rain4, rainType) # combine dataset with column rainType
temp <- strata(rain5,
               stratanames = "rainType",
               size = c(100,100,100))
temp

idxEqualSample <- temp$ID_unit

rain6 <- rain5[idxEqualSample,] # 300 rows: 100 observations for each rain type



