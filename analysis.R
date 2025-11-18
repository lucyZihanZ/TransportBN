# 
install.packages("stats19")
library(stats19)

# Here we analyze three different parts 
casualties = get_stats19(year = 2017, type = "casualties", ask = FALSE)
write.csv(casualties, "Casualties2017.csv", row.names = FALSE)
#> Files identified: dftRoadSafetyData_Casualties_2017.zip
#>    http://data.dft.gov.uk.s3.amazonaws.com/road-accidents-safety-data/dftRoadSafetyData_Casualties_2017.zip
#> Attempt downloading from:
#> Data saved at /tmp/RtmpDI4lPA/dftRoadSafetyData_Casualties_2017/Cas.csv
nrow(casualties)
#> [1] 170993
ncol(casualties)
#> [1] 16
sapply(casualties, function(col) sum(is.na(col)))

vehicles = get_stats19(year = 2017, type = "vehicles", ask = FALSE)
write.csv(casualties, "Casualties2017.csv", row.names = FALSE)
names(vehicles)
nrow(vehicles)
