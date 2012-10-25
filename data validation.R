setwd("/Users/salahchafik/Desktop")
getwd()
library('ggplot2')
library('plyr')
library('doBy')

data_validation <- read.csv("data_validation.csv")
summary(data_validation$Official.LGA..s)
data_validation$Official.LGA..s <- as.numeric(data_validation$Official.LGA..s)

#fixing one LGA that is NA
##summary(lga_wide$Mean_Disc_Ratio)
##Min.     1st Qu. Median  Mean    3rd Qu. Max.       NA's 
## 0.0000  0.1277  0.2659  0.2799  0.3729  0.9699       1 M
data_validation$NMIS[is.na(data_validation$NMIS)] <- 0 

#making data numeric and creating discrepancy
summary(data_validation$NMIS)
data_validation$NMIS <- as.numeric(data_validation$NMIS)
data_validation$Discrepancy <- abs(abs(data_validation$Official.LGA..s) - abs(data_validation$NMIS))
data_validation$FractionDiscrepancy <- data_validation$Discrepancy / (data_validation$Official.LGA..s)

#dropping rows where discrepancy is 0
dv2 <- subset(data_validation, subset=(abs(Discrepancy)>0))

#totaling facilities by LGA
lga_wide <- ddply(dv2, .(LGA), summarize, sum(Discrepancy), sum(NMIS), sum(Official.LGA..s))
names(lga_wide) <- c("LGA", "Discrepancy_Total", "Total_NMIS", "Total_official")
lga_wide$Mean_Disc_Ratio <- lga_wide$Discrepancy_Total / lga_wide$Total_official


#plotting discrepancy
summary(lga_wide$Mean_Disc_Ratio)
qplot(abs(lga_wide$Mean_Disc_Ratio))
str(lga_wide)


#plotting coverage
lga_wide$NMIS_coverage <- lga_wide$Total_NMIS / lga_wide$Total_official 
summary(lga_wide$NMIS_coverage)
qplot(abs(lga_wide$NMIS_coverage))



