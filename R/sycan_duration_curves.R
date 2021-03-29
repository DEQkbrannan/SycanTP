#Load and Flow Duration Curve Script for Sycan River At Drews Rd near Beatty, Oregon
#Flow data from OWRD gage #11499100, nutrient data from The Klamath Tribes
#Created by Olivia Stoken (Oregon DEQ) on 2/25/21


##setwd ("C:/Users/ostoken/Documents/Sycan BDA project/DEQ monitoring proposal/R")
setwd("C:/Users/kbranna/Documents/Committees/NPS-DW-TMDL_monitoring_subcommittee/SycanRiverProposal2021")
library(dplyr)
library(ggplot2)
library(lubridate)

mean_daily_flow <- read.csv (file="Sycan_mean_daily_flow_1973_2020.csv", header=TRUE)
nutrients <- read.csv (file="Sycan_TKT_data_2001_2020.csv", header=TRUE)

#Flow Duration Curve 1973-2020

FDC <- with(mean_daily_flow, data.frame(date = date, mean_daily_flow = mean_daily_flow_cfs, percentile=(100/length(mean_daily_flow_cfs)*1:length(mean_daily_flow_cfs))))

#Flow duration curve in base R
plot(x = FDC$percentile, y = FDC$mean_daily_flow, type = "l", log = "y",ylab="Discharge (cfs)",xlab="Flow Duration Interval (%)",main="Flow Duration Curve for Sycan River")
grid()

#Flow duration curve in ggplot2
ggplot(data=FDC, aes(x=percentile, y=mean_daily_flow)) + geom_line(size=1.5) + theme_bw() + scale_y_log10() +
  xlab("Flow Duration Interval (%)") + ylab("Discharge (cfs)") +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  geom_vline(xintercept = c(10,40,60,90), color = "black", size=1) +
  annotate("text", x = 3, y = 8000, label = "High Flows") + annotate("text",x = 25, y = 8000, label = "Moist Conditions") +
  annotate("text",x = 50, y = 8000, label = "Mid-range Flows") + annotate("text",x = 75, y = 8000, label = "Dry Conditions") +
  annotate("text",x = 98, y = 8000, label = "Low Flows")


#Flow Duration Curve 2001-2020 (to match TKT data)
FDC_2001 <- FDC
FDC_2001$date <- as.POSIXct(strptime(FDC_2001$date,"%m/%d/%Y"))
FDC_2001 <- filter(FDC_2001, date > "2000-12-31")

ggplot(data=FDC_2001, aes(x=percentile, y=mean_daily_flow)) + geom_line(size=1.5) + theme_bw() + scale_y_log10() +
  xlab("Flow Duration Interval (%)") + ylab("Discharge (cfs)") +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  geom_vline(xintercept = c(10,40,60,90), color = "black", size=1) +
  annotate("text", x = 3, y = 3000, label = "High Flows") + annotate("text",x = 25, y = 3000, label = "Moist Conditions") +
  annotate("text",x = 50, y = 3000, label = "Mid-range Flows") + annotate("text",x = 75, y = 3000, label = "Dry Conditions") +
  annotate("text",x = 98, y = 3000, label = "Low Flows")


#Load Duration Curve for Total Phosphorus
#TP variable is pounds/per day calculated based on guidance from EPA 2007 document and 66 micrograms/L TP surrogate measure from UKL drainage TMDL
LDC_TP <- mutate(FDC, TP = mean_daily_flow*66*0.005393)

#LDC in base R
plot(x = LDC_TP$percentile, y = LDC_TP$TP, type = "l", log = "y",ylab="Total Phosphorus (pounds per day)",xlab="Flow Duration Interval (%)",main="Total Phosphorus Load Duration Curve for Sycan River")
grid()

#Join data sets to add instantaneous TP load and update Load Duration Curve with daily TP points
Sycan_TP <- right_join(FDC, nutrients, by = "date")
Sycan_TP <- select(Sycan_TP, date, YR, mean_daily_flow, percentile, TP_microg_L)
Sycan_TP <- mutate(Sycan_TP, TP_load = TP_microg_L*mean_daily_flow*0.005393)
Sycan_TP <- rename(Sycan_TP, Year=YR)
Sycan_TP <- mutate(Sycan_TP, above_TMDL=if_else(TP_microg_L <= 66, "No","Yes"))
Sycan_TP <- mutate(Sycan_TP, adjudication=if_else(Year <= 2013, "Pre","Post"))

#LDC with points in base R
plot(x = LDC_TP$percentile, y = LDC_TP$TP, type = "l", log = "y",ylab="Total Phosphorus (pounds per day)",xlab="Flow Duration Interval (%)",main="Total Phosphorus Load Duration Curve for Sycan River")
points(x=Sycan_TP$percentile, y =Sycan_TP$TP_load)
grid()

#LDC with points in ggplot2 with points color above/below TMDL criteria
ggplot(data=LDC_TP, aes(x=percentile, y=TP)) + geom_line(size=1) + theme_bw() + scale_y_log10() +
    xlab("Flow Duration Interval (%)") + ylab("Total Phosphorus (pounds per day)") +
    scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
    geom_point(data=Sycan_TP, aes(x=percentile, y=TP_load, color=above_TMDL), size=4) +
    scale_color_manual(values=c("grey","black")) +
    geom_vline(xintercept = c(10,40,60,90), color = "black", size=1) +
    annotate("text", x = 5, y = 1500, label = "High Flows") + annotate("text",x = 25, y = 1500, label = "Moist Conditions") +
    annotate("text",x = 50, y = 1500, label = "Mid-range Flows") + annotate("text",x = 75, y = 1500, label = "Dry Conditions") +
    annotate("text",x = 95, y = 1500, label = "Low Flows") +
    theme(legend.position = "bottom")

#LDC with points color gradient by year
ggplot(data=LDC_TP, aes(x=percentile, y=TP)) + geom_line(size=1) + theme_bw() + scale_y_log10() +
  xlab("Flow Duration Interval (%)") + ylab("Total Phosphorus (pounds per day)") +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_point(data=Sycan_TP, aes(x=percentile, y=TP_load, color=Year), size=4) +
  geom_vline(xintercept = c(10,40,60,90), color = "black", size=1) +
  annotate("text", x = 5, y = 1500, label = "High Flows") + annotate("text",x = 25, y = 1500, label = "Moist Conditions") +
  annotate("text",x = 50, y = 1500, label = "Mid-range Flows") + annotate("text",x = 75, y = 1500, label = "Dry Conditions") +
  annotate("text",x = 95, y = 1500, label = "Low Flows") +
  theme(legend.position = "bottom")

#LDC with points color by pre/post adjudication
##Note that 2017 was first year of full water right calls, 2013-2016 were partial calls
ggplot(data=LDC_TP, aes(x=percentile, y=TP)) + geom_line(size=1) + theme_bw() + scale_y_log10() +
  xlab("Flow Duration Interval (%)") + ylab("Total Phosphorus (pounds per day)") +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  geom_point(data=Sycan_TP, aes(x=percentile, y=TP_load, color=adjudication), size=4) +
  scale_color_manual(values=c("darkblue","lightblue")) +
  geom_vline(xintercept = c(10,40,60,90), color = "black", size=1) +
  annotate("text", x = 5, y = 1500, label = "High Flows") + annotate("text",x = 25, y = 1500, label = "Moist Conditions") +
  annotate("text",x = 50, y = 1500, label = "Mid-range Flows") + annotate("text",x = 75, y = 1500, label = "Dry Conditions") +
  annotate("text",x = 95, y = 1500, label = "Low Flows") +
  theme(legend.position = "bottom")


#Linear regression model of TP load vs time
Sycan_lm <- Sycan_TP
Sycan_lm$date <- as.POSIXct(strptime(Sycan_lm$date,"%m/%d/%Y"))

lm_TP_load = lm(TP_load~date, data = Sycan_lm)

#Summary to show statistical relationship, no relationship based on date
summary(lm_TP_load)

#Plot of daily TP load vs date
ggplot(data=Sycan_lm, aes(x=date, y=TP_load, color=above_TMDL)) + geom_point(size=3) + theme_bw() +
  scale_color_manual(values=c("grey","black")) +
  xlab("Date") + ylab("Total Phosphorus Load (pounds per day)")+
  scale_x_datetime(name="Year", date_breaks = "2 years")

#code to add linear model to ggplot figures
##geom_smooth(method = "lm")
