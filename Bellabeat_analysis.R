library(tidyverse)
library(lubridate)

daily_activity <- read.csv("dailyActivity_merged.csv")
daily_activity <- filter(daily_activity,TotalSteps>0)
sleep_day <- read.csv("sleepDay_merged.csv")
hour_cal <- read.csv("hourlyCalories_merged.csv")
hour_step <- read.csv("hourlySteps_merged.csv")
hour_intensity <- read.csv("hourlyIntensities_merged.csv")
hourly <- data.frame(hour_step, Calories = hour_cal$Calories, Intensity = hour_intensity$TotalIntensity, AvgIntensity =hour_intensity$AverageIntensity)
View(daily_activity)
View(sleep_day)
View(hourly)

#Check for and eliminate duplicates
sleep_day <- sleep_day %>%
  distinct() %>%
  drop_na()

daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

hourly <- hourly %>%
  distinct() %>%
  drop_na()

#strip reformat the %Day columns for uniformity
daily_activity$ActivityDate <- as.Date(mdy(daily_activity$ActivityDate))
sleep_day$SleepDay <-as.Date(mdy_hms(sleep_day$SleepDay))
hourly$time = as.POSIXct(hourly$ActivityHour, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone())
hourly$ActivityHour = substr(hourly$time,11,22)
hourly$ActivityDay <- substr(hourly$time, 1, 10)

#Create a new ID and Date column to index dataframes
daily_activity$ID <- paste(daily_activity$Id, daily_activity$ActivityDate)
sleep_day$ID <- paste(sleep_day$Id, sleep_day$SleepDay)
hourly$ID <- paste(hourly$Id, hourly$ActivityDay)

#Look at calorie expenditure by category (low, mean, high)
daily_activity <- data.frame(daily_activity, Category = 0)
hourly <- data.frame(hourly, Category = 0)
 a <- as.numeric(quantile(daily_activity$Calories, prob = c(.10, .90))[2])
 b <- as.numeric(quantile(daily_activity$Calories, prob = c(.10, .90))[1])

for (i in 1:nrow(daily_activity)) {
   if (daily_activity$Calories[i] >= a) {
     daily_activity$Category[i]<- "High"
   } else if (daily_activity$Calories[i] <= b) {
     daily_activity$Category[i] <- "Low"
   } else {
     daily_activity$Category[i] <- "Intermediate"
   }}

 

for (i in 1:nrow(hourly)) {
  x <- which(daily_activity$ID == hourly$ID[i])
  if (length(x)!=0) {
    hourly$Category[i] <- daily_activity$Category[x]
  }
}
hourly <- filter(hourly, Category != 0)

#Inner join daily_activity with sleep on the new ID (Id+date) column
activity_sleep <- inner_join(daily_activity, sleep_day, by = "ID")
activity_sleep <- subset(activity_sleep, select = -Id.y)

#Look at time of usage across groups
activity_sleep$Usage <- rowSums(activity_sleep[, c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes", "TotalMinutesAsleep")])
activity_sleep$UsageH <- activity_sleep$Usage/60

#filter and summarize the data
#means for metrics by the hour
options(dplyr.summarise.inform = FALSE)
hour_summary <- hourly %>% 
  group_by(ActivityHour,Category)%>% 
  summarise(meanSteps = mean(StepTotal), meanCal = mean(Calories), meanIntensity = mean(Intensity))

daily_summary <- daily_activity %>%
  group_by(Category) %>%
  summarise(VeryActive = mean(VeryActiveMinutes), FairlyActive = mean(FairlyActiveMinutes),
            LightlyActive = mean(LightlyActiveMinutes), VAsd = sd(VeryActiveMinutes, na.rm = TRUE), 
            FAsd = sd(FairlyActiveMinutes, na.rm = TRUE), LAsd = sd(LightlyActiveMinutes, na.rm = TRUE))

#Run the statistics
##Intensity vs Calories
RegICH = summary(lm(data = filter(hourly, Category == "High"), Calories ~ Intensity))
RegICI = summary(lm(data = filter(hourly, Category == "Intermediate"), Calories ~ Intensity))
RegICL = summary(lm(data = filter(hourly, Category == "Low"), Calories ~ Intensity))
##Steps vs Calories
RegSCH = summary(lm(data = filter(hourly, Category == "High"), Calories ~ StepTotal))
RegSCI = summary(lm(data = filter(hourly, Category == "Intermediate"), Calories ~ StepTotal))
RegSCL = summary(lm(data = filter(hourly, Category == "Low"), Calories ~ StepTotal))

#Define Plots
pal1 <- c("#0570B0", "#74A9CF", "#999999" )
#Histogram of usage
hu <- ggplot(data = activity_sleep, aes(x = UsageH, fill = Category)) + 
  geom_histogram(binwidth = 1, position = "Dodge", color = "white") + 
  scale_fill_manual(values = pal1)+
  labs(title = "Distribution of smart device usage", x = "Usage [hours/day]", y = "Count",fill = "Calorie burner category")

#Histogram of daily calories from daily_activity
h1 <- ggplot(daily_activity, aes(x=Calories)) +
  geom_histogram(binwidth = 50, position="Identity",color = "white", fill = "black") +
  labs(title = "Distibution of Daily Calories Burned", x = "Calories", y = "Count")

h2 <- ggplot(daily_activity, aes(x=Calories, fill=Category)) +
  geom_histogram(binwidth = 50, position="Identity",color = "white") +
  scale_fill_manual(values=pal1)+
  geom_vline(aes(xintercept=a),color=pal1[1], linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=b),color=pal1[3], linetype="dashed", size=1) +
  annotate(geom="text", x=4000, y=30, label="Top 10%", color=pal1[1], size = 6) +
  annotate(geom="text", x=750, y=30, label="Bottom 10%", color=pal1[3], size = 6) + 
  labs(title = "Distibution of Daily Calories Burned", x = "Calories", 
       y = "Count", fill = "Calorie burner category")

#Calories vs. steps from daily_activity
scatterDSteps <- ggplot(data = daily_activity)+geom_point(mapping = aes(x = TotalSteps, y = Calories)) + 
  geom_hline(yintercept = a, linetype = "dashed", color = "#0570B0", size = 1.25) + 
  geom_hline(yintercept = b, linetype = "dashed", color = "#999999", size = 1.25) + 
  annotate(geom="text", x=30000, y=a+250, label="Top 10%", color="#0570B0", size = 6) +
  annotate(geom="text", x=30000, y=b-250, label="Bottom 10%", color="#999999", size = 6) + 
  labs(title = "Relationship between calories burned and steps taken", x = "steps taken", 
       y = "calories burned")

#calories vs. sleep from activity_sleep
scatterDSleep <- ggplot(data = activity_sleep, aes(x = TotalMinutesAsleep/60, y = Calories, color = Category)) + 
  geom_point() +
  scale_colour_manual(values = pal1)+ 
  labs(title = "Relationship between sleep and calories burned", x = "hours of sleep", y = "calories burned", color = "Calorie burner category") 

#Intensity proportions segregated by calorie category - from daily data
Activity <- c("Very","Fairly", "Lightly")
A1 <- as.numeric(daily_summary[1,2:4])
A2 <- as.numeric(daily_summary[2,2:4])
A3 <- as.numeric(daily_summary[3,2:4])
df1 <- data.frame(Activity,A1)
df2 <- data.frame(Activity,A2)
df3 <- data.frame(Activity,A3)
hsize <- 1.0

#Add hsize
df1 <- data.frame(df1, x = hsize)
df2 <- data.frame(df2, x = hsize)
df3 <- data.frame(df3, x = hsize)

#set order
df1$Activity <- factor(df1$Activity, levels = df1$Activity)
df2$Activity <- factor(df2$Activity, levels = df2$Activity)
df3$Activity <- factor(df3$Activity, levels = df3$Activity)

# Make the plot
donut1 <- ggplot(df1, aes(x = hsize, y = (A1/(sum(A1))*100), fill = Activity)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = round((A1/sum(A1))*100)), position = position_stack(vjust = 0.5)) + 
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c("#0570B0", "#74A9CF", "#D0D1E6")) +
  xlim(c(0.2, hsize + .5))+ 
  labs(title = "High calorie burners", y = "% of active minutes in each level of activity", x = "")+
  guides(fill = guide_legend(title = "Active level")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

donut2 <- ggplot(df2, aes(x = hsize, y = (A2/(sum(A2))*100), fill = Activity)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = round((A2/sum(A2))*100)), position = position_stack(vjust = 0.5)) + 
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c("#0570B0", "#74A9CF", "#D0D1E6")) +
  xlim(c(0.2, hsize + .5))+ 
  labs(title = "Intermediate calorie burners", y = "% of active minutes in each level of activity", x = "")+
  guides(fill = guide_legend(title = "Active level")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

donut3 <- ggplot(df3, aes(x = hsize, y = (A3/(sum(A3))*100), fill = Activity)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = round((A3/sum(A3))*100)), position = position_stack(vjust = 0.5)) + 
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c("#0570B0", "#74A9CF", "#D0D1E6")) +
  xlim(c(0.2, hsize + .5))+ 
  labs(title = "Low calorie burners", y = "% of active minutes in each level of activity", x = "")+
  guides(fill = guide_legend(title = "Active level")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
 
#Hourly data charts
dat_text <- data.frame(
  label = c("R^2 = .76, p < .001", "R^2 = 0.70, p < .001", "R^2 = 0.62, p < .001"),
  cat = c("High", "Intermediate", "Low"))

scatterHI <- ggplot(data = hourly, aes(x = Intensity, y = Calories, color = Category)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  scale_color_manual(values = pal1) +
  #facet_wrap(~Category)+
  labs(title = "Calorie consumption as a function of movement intensity", x = "exercise intensity", 
       y = "Calories burned", color = "Calorie burner category")

#How does step count correlate with calories
scatterHS <- ggplot(data = hourly, aes(x = StepTotal, y = Calories, color = Category)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  scale_color_manual(values = pal1) +
  #facet_wrap(~Category)+
  labs(title = "Calorie consumption as a function of steps taken", x = "Steps", 
       y = "Calories burned", color = "Calorie burner category")

#How does the hour of the day correlate with calories burned
barHC <- ggplot(data = hour_summary, aes(x = ActivityHour, y = meanCal, fill = Category)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual(values = pal1) + 
  labs(title = "Calorie expenditure per hour of the day", x = "Hour [24 hour scale]", 
        y = "Calories/hour [mean]", fill = "Calorie burner category")+
   theme(axis.text.x = element_text(angle = 30))

#Plotting
hu
h1
h2
#scatterDSteps
scatterDSleep
donut1
donut2
donut3
scatterHI
scatterHS
barHC


mets <- read.csv("minuteMETsNarrow_merged.csv")
mets$time <- as.POSIXct(mets$ActivityMinute, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
mets$ActivityDay <- substr(mets$time, 1, 10)
mets$ActivityTime <- substr(mets$time,11,22)
mets$ID1 <- paste(mets$Id, mets$ActivityDay)
mets$ID2 <- paste(mets$ID1, substr(mets$ActivityTime,1,3))
new_mets <- data.frame(unique(unlist(mets$ID2)), colMeans(matrix(mets$METs, nrow = 60)))
new_mets <- setNames(new_mets, c("ID","METs"))
rm(mets)



                  
