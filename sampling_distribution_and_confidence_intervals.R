#Confidence Intervals in R using dplyr

library(dplyr)
library(devtools)
library(statsr)
library(ggplot2)

#We are using the ames housing dataset.
data(ames)

set.seed(1234)

#Create 50 samples each of size 60 from the original population.
#Calculate the sample means for each sample.
samp_means <- ames %>%
  rep_sample_n(size=60,reps=50,replace=T) %>% 
  summarise(mean=mean(area))

#Plotting a histogram of the sample means show that the distribution of the sample means is nearly normal.
hist(samp_means$mean)

#Get 95% Confidence Intervals
z_score = 1.96
n=60

samp_ci95 <- ames %>% 
  rep_sample_n(60,50,replace=T) %>%
  summarise(lower= mean(area) - z_score * (sd(area)/sqrt(n)),
            upper= mean(area) + z_score * (sd(area)/sqrt(n)))

#We add a column to this data frame which tells us wether the true mean falls in the interval or not.
samp_ci95$capture <- ifelse((samp_ci95$lower < mean(ames$area)) & (samp_ci95$upper > mean(ames$area)), "yes", "no")

#Percentage of samples that captured the mean
prop.table(table(samp_ci95$capture))

#To visualise the same we need to re-organize our data as follows
#we create a new dataframe
ciplotdata <- data.frame(id=c(samp_ci95$replicate,samp_ci95$replicate),
                         bound=c(samp_ci95$lower,samp_ci95$upper),
                         capture=c(samp_ci95$capture,samp_ci95$capture))
#Plot
ggplot(data=ciplotdata,aes(x=bound,y=id,group=id,color=capture))+
  geom_point()+geom_line()

# Statistical Inference
#Data about births
data("nc")
sr(nc)

#side by side boxplot of weights of babies of smokers and non-smokers
boxplot(weight~habit,data=nc)

#We conduct a hypothesis test to check if there is a significant difference in the mean weight of babies between the smokers and non smokers
inference(x=habit,y=weight,data=nc,statistic = "mean", type="ht",null=0
          ,alternative = "twosided", method="theoretical" )

