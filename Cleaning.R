# ------------------------------------------
# Analysis cleaning experiment - One full year
# Evelien van den Berg & Ewout Knoester
# Created 11-Jan-2021
# ------------------------------------------

# Set R and packages
rm(list=ls()) # Clear workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(data.table)
library(ggplot2)
library(plyr)

#Load and organize data

#Volledige Productie dataframe
df.production <- read.csv("Production.csv", check.names = FALSE, header = TRUE)
df.production <- setDT(df.production)
df.production$species <- factor(df.production$species)
df.production$size <- factor(df.production$size, labels = c("Small", "Normal", "Large"))
df.production$cleaning <- factor(df.production$cleaning, levels = c(1,2,3,4), labels = c("Weekly", "Monthly", "3-Monthly", "Never"))

#Growth dataframe: selectie van rijen waar survival >80%
df.growth <- subset(df.production, survival > 80)

#mean(df.production$SGR)
#mean(df.growth$SGR)

#Gemiddelde SGR van growth (>80% survival)
#aggregate(cbind(Growth$SGR) ~ Growth$size + Growth$species + Growth$date + Growth$cleaning, data = Growth, FUN='mean', na.rm = TRUE)
#df.growth<- data.frame(aggregate(cbind(Growth$SGR) ~ Growth$size + Growth$species + Growth$date + Growth$cleaning, data = Growth, FUN='mean', na.rm = TRUE)
                       
#Gemiddelde SGR van production (alle survival waardes waardoor je groei en survival in een hebt)
#aggregate(cbind(Production$SGR) ~ Production$size + Production$species + Production$date + Production$cleaning, data = Production, FUN='mean', na.rm = TRUE)
#df.production<- data.frame(aggregate(cbind(Production$SGR) ~ Production$size + Production$species + Production$date + Production$cleaning, data = Production, FUN='mean', na.rm = TRUE))
#print(df.production)
#write_xlsx(df.production,"C:/Users/eveli/Documents/Wageningen University/MSc Thesis/Nieuwe thesis - cleaning interval/Data\\Mean Production.xlsx")
#Growth %>% count(Growth$size, Growth$species, Growth$date, Growth$cleaning)

#Grafieken: 
#plot(df.growth$V1 ~ df.growth$Growth.date, data=subset(df.growth, df.growth$Growth.species=='Acropora formosa'& df.growth$Growth.cleaning=='1' & df.growth$Growth.size=='1'))
#title(main = "Acropora formosa - weekly cleaning")
#barplot(df.growth$V1, main="Acropora formosa- weekly cleaned",xlab="Date (month)", ylab="SGR (/d)")
#barplot(subset(df.growth$v1, df.growth$Growth.species=='Acropora formosa'& df.growth$Growth.cleaning=='1' & df.growth$Growth.size=='1'))
#ggplot(df.growth, aes(fill=Growth.species, y=V1, x=Growth.size)) + geom_bar(position="dodge", stat="identity")

#Summary function, getting mean, standard deviation and number of observations split per category
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      n  = length(x[[col]]),
      se = sd(x[[col]]/sqrt(length(x[[col]])), na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#Applying summary function to get summary values for Production, Growth and Survival
production.summary <- data_summary(df.production, varname="SGR", 
                    groupnames=c("species", "cleaning", "size", "date"))
production.summary$date <- factor(production.summary$date)

growth.summary <- data_summary(df.growth, varname="SGR", 
                                   groupnames=c("species", "cleaning", "size", "date"))
growth.summary$date <- factor(growth.summary$date)

survival.summary <- data_summary(df.production, varname="survival", 
                               groupnames=c("species", "cleaning", "size", "date"))
survival.summary$date <- factor(production.summary$date)

#Bar graphs
ggplot(production.summary, aes(fill=cleaning, y=SGR, x=date))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species + size)+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.2, position=position_dodge(.9))

ggplot(growth.summary, aes(fill=cleaning, y=SGR, x=date))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species + size)+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.2, position=position_dodge(.9))

ggplot(survival.summary, aes(fill=cleaning, y=survival, x=date))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species + size)+
  geom_errorbar(aes(ymin=survival-se, ymax=survival+se), width=.2, position=position_dodge(.9))


survival.summary %>% group_by(cleaning, date, size) %>% summarise(statistic = shapiro.test(survival.summary$survival)$statistic, p.value = shapiro.test(survival.summary$survival)$p.value)                     
           
kruskal.test(survival.summary$survival ~ size + species + date + cleaning, data = survival.summary)

# ggplots met errorbars
#ggplot(df.production, aes(fill=Production.size, y=V1, x=Production.date)) + geom_bar(position="dodge", stat="identity") + geom_errorbar(aes(ymin= Production$SGR - sd.production, ymax= Production$SGR + sd.production) + facet_wrap(~Production.species + Production.cleaning)
#ggplot(df.survival_kopie, aes(fill=Survival_kopie.size, y=V1, x=Survival_kopie.date)) + geom_bar(position="dodge", stat="identity") +  geom_errorbar(aes(ymin= Survival_kopie$survival - sd.survival, ymax= Survival_kopie$survival + sd.survival)+ facet_wrap(~Survival_kopie.species + Survival_kopie.cleaning)
                                                                                                                                          
#histogram
#hist(Growth$SGR, main="Histogram for SGR - Growth", xlab="SGR",las=1, breaks=50)
#hist(Production$SGR, main="Histogram for SGR - Production", xlab="SGR",las=1, breaks=100)
#hist(Survival_kopie$survival, main="Histogram for survival", xlab="SGR",las=1, breaks=10)
#hist(sqrt.production, main="Histogram for SGR - Production (transformed)", xlab="SGR",las=1, breaks=100)

#Growth %>% ggplot(aes(sample = Growth$SGR)) + geom_qq() + geom_qq_line() + facet_wrap(~Growth$species + Growth$cleaning + Growth$size, scales = "free_y")
#Production %>% ggplot(aes(sample = Production$SGR)) + geom_qq() + geom_qq_line() + facet_wrap(~Production$species + Production$cleaning + Production$size, scales = "free_y")


#ggplot(Growth, aes(x = SGR)) + geom_histogram(aes(color = SGR, fill = SGR), position = "identity", bins = 30, alpha = 0.4) + scale_color_manual(values = c("#00AFBB", "#E7B800")) + scale_fill_manual(values = c("#00AFBB", "#E7B800")) + facet_wrap(~Growth$species + Growth$cleaning + Growth$size)
#ggplot(Production, aes(x = SGR)) + geom_histogram(aes(color = SGR, fill = SGR), position = "identity", bins = 30, alpha = 0.4) + scale_color_manual(values = c("#00AFBB", "#E7B800")) + scale_fill_manual(values = c("#00AFBB", "#E7B800")) + facet_wrap(~Production$species + Production$cleaning + Production$size)
#ggplot(Survival_kopie, aes(x = survival)) + geom_histogram(aes(color = survival, fill = survival), position = "identity", bins = 30, alpha = 0.4) + scale_color_manual(values = c("#00AFBB", "#E7B800")) + scale_fill_manual(values = c("#00AFBB", "#E7B800")) + facet_wrap(~ Survival_kopie$cleaning + Survival_kopie$size + Survival_kopie$date)

#Histograms
ggplot(df.production, aes(x = SGR))+
  geom_histogram(aes(fill = SGR), position = "identity", bins = 30, alpha = 0.4)+
  facet_wrap(~species + cleaning + size)

ggplot(df.growth, aes(x = SGR))+
  geom_histogram(aes(fill = SGR), position = "identity", bins = 30, alpha = 0.4)+
  facet_wrap(~species + cleaning + size)

ggplot(df.production, aes(x = survival))+
  geom_histogram(aes(fill = survival), position = "identity", bins = 30, alpha = 0.4)+
  facet_wrap(~species + cleaning + size)

#QQ plots
ggplot(df.production, aes(sample = SGR))+
  stat_qq()+
  facet_wrap(~species + cleaning + size)

ggplot(df.growth, aes(sample = SGR))+
  stat_qq()+
  facet_wrap(~species + cleaning + size)

#QQ plots
qqnorm(Growth$SGR)
qqline(Growth$SGR, col = "steelblue", lwd = 2)
qqnorm(Production$SGR)
qqline(Production$SGR, col = "steelblue", lwd = 2)
qqnorm(Survival_kopie$survival)
qqline(Survival_kopie$survival, col = "steelblue", lwd = 2)


anov.growth <- anova_test(data = Growth, dv = Growth$SGR, wid = id, within = c(Growth$species, Growth$cleaning, Growth$size, Growth$date))

#Shapiro-Wilk test 
aggregate(cbind(P.value=production.summary$SGR) ~ size + species + cleaning, data = production.summary, function(x) shapiro.test(production.summary$SGR)$p.value)
aggregate(cbind(P.value=growth.summary$SGR) ~ size + species + cleaning, data = growth.summary, function(x) shapiro.test(growth.summary$SGR)$p.value)
aggregate(cbind(P.value=survival.summary$survival) ~ size + species + cleaning, data = survival.summary, function(x) shapiro.test(survival.summary$survival)$p.value)

#SGRT transformation
sqrt_production <- sqrt(production.summary$SGR)
production.summary$SGR <- sqrt(production.summary$SGR)
hist(sqrt_production, col='coral2', main='Square Root Transformed',breaks=100)
hist(production.summary$SGR, col='coral2', main='Production',breaks=100)
#QQ plot transformed data
qqnorm(df.SQRTproduction$sqrt.production.summary.SGR.)
qqline(df.SQRTproduction$sqrt.production.summary.SGR., col = "steelblue", lwd = 2)

#Non-parametric test for survival
kruskal.test(survival.summary$survival ~ size + species + date + cleaning, data = survival.summary)
kruskal.test(survival.summary$survival ~ size, data = survival.summary)
kruskal.test(survival.summary$survival ~ species, data = survival.summary)
kruskal.test(survival.summary$survival ~ cleaning, data = survival.summary)
kruskal.test(survival.summary$survival ~ date, data = survival.summary)

#Three way anova with repeated measures
is.numeric(growth.summary$SGR)
growth.aov <- anova_test(data = growth.summary, dv = SGR, wid = SGR, within = c(species, size, cleaning))
growth.aov <- anova_test(data = growth.summary, dv = growth.summary$SGR, wid = SGR,within = c(species, size, cleaning))
production.aov <- anova_test(data = production.summary, dv = SGR, wid = SGR, within = c(species, size, cleaning))

