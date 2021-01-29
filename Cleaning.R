# ------------------------------------------
# Analysis cleaning experiment - One full year
# Evelien van den Berg & Ewout Knoester
# Created 11-Jan-2021
# ------------------------------------------

# Set R and packages
rm(list=ls()) #Clear workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set directory 
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(rstatix)
library(lme4)
library(car)
library(multcompView)
library(lsmeans)
library(multcomp)

#Load and organize data

  #Volledige Productie dataframe
  df.production <- read.csv("Production.csv", check.names = FALSE, header = TRUE)
  df.production <- setDT(df.production)
  df.production$species   <- factor(df.production$species, labels = c("Acropora cf formosa", "Acropora verweyi", "Pocillopora verrucosa", "Porites cylindrica"))
  df.production$size      <- factor(df.production$size, labels = c("Small", "Normal", "Large"))
  df.production$date      <- factor(df.production$date, labels = c("Dec18", "Feb19", "Apr19", "Jun19", "Aug19", "Oct19"))
  df.production$cleaning  <- factor(df.production$cleaning, levels = c(1,2,3,4), labels = c("Weekly", "Monthly", "3-Monthly", "Never"))
  df.production           <- mutate(df.production, id = paste0(structure, "_", position))
  df.production$id        <- factor(df.production$id)
  df.production$structure <- factor(df.production$structure)
  df.production$position  <- factor(df.production$position)

  #Growth dataframe: selectie van rijen waar survival >80%
  df.growth               <- subset(df.production, survival > 80)

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

#Summary function, getting mean, standard deviation, number of observations and standard error split per category
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


#survival.summary %>% group_by(cleaning, date, size) %>% summarise(statistic = shapiro.test(survival.summary$survival)$statistic, p.value = shapiro.test(survival.summary$survival)$p.value)                     
           
#kruskal.test(survival.summary$survival ~ size + species + date + cleaning, data = survival.summary)

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

#anov.growth <- anova_test(data = Growth, dv = Growth$SGR, wid = id, within = c(Growth$species, Growth$cleaning, Growth$size, Growth$date))

#Shapiro-Wilk test 
aggregate(cbind(P.value=production.summary$SGR) ~ size + species + cleaning, data = production.summary, function(x) shapiro.test(production.summary$SGR)$p.value)
aggregate(cbind(P.value=growth.summary$SGR) ~ size + species + cleaning, data = growth.summary, function(x) shapiro.test(growth.summary$SGR)$p.value)
aggregate(cbind(P.value=survival.summary$survival) ~ size + species + cleaning, data = survival.summary, function(x) shapiro.test(survival.summary$survival)$p.value)

#SGRT TRANSFORMATION + Bar + Histo + QQ
df.production <- mutate(df.production, SQRT_SGR = sqrt(abs(SGR)))

production.summary.SQRT <- data_summary(df.production, varname="SQRT_SGR", 
                                   groupnames=c("species", "cleaning", "size", "date"))

ggplot(production.summary.SQRT, aes(fill=cleaning, y=SQRT_SGR, x=date))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species + size)+
  geom_errorbar(aes(ymin=SQRT_SGR-se, ymax=SQRT_SGR+se), width=.2, position=position_dodge(.9))

ggplot(df.production, aes(x = SQRT_SGR))+
  geom_histogram(aes(fill = SQRT_SGR), position = "identity", bins = 30, alpha = 0.4)+
  facet_wrap(~species + cleaning + size)

ggplot(df.production, aes(sample = SQRT_SGR))+
  stat_qq()+
  facet_wrap(~species + cleaning + size)

#sqrt_production <- sqrt(production.summary$SGR)
#production.summary$SGR <- sqrt(production.summary$SGR)
#hist(sqrt_production, col='coral2', main='Square Root Transformed',breaks=100)
#hist(production.summary$SGR, col='coral2', main='Production',breaks=100)

#QQ plot transformed data
#qqnorm(df.SQRTproduction$sqrt.production.summary.SGR.)
#qqline(df.SQRTproduction$sqrt.production.summary.SGR., col = "steelblue", lwd = 2)

#Non-parametric test for survival
##kruskal.test(survival.summary$survival ~ size + species + date + cleaning, data = survival.summary)

#SURVIVAL
#Surival by size
kruskal.test(survival.summary$survival ~ size, data = survival.summary)
survival.summary.size <- data_summary(df.production, varname="survival", 
                                 groupnames=c("size"))
ggplot(survival.summary.size, aes(fill=size, y=survival, x=size))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=survival-(2*se), ymax=survival+(2*se), width=.2))

#Survival by species
kruskal.test(survival.summary$survival ~ species, data = survival.summary)
survival.summary.species <- data_summary(df.production, varname="survival", 
                                      groupnames=c("species"))
ggplot(survival.summary.species, aes(fill=species, y=survival, x=species))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=survival-(2*se), ymax=survival+(2*se), width=.2))

#Survival by cleaning
kruskal.test(survival.summary$survival ~ cleaning, data = survival.summary)
survival.summary.cleaning <- data_summary(df.production, varname="survival", 
                                      groupnames=c("cleaning"))
ggplot(survival.summary.cleaning, aes(fill=cleaning, y=survival, x=cleaning))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=survival-(2*se), ymax=survival+(2*se), width=.2))

#Survival by date
kruskal.test(survival.summary$survival ~ date, data = survival.summary)
survival.summary.date <- data_summary(df.production, varname="survival", 
                                          groupnames=c("date"))
ggplot(survival.summary.date, aes(fill=date, y=survival, x=date))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=survival-(2*se), ymax=survival+(2*se), width=.2))

#PRODUCTION (SQRT TRANSFORMED)
production.summary.tree <- data_summary(df.production, varname="SQRT_SGR", 
                                        groupnames=c("species", "cleaning", "size", "structure"))

aov.production <- lm(SQRT_SGR ~ cleaning * size * species, data = production.summary.tree)
hist(residuals(aov.production))
plot(fitted(aov.production), residuals(aov.production))
Anova(aov.production, test="F", type="II")

#PROUDCTION: simple main cleaning
production.summary.cleaning <- data_summary(df.production, varname="SGR", 
                                      groupnames=c("cleaning"))
ggplot(production.summary.cleaning , aes(fill=cleaning, y=SGR, x=cleaning))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=SGR-(2*se), ymax=SGR+(2*se), width=.2))
cld(lsmeans(aov.production, "cleaning", adjust="tukey"), alpha=.05,  Letters=letters)

#PROUDCTION: simple main size
production.summary.size <- data_summary(df.production, varname="SGR", 
                                            groupnames=c("size"))
ggplot(production.summary.size, aes(fill=size, y=SGR, x=size))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=SGR-(2*se), ymax=SGR+(2*se), width=.2))
cld(lsmeans(aov.production, "size", adjust="tukey"), alpha=.05,  Letters=letters)

#PROUDCTION: simple main size
production.summary.species <- data_summary(df.production, varname="SGR", 
                                        groupnames=c("species"))
ggplot(production.summary.species, aes(fill=species, y=SGR, x=species))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=SGR-(2*se), ymax=SGR+(2*se), width=.2))
cld(lsmeans(aov.production, "species", adjust="tukey"), alpha=.05,  Letters=letters)
