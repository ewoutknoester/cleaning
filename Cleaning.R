# ------------------------------------------
# Analysis cleaning experiment - One full year
# Evelien van den Berg & Ewout Knoester
# Created 11-Jan-2021
# ------------------------------------------

# Set R and packages
rm(list=ls()) #Clear workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set directory 
#Load and organize data
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
library(readr)
library(data.table)

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

#write_xlsx(df.production,"C:/Users/eveli/Documents/Wageningen University/MSc Thesis/Nieuwe thesis - cleaning interval/Data\\Mean Production.xlsx")
  
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

growth.summary <- data_summary(df.growth, varname='SGR', 
                                   groupnames=c("species", "cleaning", "size", "date"))
growth.summary$date <- factor(growth.summary$date)

survival.summary <- data_summary(df.production, varname="survival", 
                               groupnames=c("species", "cleaning", "size", "date"))
survival.summary$date <- factor(production.summary$date)

#Bar graphs
ggplot(production.summary, aes(fill=cleaning, y=SGR, x=size))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.2, position=position_dodge(.9))

ggplot(growth.summary, aes(fill=cleaning, y=SGR, x=date))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species + size)+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.2, position=position_dodge(.9))

ggplot(survival.summary, aes(fill=cleaning, y=survival, x=date))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species + size)+
  geom_errorbar(aes(ymin=survival-se, ymax=survival+se), width=.2, position=position_dodge(.9))
                                                                                                                                          
#histogram
#hist(Growth$SGR, main="Histogram for SGR - Growth", xlab="SGR",las=1, breaks=50)
#hist(Production$SGR, main="Histogram for SGR - Production", xlab="SGR",las=1, breaks=100)
#hist(Survival_kopie$survival, main="Histogram for survival", xlab="SGR",las=1, breaks=10)
#hist(sqrt.production, main="Histogram for SGR - Production (transformed)", xlab="SGR",las=1, breaks=100)
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
production.summary$SGR <- sqrt(production.summary$SGR)

production.summary.SQRT <- data_summary(df.production, varname="SGR", 
                                   groupnames=c("species", "cleaning", "size", "date"))

ggplot(production.summary.SQRT, aes(fill=cleaning, y=SGR, x=size))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.2, position=position_dodge(.9))

ggplot(df.production, aes(x = SGR))+
  geom_histogram(aes(fill = SGR), position = "identity", bins = 30, alpha = 0.4)+
  facet_wrap(~species + cleaning + size)

ggplot(df.production, aes(sample = SGR))+
  stat_qq()+
  facet_wrap(~species + cleaning + size)

#sqrt_production <- sqrt(production.summary$SGR)
production.summary$SGR <- sqrt(production.summary$SGR)
#hist(sqrt_production, col='coral2', main='Square Root Transformed',breaks=100)
#hist(production.summary$SGR, col='coral2', main='Production',breaks=100)

#QQ plot transformed data
#qqnorm(df.SQRTproduction$sqrt.production.summary.SGR.)
#qqline(df.SQRTproduction$sqrt.production.summary.SGR., col = "steelblue", lwd = 2)

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
production.summary.tree <- data_summary(df.production, varname="SGR", 
                                        groupnames=c("species", "cleaning", "size", "structure"))

aov.production <- lm(SGR ~ cleaning * size * species, data = production.summary.tree)
hist(residuals(aov.production))
plot(fitted(aov.production), residuals(aov.production))
Anova(aov.production, test="F", type="II")

#PROUDCTION: simple main cleaning
production.summary.cleaning <- data_summary(df.production, varname="SGR", 
                                      groupnames=c("cleaning"))
ggplot(production.summary.cleaning , aes(fill=cleaning, y=SGR, x=cleaning))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=SGR-(2*se), ymax=SGR+(2*se), width=.2)) + 
  theme(axis.text = element_text(size = 18),legend.title=element_text(size=20), legend.text=element_text(size=20), axis.title.x = element_text(size=18),axis.title.y = element_text(size=18), strip.text =  element_text(size=14))
cld(lsmeans(aov.production, "cleaning", adjust="tukey"), alpha=.05,  Letters=letters)

#PROUDCTION: simple main size
production.summary.size <- data_summary(df.production, varname="SGR", 
                                            groupnames=c("size"))
ggplot(production.summary.size, aes(fill=size, y=SGR, x=size))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=SGR-(2*se), ymax=SGR+(2*se), width=.2)) + 
  theme(axis.text = element_text(size = 18),legend.title=element_text(size=20), legend.text=element_text(size=20), axis.title.x = element_text(size=18),axis.title.y = element_text(size=18), strip.text =  element_text(size=14))
cld(lsmeans(aov.production, "size", adjust="tukey"), alpha=.05,  Letters=letters)

#PROUDCTION: simple main species
production.summary.species <- data_summary(df.production, varname="SGR", 
                                        groupnames=c("species"))
ggplot(production.summary.species, aes(fill=species, y=SGR, x=species))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=SGR-(2*se), ymax=SGR+(2*se), width=.2)) + 
  theme(axis.text = element_text(size = 18),legend.title=element_text(size=20), legend.text=element_text(size=20), axis.title.x = element_text(size=18),axis.title.y = element_text(size=18), strip.text =  element_text(size=14))
tukey_p.species <- cld(lsmeans(aov.production, "species", adjust="tukey"), alpha=.05,  Letters=letters)
geom_text(data=tukey_p.species,aes(label=tukey_p.species$.group),vjust=0)

#Barplots zonder tijdsaspect
production.summary.new <- data_summary(df.production, varname="SGR", groupnames=c("species", "cleaning", "size"))
ggplot(production.summary.new, aes(fill=cleaning, y=SGR, x=size))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.2, position=position_dodge(.9)) + 
  theme(axis.text = element_text(size = 18),legend.title=element_text(size=20), legend.text=element_text(size=20), axis.title.x = element_text(size=18),axis.title.y = element_text(size=18), strip.text =  element_text(size=14))

growth.summary.new <- data_summary(df.growth, varname="SGR", groupnames=c("species", "cleaning", "size"))
ggplot(growth.summary.new, aes(fill=cleaning, y=SGR, x=size))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.2, position=position_dodge(.9))
cld(SGR, level = 0.05, decreasing = FALSE)

survival.summary.new <- data_summary(df.production, varname="survival", groupnames=c("species", "cleaning", "size"))
ggplot(survival.summary.new, aes(fill=cleaning, y=survival, x=size))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+
  geom_errorbar(aes(ymin=survival-se, ymax=survival+se), width=.2, position=position_dodge(.9))

#Conclusion ratio 
library(readr)
df.conclusion <- read_delim("Conclusion new.csv", 
                         ";", escape_double = FALSE, col_types = cols(ratio = col_number()), 
                         trim_ws = TRUE)
df.conclusion <- setDT(df.conclusion)
species   <- factor(df.conclusion$species, labels = c("Acropora formosa", "Acropora verweyi", "Pocillopora verrucosa", "Porites cylindrica"))
size      <- factor(df.conclusion$size, levels = c("small", "normal", "large"), labels = c("small", "normal", "large"))
cleaning  <- factor(df.conclusion$cleaning,levels = c("Weekly", "Monthly", "3-monthly", "Never"),labels = c("Weekly", "Monthly", "3-monthly", "Never"))
ratio <- factor(df.conclusion$ratio)
library(DescTools)
df.conclusion$cleaning <- reorder(cleaning, new.order=c("Weekly", "Monthly", "3-monthly", "Never"))
df.conclusion$size <- reorder(size, new.order=c("small", "normal", "large"))
ggplot(df.conclusion, aes(fill=cleaning, x=size, y=ratio))+
  geom_bar(position="dodge", stat="identity")+facet_wrap(~species)+
  theme(axis.text = element_text(size = 18),legend.title=element_text(size=18), legend.text=element_text(size=18), axis.title.x = element_text(size=18),axis.title.y = element_text(size=18), strip.text =  element_text(size=14))+
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))

#post hoc 2-way interactions
#size:species
ggplot(production.summary , aes(fill=size, y=SGR, x=size))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+ 
  geom_errorbar(aes(ymin=SGR-(2*se), ymax=SGR+(2*se), width=.2)) + 
  theme(axis.text = element_text(size = 14),legend.title=element_text(size=16), legend.text=element_text(size=16), axis.title.x = element_text(size=14),axis.title.y = element_text(size=14), strip.text =  element_text(size=12))
size.species <- cld(lsmeans(aov.production, 'size' , 'species', adjust="tukey"), alpha=.05,  Letters=letters)
ggplot(size.species , aes(fill=size, y=lsmean, x=size))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+ 
  geom_errorbar(aes(ymin=lsmean-(2*SE), ymax=lsmean+(2*SE), width=.2)) + 
  theme(axis.text = element_text(size = 14),legend.title=element_text(size=16), legend.text=element_text(size=16), axis.title.x = element_text(size=14),axis.title.y = element_text(size=14), strip.text =  element_text(size=12))

#cleaning:species
ggplot(production.summary , aes(fill=cleaning, y=SGR, x=cleaning))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+ 
  geom_errorbar(aes(ymin=SGR-(2*se), ymax=SGR+(2*se), width=.2)) + 
  theme(axis.text = element_text(size = 14),legend.title=element_text(size=16), legend.text=element_text(size=16), axis.title.x = element_text(size=14),axis.title.y = element_text(size=14), strip.text =  element_text(size=12))
cleaning.species<-cld(lsmeans(aov.production, 'cleaning' , 'species', adjust="tukey"), alpha=.05,  Letters=letters)
ggplot(cleaning.species , aes(fill=cleaning, y=lsmean, x=cleaning))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+ 
  geom_errorbar(aes(ymin=lsmean-(2*SE), ymax=lsmean+(2*SE), width=.2)) + 
  theme(axis.text = element_text(size = 14),legend.title=element_text(size=16), legend.text=element_text(size=16), axis.title.x = element_text(size=14),axis.title.y = element_text(size=14), strip.text =  element_text(size=12))

#cleaning:size
ggplot(production.summary , aes(fill=cleaning, y=SGR, x=cleaning))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~species)+ 
  geom_errorbar(aes(ymin=SGR-(2*se), ymax=SGR+(2*se), width=.2)) + 
  theme(axis.text = element_text(size = 14),legend.title=element_text(size=16), legend.text=element_text(size=16), axis.title.x = element_text(size=14),axis.title.y = element_text(size=14), strip.text =  element_text(size=12))
cleaning.size<-cld(lsmeans(aov.production, 'cleaning' , 'size', adjust="tukey"), alpha=.05,  Letters=letters)
ggplot(cleaning.size , aes(fill=cleaning, y=lsmean, x=cleaning))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~size)+ 
  geom_errorbar(aes(ymin=lsmean-(2*SE), ymax=lsmean+(2*SE), width=.2)) + 
  theme(axis.text = element_text(size = 14),legend.title=element_text(size=16), legend.text=element_text(size=16), axis.title.x = element_text(size=14),axis.title.y = element_text(size=14), strip.text =  element_text(size=12))
geom_text(data=cleaning.size,aes(label=.group),vjust=1)
