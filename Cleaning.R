mean(Production$SGR)
mean(Growth$SGR)
library(dplyr)
#Gemiddelde SGR van growth (>80% survival)
aggregate(cbind(Growth$SGR) ~ Growth$size + Growth$species + Growth$date + Growth$cleaning, data = Growth, FUN='mean', na.rm = TRUE)
df.growth<- data.frame(aggregate(cbind(Growth$SGR) ~ Growth$size + Growth$species + Growth$date + Growth$cleaning, data = Growth, FUN='mean', na.rm = TRUE)
                       
#Gemiddelde SGR van production (alle survival waardes waardoor je groei en survival in een hebt)
aggregate(cbind(Production$SGR) ~ Production$size + Production$species + Production$date + Production$cleaning, data = Production, FUN='mean', na.rm = TRUE)
df.production<- data.frame(aggregate(cbind(Production$SGR) ~ Production$size + Production$species + Production$date + Production$cleaning, data = Production, FUN='mean', na.rm = TRUE))
print(df.production)
write_xlsx(df.production,"C:/Users/eveli/Documents/Wageningen University/MSc Thesis/Nieuwe thesis - cleaning interval/Data\\Mean Production.xlsx")
Growth %>% count(Growth$size, Growth$species, Growth$date, Growth$cleaning)

#Grafieken: 
plot(df.growth$V1 ~ df.growth$Growth.date, data=subset(df.growth,df.growth$Growth.species=='Acropora formosa'& df.growth$Growth.cleaning=='1' & df.growth$Growth.size=='1'))
title(main = "Acropora formosa - weekly cleaning")
barplot(df.growth$V1, main="Acropora formosa- weekly cleaned",xlab="Date (month)", ylab="SGR (/d)")
barplot(subset(df.growth$v1, df.growth$Growth.species=='Acropora formosa'& df.growth$Growth.cleaning=='1' & df.growth$Growth.size=='1'))
ggplot(df.growth, aes(fill=Growth.species, y=V1, x=Growth.size)) + geom_bar(position="dodge", stat="identity")

#Goede gg plots:
ggplot(df.growth, aes(fill=Growth.species, y=V1, x=Growth.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Growth.species + Growth.cleaning + Growth.size)
ggplot(df.production, aes(fill=Production.species, y=V1, x=Production.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Production.species + Production.cleaning + Production.size) 

#Gemiddelde surival over groepen:
aggregate(cbind(Survival$survival) ~ Survival$size + Survival$species + Survival$date + Survival$cleaning, data = Survival, FUN='mean', na.rm = TRUE)
df.survival<- data.frame(aggregate(cbind(Survival$survival) ~ Survival$size + Survival$species + Survival$date + Survival$cleaning, data = Survival, FUN='mean', na.rm = TRUE))
aggregate(cbind(Survival_kopie$survival) ~ Survival_kopie$size + Survival_kopie$species + Survival_kopie$date + Survival_kopie$cleaning, data = Survival_kopie, FUN='mean', na.rm = TRUE)
ggplot(df.survival, aes(fill=Survival.species, y=V1, x=Survival.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Survival.species + Survival.cleaning + Survival.size) 
sd.survival <-aggregate(cbind(Survival_kopie$survival) ~ Survival_kopie$size + Survival_kopie$species + Survival_kopie$date + Survival_kopie$cleaning, data = Survival_kopie, FUN='sd', na.rm = TRUE)

#Met cleaning en size namen:
aggregate(cbind(Survival_kopie$survival) ~ Survival_kopie$size + Survival_kopie$species + Survival_kopie$date + Survival_kopie$cleaning, data = Survival_kopie, FUN='mean', na.rm = TRUE)
df.survival_kopie<- data.frame(aggregate(cbind(Survival_kopie$survival) ~ Survival_kopie$size + Survival_kopie$species + Survival_kopie$date + Survival_kopie$cleaning, data = Survival_kopie, FUN='mean', na.rm = TRUE)
ggplot(df.survival_kopie, aes(fill=Survival_kopie.species, y=V1, x=Survival_kopie.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Survival_kopie.species + Survival_kopie.cleaning + Survival_kopie.size)

#Zelfde grafiek als eerder in excel gemaakt:
ggplot(df.survival_kopie, aes(fill=Survival_kopie.size, y=V1, x=Survival_kopie.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Survival_kopie.species + Survival_kopie.cleaning)
ggplot(df.production, aes(fill=Production.size, y=V1, x=Production.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Production.species + Production.cleaning)
ggplot(df.growth, aes(fill=Growth.size, y=V1, x=Growth.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Growth.species + Growth.cleaning)

geom_errorbar(aes(ymin=len-sd, ymax=len+sd)

#Shapiro Wilk
shapiro.test(Production$SGR)
shapiro.test(Survival_kopie$survival)
shapiro.test(Growth$SGR)

#per groep
aggregate(cbind(P.value=Growth$SGR) ~ Growth$size + Growth$species + Growth$cleaning, data = Growth, function(x) shapiro.test(Growth$SGR)$p.value)
aggregate(cbind(P.value=Production$SGR) ~ Production$size + Production$species + Production$cleaning, data = Production, function(x) shapiro.test(Production$SGR)$p.value
aggregate(cbind(P.value=Survival_kopie$survival) ~ Survival_kopie$size + Survival_kopie$date + Survival_kopie$cleaning, data = Survival_kopie, function(x) shapiro.test(Survival_kopie$survival)$p.value

Survival_kopie %>% group_by(Survival_kopie$cleaning, Survival_kopie$date, Survival_kopie$size) %>% summarise(statistic = shapiro.test(Survival_kopie$survival)$statistic, p.value = shapiro.test(Survival_kopie$survival)$p.value)                     
           
kruskal.test(Survival_kopie$survival ~ Survival_kopie$size + Survival_kopie$species + Survival_kopie$date + Survival_kopie$cleaning, data = Survival_kopie)

# ggplots met errorbars
ggplot(df.production, aes(fill=Production.size, y=V1, x=Production.date)) + geom_bar(position="dodge", stat="identity") + geom_errorbar(aes(ymin= Production$SGR - sd.production, ymax= Production$SGR + sd.production) + facet_wrap(~Production.species + Production.cleaning)
ggplot(df.survival_kopie, aes(fill=Survival_kopie.size, y=V1, x=Survival_kopie.date)) + geom_bar(position="dodge", stat="identity") +  geom_errorbar(aes(ymin= Survival_kopie$survival - sd.survival, ymax= Survival_kopie$survival + sd.survival)+ facet_wrap(~Survival_kopie.species + Survival_kopie.cleaning)
                                                                                                                                          
#histogram
hist(Growth$SGR, main="Histogram for SGR - Growth", xlab="SGR",las=1, breaks=50)
hist(Production$SGR, main="Histogram for SGR - Production", xlab="SGR",las=1, breaks=100)
hist(Survival_kopie$survival, main="Histogram for survival", xlab="SGR",las=1, breaks=10)
hist(sqrt.production, main="Histogram for SGR - Production (transformed)", xlab="SGR",las=1, breaks=100)

Growth %>% ggplot(aes(sample = Growth$SGR)) + geom_qq() + geom_qq_line() + facet_wrap(~Growth$species + Growth$cleaning + Growth$size, scales = "free_y")
Production %>% ggplot(aes(sample = Production$SGR)) + geom_qq() + geom_qq_line() + facet_wrap(~Production$species + Production$cleaning + Production$size, scales = "free_y")


ggplot(Growth, aes(x = SGR)) + geom_histogram(aes(color = SGR, fill = SGR), position = "identity", bins = 30, alpha = 0.4) + scale_color_manual(values = c("#00AFBB", "#E7B800")) + scale_fill_manual(values = c("#00AFBB", "#E7B800")) + facet_wrap(~Growth$species + Growth$cleaning + Growth$size)
ggplot(Production, aes(x = SGR)) + geom_histogram(aes(color = SGR, fill = SGR), position = "identity", bins = 30, alpha = 0.4) + scale_color_manual(values = c("#00AFBB", "#E7B800")) + scale_fill_manual(values = c("#00AFBB", "#E7B800")) + facet_wrap(~Production$species + Production$cleaning + Production$size)
ggplot(Survival_kopie, aes(x = survival)) + geom_histogram(aes(color = survival, fill = survival), position = "identity", bins = 30, alpha = 0.4) + scale_color_manual(values = c("#00AFBB", "#E7B800")) + scale_fill_manual(values = c("#00AFBB", "#E7B800")) + facet_wrap(~ Survival_kopie$cleaning + Survival_kopie$size + Survival_kopie$date)


#QQ plots
qqnorm(Growth$SGR)
qqline(Growth$SGR, col = "steelblue", lwd = 2)
qqnorm(Production$SGR)
qqline(Production$SGR, col = "steelblue", lwd = 2)
qqnorm(Survival_kopie$survival)
qqline(Survival_kopie$survival, col = "steelblue", lwd = 2)

#QQ plot transformed data
Production %>% ggplot(aes(sample = sqrt.production)) + geom_qq() + geom_qq_line() + facet_wrap(~Production$species + Production$cleaning + Production$size, scales = "free_y")

anov.growth <- anova_test(data = Growth, dv = Growth$SGR, wid = id, within = c(Growth$species, Growth$cleaning, Growth$size, Growth$date))

