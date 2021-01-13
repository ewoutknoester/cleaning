mean(Production$SGR)
mean(Growth$SGR)
library(dplyr)
Gemiddelde SGR van growth (>80% survival)
aggregate(cbind(Growth$SGR) ~ Growth$size + Growth$species + Growth$date + Growth$cleaning, data = Growth, FUN='mean', na.rm = TRUE)
df.growth<- data.frame(aggregate(cbind(Growth$SGR) ~ Growth$size + Growth$species + Growth$date + Growth$cleaning, data = Growth, FUN='mean', na.rm = TRUE)
                       
Gemiddelde SGR van production (alle survival waardes waardoor je groei en survival in een hebt)
aggregate(cbind(Production$SGR) ~ Production$size + Production$species + Production$date + Production$cleaning, data = Production, FUN='mean', na.rm = TRUE)
df.production<- data.frame(aggregate(cbind(Production$SGR) ~ Production$size + Production$species + Production$date + Production$cleaning, data = Production, FUN='mean', na.rm = TRUE))
print(df.production)
write_xlsx(df.production,"C:/Users/eveli/Documents/Wageningen University/MSc Thesis/Nieuwe thesis - cleaning interval/Data\\Mean Production.xlsx")
Growth %>% count(Growth$size, Growth$species, Growth$date, Growth$cleaning)

Grafieken: 
plot(df.growth$V1 ~ df.growth$Growth.date, data=subset(df.growth,df.growth$Growth.species=='Acropora formosa'& df.growth$Growth.cleaning=='1' & df.growth$Growth.size=='1'))
title(main = "Acropora formosa - weekly cleaning")
barplot(df.growth$V1, main="Acropora formosa- weekly cleaned",xlab="Date (month)", ylab="SGR (/d)")
barplot(subset(df.growth$v1, df.growth$Growth.species=='Acropora formosa'& df.growth$Growth.cleaning=='1' & df.growth$Growth.size=='1'))
ggplot(df.growth, aes(fill=Growth.species, y=V1, x=Growth.size)) + geom_bar(position="dodge", stat="identity")

Goede gg plots:
ggplot(df.growth, aes(fill=Growth.species, y=V1, x=Growth.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Growth.species + Growth.cleaning + Growth.size)
ggplot(df.production, aes(fill=Production.species, y=V1, x=Production.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Production.species + Production.cleaning + Production.size) 

Gemiddelde surival over groepen:
aggregate(cbind(Survival$survival) ~ Survival$size + Survival$species + Survival$date + Survival$cleaning, data = Survival, FUN='mean', na.rm = TRUE)
df.survival<- data.frame(aggregate(cbind(Survival$survival) ~ Survival$size + Survival$species + Survival$date + Survival$cleaning, data = Survival, FUN='mean', na.rm = TRUE))
aggregate(cbind(Survival_kopie$survival) ~ Survival_kopie$size + Survival_kopie$species + Survival_kopie$date + Survival_kopie$cleaning, data = Survival_kopie, FUN='mean', na.rm = TRUE)
ggplot(df.survival, aes(fill=Survival.species, y=V1, x=Survival.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Survival.species + Survival.cleaning + Survival.size) 

Met cleaning en size namen:
aggregate(cbind(Survival_kopie$survival) ~ Survival_kopie$size + Survival_kopie$species + Survival_kopie$date + Survival_kopie$cleaning, data = Survival_kopie, FUN='mean', na.rm = TRUE)
df.survival_kopie<- data.frame(aggregate(cbind(Survival_kopie$survival) ~ Survival_kopie$size + Survival_kopie$species + Survival_kopie$date + Survival_kopie$cleaning, data = Survival_kopie, FUN='mean', na.rm = TRUE)
ggplot(df.survival_kopie, aes(fill=Survival_kopie.species, y=V1, x=Survival_kopie.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Survival_kopie.species + Survival_kopie.cleaning + Survival_kopie.size)

Zelfde grafiek als eerder gemaakt:
ggplot(df.growth, aes(fill=Growth.size, y=V1, x=Growth.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Growth.species + Growth.cleaning)
ggplot(df.survival_kopie, aes(fill=Survival_kopie.size, y=V1, x=Survival_kopie.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Survival_kopie.species + Survival_kopie.cleaning)
ggplot(df.production, aes(fill=Production.size, y=V1, x=Production.date)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~Production.species + Production.cleaning)
ggplot(df.growth, aes(fill=Growth.size, y=V1, x=Growth.date)) + geom_col(position="dodge", stat="identity") + facet_wrap(~Growth.species + Growth.cleaning)

