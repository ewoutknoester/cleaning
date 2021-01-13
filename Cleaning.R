// Test script
//12 jan 
10:25
10:31
mean(Production$SGR)
mean(Growth$SGR)
library(dplyr)
Gemiddelde SGR van growth (>80% survival)
aggregate(cbind(Growth$SGR) ~ Growth$size + Growth$species + Growth$date + Growth$cleaning, data = Growth, FUN='mean', na.rm = TRUE)
Gemiddelde SGR van production (alle survival waardes waardoor je groei en survival in een hebt)
aggregate(cbind(Production$SGR) ~ Production$size + Production$species + Production$date + Production$cleaning, data = Production, FUN='mean', na.rm = TRUE)
df.production<- data.frame(aggregate(cbind(Production$SGR) ~ Production$size + Production$species + Production$date + Production$cleaning, data = Production, FUN='mean', na.rm = TRUE)
print(df.production)
write_xlsx(df.production,"C:/Users/eveli/Documents/Wageningen University/MSc Thesis/Nieuwe thesis - cleaning interval/Data\\Mean Production.xlsx")
Growth %>% count(Growth$size, Growth$species, Growth$date, Growth$cleaning)