library(tidyverse)

#eliminating extra rows
CaliJan2022 <- Cali2022 %>%
  select(-Date, -Make, -`Model Year`)
CaliJan2022 <- CaliJan2022 %>%
  arrange(`Zip Code`)
view(CaliJan2022)  

#creating new data set for light cars in Los Angeles 
losAngelesLight22 <- CaliJan2022 %>%
  filter(`Zip Code`<= 91609, 
         Duty == "Light")
losAngelesLight22 <- losAngelesLight22 %>%
  arrange(`Zip Code`)
View(losAngelesLight22)

#creating new data set for heavy cars in Los Angeles
losAngelesHeavy22 <- CaliJan2022 %>%
  filter(`Zip Code`<= 91609,
         Duty == "Heavy")
losAngelesHeavy22 <- losAngelesHeavy22 %>%
  arrange(`Zip Code`)
View(losAngelesHeavy22)


#Finding the total number of light vehicles for each fuel type in Los Angeles
losAngelesLight22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Los Angeles
losAngelesHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))


#creating new data set for light cars in San Diego
sanDiegoLight22 <- CaliJan2022 %>%
  filter(`Zip Code`>= 92014,
         `Zip Code`<= 92199,
         Duty == "Light")
sanDiegoLight22 <- sanDiegoLight22 %>%
  arrange(`Zip Code`)
view(sanDiegoLight22)

#creating new data set for heavy cars in San Diego
sanDiegoHeavy22 <- CaliJan2022 %>%
  filter(`Zip Code`>= 92014,
         `Zip Code`<= 92199,
         Duty == "Heavy")
sanDiegoHeavy22 <- sanDiegoHeavy22 %>%
  arrange(`Zip Code`)
view(sanDiegoHeavy22)

#Finding the total number of light vehicles for each fuel type in San Diego
sanDiegoLight22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in San Diego
sanDiegoHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))


#creating new data set for light cars in Orange
orangeLight22 <- CaliJan2022 %>%
  filter(`Zip Code`>= 92856,
         `Zip Code`<=92869,
         Duty == "Light")
orangeLight22 <- orangeLight22 %>%
  arrange(`Zip Code`)
View(orangeLight22)

#creating new data set for heavy cars in Orange
orangeHeavy22 <- CaliJan2022 %>%
  filter(`Zip Code`>= 92856,
         `Zip Code`<=92869,
         Duty == "Heavy")
orangeHeavy22 <- orangeHeavy22 %>%
  arrange(`Zip Code`)
View(orangeHeavy22)

#Finding the total number of light vehicles for each fuel type in Orange
orangeLight22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Orange
orangeHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light cars in Alameda
alamedaLight22 <- CaliJan2022 %>%
  filter(`Zip Code` >= 94501,
         `Zip Code` <= 94502,
         Duty == "Light")
alam <- CaliJan2022 %>%
  filter(`Zip Code` == 94606,
         Duty == "Light")
alamedaLight22 <- rbind(alamedaLight22, alam)
alamedaLight22 <- alamedaLight22 %>%
  arrange(`Zip Code`)
View(alamedaLight22)

#creating new data set for heavy cars in Alameda
alamedaHeavy22 <- CaliJan2022 %>%
  filter(`Zip Code` >= 94501,
         `Zip Code` <= 94502,
         Duty == "Heavy")
alam <- CaliJan2022 %>%
  filter(`Zip Code` == 94606,
         Duty == "Heavy")
alamedaHeavy22 <- rbind(alamedaHeavy22, alam)
alamedaHeavy22 <- alamedaHeavy22 %>%
  arrange(`Zip Code`)
View(alamedaHeavy22)

#Finding the total number of light vehicles for each fuel type in Alameda
alamedaLight22 %>%
  group_by(Fuel)%>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Alameda
alamedaHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light cars in Sacramento
sacLight22 <- CaliJan2022 %>%
  filter(`Zip Code`>= 94203,
         `Zip Code`<= 94299,
         Duty == "Light")
sac <- CaliJan2022 %>%
  filter(`Zip Code` >= 95811,
         `Zip Code`<= 95864,
         Duty == "Light")
sacLight22 <- rbind(sacLight22, sac)
sacLight22 <- sacLight22 %>%
  arrange(`Zip Code`)
View(sacLight22)

#creating new data set for heavy cars in Sacramento
sacHeavy22 <- CaliJan2022 %>%
  filter(`Zip Code`>= 94203,
         `Zip Code`<= 94299,
         Duty == "Heavy")
sac <- CaliJan2022 %>%
  filter(`Zip Code` >= 95811,
         `Zip Code`<= 95864,
         Duty == "Heavy")
sacHeavy22 <- rbind(sacHeavy22, sac)
sacHeavy22 <- sacHeavy22 %>%
  arrange(`Zip Code`)
View(sacHeavy22)

#Finding the total number of light vehicles for each fuel type in Sacramento
sacLight22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Sacramento
sacHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Alpine
alpineLight22 <- CaliJan2022 %>%
  filter((`Zip Code` == 91901 |
            `Zip Code` == 91903 |
            `Zip Code` == 92019),
         Duty == "Light")
View(alpineLight22)

alpineHeavy22 <- CaliJan2022 %>%
  filter((`Zip Code` == 91901 |
            `Zip Code` == 91903 |
            `Zip Code` == 92019),
         Duty == "Heavy")
View(alpineHeavy22)

#Finding total number of light and heavy cars for each fuel type in Alpine
alpineLight22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

alpineHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Inyo
inyoLight22 <- CaliJan2022 %>%
  filter((`Zip Code` == 93514 |
            `Zip Code` == 92364 |
            `Zip Code` == 93513 |
            `Zip Code` == 93527 |
            `Zip Code` == 93545 |
            `Zip Code` == 92328 |
            `Zip Code` == 93526 |
            `Zip Code` == 93549 |
            `Zip Code` == 93592 |
            `Zip Code` == 93522 |
            `Zip Code` == 93530 |
            `Zip Code` == 92384 |
            `Zip Code` == 92389 |
            `Zip Code` == 93515 |
            `Zip Code` == 93542 ),
         Duty == "Light")
View(inyoLight22)

inyoHeavy22 <- CaliJan2022 %>%
  filter((`Zip Code` == 93514 |
            `Zip Code` == 92364 |
            `Zip Code` == 93513 |
            `Zip Code` == 93527 |
            `Zip Code` == 93545 |
            `Zip Code` == 92328 |
            `Zip Code` == 93526 |
            `Zip Code` == 93549 |
            `Zip Code` == 93592 |
            `Zip Code` == 93522 |
            `Zip Code` == 93530 |
            `Zip Code` == 92384 |
            `Zip Code` == 92389 |
            `Zip Code` == 93515 |
            `Zip Code` == 93542 ),
         Duty == "Heavy")
View(inyoHeavy22)

#Finding total number of light and heavy cars for each fuel type in Inyo
inyoLight22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

inyoHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Modoc
modocLight22 <- CaliJan2022 %>%
  filter((`Zip Code` == 96101 | 
            `Zip Code` == 96134 | 
            `Zip Code` == 96058 | 
            `Zip Code` == 96056 | 
            `Zip Code` == 96104 | 
            `Zip Code` == 96110 | 
            `Zip Code` == 96115 |
            `Zip Code` == 96006 |
            `Zip Code` == 96015 |
            `Zip Code` == 96108 |
            `Zip Code` == 96116 |
            `Zip Code` == 96112 |
            `Zip Code` == 97635 |
            `Zip Code` == 96054),
         Duty == "Light")
View(modocLight22)

modocHeavy22 <- CaliJan2022 %>%
  filter((`Zip Code` == 96101 | 
            `Zip Code` == 96134 | 
            `Zip Code` == 96058 | 
            `Zip Code` == 96056 | 
            `Zip Code` == 96104 | 
            `Zip Code` == 96110 | 
            `Zip Code` == 96115 |
            `Zip Code` == 96006 |
            `Zip Code` == 96015 |
            `Zip Code` == 96108 |
            `Zip Code` == 96116 |
            `Zip Code` == 96112 |
            `Zip Code` == 97635 |
            `Zip Code` == 96054),
         Duty == "Heavy")
View(modocHeavy22)

#Finding total number of light and heavy cars for each fuel type in Modoc
modocLight22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

modocHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Sierra
sierraLight22 <- CaliJan2022 %>%
  filter((`Zip Code` == 95910 |
            `Zip Code` == 95936 |
            `Zip Code` == 95944 |
            `Zip Code` == 96118 |
            `Zip Code` == 96124 |
            `Zip Code` == 96125 |
            `Zip Code` == 96126),
         Duty == "Light")
View(sierraLight22)

sierraHeavy22 <- CaliJan2022 %>%
  filter((`Zip Code` == 95910 |
            `Zip Code` == 95936 |
            `Zip Code` == 95944 |
            `Zip Code` == 96118 |
            `Zip Code` == 96124 |
            `Zip Code` == 96125 |
            `Zip Code` == 96126),
         Duty == "Heavy")
View(sierraHeavy22)

#Finding total number of light and heavy cars for each fuel type in Sierra
sierraLight22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

sierraHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Trinity
trinityLight22 <- CaliJan2022 %>%
  filter((`Zip Code` == 96067 |
            `Zip Code` == 96093 |
            `Zip Code` == 95454 |
            `Zip Code` == 96041 |
            `Zip Code` == 96076 |
            `Zip Code` == 96048 |
            `Zip Code` == 96046 |
            `Zip Code` == 96052 |
            `Zip Code` == 96024 |
            `Zip Code` == 95563 |
            `Zip Code` == 96034 |
            `Zip Code` == 96091 |
            `Zip Code` == 96010 |
            `Zip Code` == 96031 |
            `Zip Code` == 96033 |
            `Zip Code` == 95552 |
            `Zip Code` == 95573 |
            `Zip Code` == 95526 |
            `Zip Code` == 95527 |
            `Zip Code` == 95595 |
            `Zip Code` == 95511),
         Duty == "Light")
View(trinityLight22)

trinityHeavy22 <- CaliJan2022 %>%
  filter((`Zip Code` == 96067 |
            `Zip Code` == 96093 |
            `Zip Code` == 95454 |
            `Zip Code` == 96041 |
            `Zip Code` == 96076 |
            `Zip Code` == 96048 |
            `Zip Code` == 96046 |
            `Zip Code` == 96052 |
            `Zip Code` == 96024 |
            `Zip Code` == 95563 |
            `Zip Code` == 96034 |
            `Zip Code` == 96091 |
            `Zip Code` == 96010 |
            `Zip Code` == 96031 |
            `Zip Code` == 96033 |
            `Zip Code` == 95552 |
            `Zip Code` == 95573 |
            `Zip Code` == 95526 |
            `Zip Code` == 95527 |
            `Zip Code` == 95595 |
            `Zip Code` == 95511),
         Duty == "Heavy")
View(trinityHeavy22)

#Finding total number of light and heavy cars for each fuel type in Trinity
trinityLight22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

trinityHeavy22 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))
