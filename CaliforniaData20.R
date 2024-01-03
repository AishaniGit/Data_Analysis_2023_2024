library(tidyverse)

#eliminating extra rows
CaliJan2020 <- Cali2020 %>%
  select(-Date, -Make, -`Model Year`)
CaliJan2020 <- CaliJan2020 %>%
  arrange(`Zip Code`)
view(CaliJan2020)  

#creating new data set for light cars in Los Angeles 
losAngelesLight20 <- CaliJan2020 %>%
  filter(`Zip Code`<= 91609, 
         Duty == "Light")
losAngelesLight20 <- losAngelesLight20 %>%
  arrange(`Zip Code`)
View(losAngelesLight20)

#creating new data set for heavy cars in Los Angeles
losAngelesHeavy20 <- CaliJan2020 %>%
  filter(`Zip Code`<= 91609,
         Duty == "Heavy")
losAngelesHeavy20 <- losAngelesHeavy20 %>%
  arrange(`Zip Code`)
View(losAngelesHeavy20)


#Finding the total number of light vehicles for each fuel type in Los Angeles
losAngelesLight20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Los Angeles
losAngelesHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))


#creating new data set for light cars in San Diego
sanDiegoLight20 <- CaliJan2020 %>%
  filter(`Zip Code`>= 92014,
         `Zip Code`<= 92199,
         Duty == "Light")
sanDiegoLight20 <- sanDiegoLight20 %>%
  arrange(`Zip Code`)
view(sanDiegoLight20)

#creating new data set for heavy cars in San Diego
sanDiegoHeavy20 <- CaliJan2020 %>%
  filter(`Zip Code`>= 92014,
         `Zip Code`<= 92199,
         Duty == "Heavy")
sanDiegoHeavy20 <- sanDiegoHeavy20 %>%
  arrange(`Zip Code`)
view(sanDiegoHeavy20)

#Finding the total number of light vehicles for each fuel type in San Diego
sanDiegoLight20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in San Diego
sanDiegoHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))


#creating new data set for light cars in Orange
orangeLight20 <- CaliJan2020 %>%
  filter(`Zip Code`>= 92856,
         `Zip Code`<=92869,
         Duty == "Light")
orangeLight20 <- orangeLight20 %>%
  arrange(`Zip Code`)
View(orangeLight20)

#creating new data set for heavy cars in Orange
orangeHeavy20 <- CaliJan2020 %>%
  filter(`Zip Code`>= 92856,
         `Zip Code`<=92869,
         Duty == "Heavy")
orangeHeavy20 <- orangeHeavy20 %>%
  arrange(`Zip Code`)
View(orangeHeavy20)

#Finding the total number of light vehicles for each fuel type in Orange
orangeLight20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Orange
orangeHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light cars in Alameda
alamedaLight20 <- CaliJan2020 %>%
  filter(`Zip Code` >= 94501,
         `Zip Code` <= 94502,
         Duty == "Light")
alam <- CaliJan2020 %>%
  filter(`Zip Code` == 94606,
         Duty == "Light")
alamedaLight20 <- rbind(alamedaLight20, alam)
alamedaLight20 <- alamedaLight20 %>%
  arrange(`Zip Code`)
View(alamedaLight20)

#creating new data set for heavy cars in Alameda
alamedaHeavy20 <- CaliJan2020 %>%
  filter(`Zip Code` >= 94501,
         `Zip Code` <= 94502,
         Duty == "Heavy")
alam <- CaliJan2020 %>%
  filter(`Zip Code` == 94606,
         Duty == "Heavy")
alamedaHeavy20 <- rbind(alamedaHeavy20, alam)
alamedaHeavy20 <- alamedaHeavy20 %>%
  arrange(`Zip Code`)
View(alamedaHeavy20)

#Finding the total number of light vehicles for each fuel type in Alameda
alamedaLight20 %>%
  group_by(Fuel)%>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Alameda
alamedaHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light cars in Sacramento
sacLight20 <- CaliJan2020 %>%
  filter(`Zip Code`>= 94203,
         `Zip Code`<= 94299,
         Duty == "Light")
sac <- CaliJan2020 %>%
  filter(`Zip Code` >= 95811,
         `Zip Code`<= 95864,
         Duty == "Light")
sacLight20 <- rbind(sacLight20, sac)
sacLight20 <- sacLight20 %>%
  arrange(`Zip Code`)
View(sacLight20)

#creating new data set for heavy cars in Sacramento
sacHeavy20 <- CaliJan2020 %>%
  filter(`Zip Code`>= 94203,
         `Zip Code`<= 94299,
         Duty == "Heavy")
sac <- CaliJan2020 %>%
  filter(`Zip Code` >= 95811,
         `Zip Code`<= 95864,
         Duty == "Heavy")
sacHeavy20 <- rbind(sacHeavy20, sac)
sacHeavy20 <- sacHeavy20 %>%
  arrange(`Zip Code`)
View(sacHeavy20)

#Finding the total number of light vehicles for each fuel type in Sacramento
sacLight20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Sacramento
sacHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Alpine
alpineLight20 <- CaliJan2020 %>%
  filter((`Zip Code` == 91901 |
            `Zip Code` == 91903 |
            `Zip Code` == 92019),
         Duty == "Light")
View(alpineLight20)

alpineHeavy20 <- CaliJan2020 %>%
  filter((`Zip Code` == 91901 |
            `Zip Code` == 91903 |
            `Zip Code` == 92019),
         Duty == "Heavy")
View(alpineHeavy20)

#Finding total number of light and heavy cars for each fuel type in Alpine
alpineLight20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

alpineHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Inyo
inyoLight20 <- CaliJan2020 %>%
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
View(inyoLight20)

inyoHeavy20 <- CaliJan2020 %>%
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
View(inyoHeavy20)

#Finding total number of light and heavy cars for each fuel type in Inyo
inyoLight20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

inyoHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Modoc
modocLight20 <- CaliJan2020 %>%
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
View(modocLight20)

modocHeavy20 <- CaliJan2020 %>%
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
View(modocHeavy20)

#Finding total number of light and heavy cars for each fuel type in Modoc
modocLight20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

modocHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Sierra
sierraLight20 <- CaliJan2020 %>%
  filter((`Zip Code` == 95910 |
            `Zip Code` == 95936 |
            `Zip Code` == 95944 |
            `Zip Code` == 96118 |
            `Zip Code` == 96124 |
            `Zip Code` == 96125 |
            `Zip Code` == 96126),
         Duty == "Light")
View(sierraLight20)

sierraHeavy20 <- CaliJan2020 %>%
  filter((`Zip Code` == 95910 |
            `Zip Code` == 95936 |
            `Zip Code` == 95944 |
            `Zip Code` == 96118 |
            `Zip Code` == 96124 |
            `Zip Code` == 96125 |
            `Zip Code` == 96126),
         Duty == "Heavy")
View(sierraHeavy20)

#Finding total number of light and heavy cars for each fuel type in Sierra
sierraLight20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

sierraHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Trinity
trinityLight20 <- CaliJan2020 %>%
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
View(trinityLight20)

trinityHeavy20 <- CaliJan2020 %>%
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
View(trinityHeavy20)

#Finding total number of light and heavy cars for each fuel type in Trinity
trinityLight20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

trinityHeavy20 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))
