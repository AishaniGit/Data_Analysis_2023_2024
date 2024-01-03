library(tidyverse)

#eliminating extra rows
CaliJan2021 <- Cali2021 %>%
  select(-Date, -Make, -`Model Year`)
CaliJan2021 <- CaliJan2021 %>%
  arrange(`Zip Code`)
view(CaliJan2021)  

#creating new data set for light cars in Los Angeles 
losAngelesLight21 <- CaliJan2021 %>%
  filter(`Zip Code`<= 91609, 
         Duty == "Light")
losAngelesLight21 <- losAngelesLight21 %>%
  arrange(`Zip Code`)
View(losAngelesLight21)

#creating new data set for heavy cars in Los Angeles
losAngelesHeavy21 <- CaliJan2021 %>%
  filter(`Zip Code`<= 91609,
         Duty == "Heavy")
losAngelesHeavy21 <- losAngelesHeavy21 %>%
  arrange(`Zip Code`)
View(losAngelesHeavy21)
  

#Finding the total number of light vehicles for each fuel type in Los Angeles
losAngelesLight21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Los Angeles
losAngelesHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))


#creating new data set for light cars in San Diego
sanDiegoLight21 <- CaliJan2021 %>%
  filter(`Zip Code`>= 92014,
         `Zip Code`<= 92199,
         Duty == "Light")
sanDiegoLight21 <- sanDiegoLight21 %>%
  arrange(`Zip Code`)
view(sanDiegoLight21)

#creating new data set for heavy cars in San Diego
sanDiegoHeavy21 <- CaliJan2021 %>%
  filter(`Zip Code`>= 92014,
         `Zip Code`<= 92199,
         Duty == "Heavy")
sanDiegoHeavy21 <- sanDiegoHeavy21 %>%
  arrange(`Zip Code`)
view(sanDiegoHeavy21)

#Finding the total number of light vehicles for each fuel type in San Diego
sanDiegoLight21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in San Diego
sanDiegoHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))


#creating new data set for light cars in Orange
orangeLight21 <- CaliJan2021 %>%
  filter(`Zip Code`>= 92856,
         `Zip Code`<=92869,
         Duty == "Light")
orangeLight21 <- orangeLight21 %>%
  arrange(`Zip Code`)
View(orangeLight21)

#creating new data set for heavy cars in Orange
orangeHeavy21 <- CaliJan2021 %>%
  filter(`Zip Code`>= 92856,
         `Zip Code`<=92869,
         Duty == "Heavy")
orangeHeavy21 <- orangeHeavy21 %>%
  arrange(`Zip Code`)
View(orangeHeavy21)

#Finding the total number of light vehicles for each fuel type in Orange
orangeLight21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Orange
orangeHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light cars in Alameda
alamedaLight21 <- CaliJan2021 %>%
  filter(`Zip Code` >= 94501,
         `Zip Code` <= 94502,
         Duty == "Light")
alam <- CaliJan2021 %>%
  filter(`Zip Code` == 94606,
         Duty == "Light")
alamedaLight21 <- rbind(alamedaLight21, alam)
alamedaLight21 <- alamedaLight21 %>%
  arrange(`Zip Code`)
View(alamedaLight21)

#creating new data set for heavy cars in Alameda
alamedaHeavy21 <- CaliJan2021 %>%
  filter(`Zip Code` >= 94501,
         `Zip Code` <= 94502,
         Duty == "Heavy")
alam <- CaliJan2021 %>%
  filter(`Zip Code` == 94606,
         Duty == "Heavy")
alamedaHeavy21 <- rbind(alamedaHeavy21, alam)
alamedaHeavy21 <- alamedaHeavy21 %>%
  arrange(`Zip Code`)
View(alamedaHeavy21)

#Finding the total number of light vehicles for each fuel type in Alameda
alamedaLight21 %>%
  group_by(Fuel)%>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Alameda
alamedaHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light cars in Sacramento
sacLight21 <- CaliJan2021 %>%
  filter(`Zip Code`>= 94203,
         `Zip Code`<= 94299,
         Duty == "Light")
sac <- CaliJan2021 %>%
  filter(`Zip Code` >= 95811,
         `Zip Code`<= 95864,
         Duty == "Light")
sacLight21 <- rbind(sacLight21, sac)
sacLight21 <- sacLight21 %>%
  arrange(`Zip Code`)
View(sacLight21)

#creating new data set for heavy cars in Sacramento
sacHeavy21 <- CaliJan2021 %>%
  filter(`Zip Code`>= 94203,
         `Zip Code`<= 94299,
         Duty == "Heavy")
sac <- CaliJan2021 %>%
  filter(`Zip Code` >= 95811,
         `Zip Code`<= 95864,
         Duty == "Heavy")
sacHeavy21 <- rbind(sacHeavy21, sac)
sacHeavy21 <- sacHeavy21 %>%
  arrange(`Zip Code`)
View(sacHeavy21)

#Finding the total number of light vehicles for each fuel type in Sacramento
sacLight21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Sacramento
sacHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Alpine
alpineLight21 <- CaliJan2021 %>%
  filter((`Zip Code` == 91901 |
            `Zip Code` == 91903 |
            `Zip Code` == 92019),
         Duty == "Light")
View(alpineLight21)

alpineHeavy21 <- CaliJan2021 %>%
  filter((`Zip Code` == 91901 |
            `Zip Code` == 91903 |
            `Zip Code` == 92019),
         Duty == "Heavy")
View(alpineHeavy21)

#Finding total number of light and heavy cars for each fuel type in Alpine
alpineLight21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

alpineHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Inyo
inyoLight21 <- CaliJan2021 %>%
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
View(inyoLight21)

inyoHeavy21 <- CaliJan2021 %>%
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
View(inyoHeavy21)

#Finding total number of light and heavy cars for each fuel type in Inyo
inyoLight21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

inyoHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Modoc
modocLight21 <- CaliJan2021 %>%
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
View(modocLight21)

modocHeavy21 <- CaliJan2021 %>%
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
View(modocHeavy21)

#Finding total number of light and heavy cars for each fuel type in Modoc
modocLight21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

modocHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Sierra
sierraLight21 <- CaliJan2021 %>%
  filter((`Zip Code` == 95910 |
            `Zip Code` == 95936 |
            `Zip Code` == 95944 |
            `Zip Code` == 96118 |
            `Zip Code` == 96124 |
            `Zip Code` == 96125 |
            `Zip Code` == 96126),
         Duty == "Light")
View(sierraLight21)

sierraHeavy21 <- CaliJan2021 %>%
  filter((`Zip Code` == 95910 |
            `Zip Code` == 95936 |
            `Zip Code` == 95944 |
            `Zip Code` == 96118 |
            `Zip Code` == 96124 |
            `Zip Code` == 96125 |
            `Zip Code` == 96126),
         Duty == "Heavy")
View(sierraHeavy21)

#Finding total number of light and heavy cars for each fuel type in Sierra
sierraLight21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

sierraHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Trinity
trinityLight21 <- CaliJan2021 %>%
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
View(trinityLight21)

trinityHeavy21 <- CaliJan2021 %>%
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
View(trinityHeavy21)

#Finding total number of light and heavy cars for each fuel type in Trinity
trinityLight21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

trinityHeavy21 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))
