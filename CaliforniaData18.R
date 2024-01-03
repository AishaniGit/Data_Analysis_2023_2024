library(tidyverse)

#eliminating extra rows
CaliJan2018 <- Cali2018 %>%
  select(-Date, -Make, -`Model Year`)
CaliJan2018 <- CaliJan2018 %>%
  arrange(`Zip Code`)
view(CaliJan2018)


#creating new data set for light cars in Los Angeles 
losAngelesLight18 <- CaliJan2018 %>%
  filter(`Zip Code`<= 91609, 
         Duty == "Light")
losAngelesLight18 <- losAngelesLight18 %>%
  arrange(`Zip Code`)
View(losAngelesLight18)

#creating new data set for heavy cars in Los Angeles
losAngelesHeavy18 <- CaliJan2018 %>%
  filter(`Zip Code`<= 91609,
         Duty == "Heavy")
losAngelesHeavy18 <- losAngelesHeavy18 %>%
  arrange(`Zip Code`)
View(losAngelesHeavy18)


#Finding the total number of light vehicles for each fuel type in Los Angeles
losAngelesLight18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Los Angeles
losAngelesHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))


#creating new data set for light cars in San Diego
sanDiegoLight18 <- CaliJan2018 %>%
  filter(`Zip Code`>= 92014,
         `Zip Code`<= 92199,
         Duty == "Light")
sanDiegoLight18 <- sanDiegoLight18 %>%
  arrange(`Zip Code`)
view(sanDiegoLight18)

#creating new data set for heavy cars in San Diego
sanDiegoHeavy18 <- CaliJan2018 %>%
  filter(`Zip Code`>= 92014,
         `Zip Code`<= 92199,
         Duty == "Heavy")
sanDiegoHeavy18 <- sanDiegoHeavy18 %>%
  arrange(`Zip Code`)
view(sanDiegoHeavy18)

#Finding the total number of light vehicles for each fuel type in San Diego
sanDiegoLight18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in San Diego
sanDiegoHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))


#creating new data set for light cars in Orange
orangeLight18 <- CaliJan2018 %>%
  filter(`Zip Code`>= 92856,
         `Zip Code`<=92869,
         Duty == "Light")
orangeLight18 <- orangeLight18 %>%
  arrange(`Zip Code`)
View(orangeLight18)

#creating new data set for heavy cars in Orange
orangeHeavy18 <- CaliJan2018 %>%
  filter(`Zip Code`>= 92856,
         `Zip Code`<=92869,
         Duty == "Heavy")
orangeHeavy18 <- orangeHeavy18 %>%
  arrange(`Zip Code`)
View(orangeHeavy18)

#Finding the total number of light vehicles for each fuel type in Orange
orangeLight18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Orange
orangeHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light cars in Alameda
alamedaLight18 <- CaliJan2018 %>%
  filter(`Zip Code` >= 94501,
         `Zip Code` <= 94502,
         Duty == "Light")
alam <- CaliJan2018 %>%
  filter(`Zip Code` == 94606,
         Duty == "Light")
alamedaLight18 <- rbind(alamedaLight18, alam)
alamedaLight18 <- alamedaLight18 %>%
  arrange(`Zip Code`)
View(alamedaLight18)

#creating new data set for heavy cars in Alameda
alamedaHeavy18 <- CaliJan2018 %>%
  filter(`Zip Code` >= 94501,
         `Zip Code` <= 94502,
         Duty == "Heavy")
alam <- CaliJan2018 %>%
  filter(`Zip Code` == 94606,
         Duty == "Heavy")
alamedaHeavy18 <- rbind(alamedaHeavy18, alam)
alamedaHeavy18 <- alamedaHeavy18 %>%
  arrange(`Zip Code`)
View(alamedaHeavy18)

#Finding the total number of light vehicles for each fuel type in Alameda
alamedaLight18 %>%
  group_by(Fuel)%>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Alameda
alamedaHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light cars in Sacramento
sacLight18 <- CaliJan2018 %>%
  filter(`Zip Code`>= 94203,
         `Zip Code`<= 94299,
         Duty == "Light")
sac <- CaliJan2018 %>%
  filter(`Zip Code` >= 95811,
         `Zip Code`<= 95864,
         Duty == "Light")
sacLight18 <- rbind(sacLight18, sac)
sacLight18 <- sacLight18 %>%
  arrange(`Zip Code`)
View(sacLight18)

#creating new data set for heavy cars in Sacramento
sacHeavy18 <- CaliJan2018 %>%
  filter(`Zip Code`>= 94203,
         `Zip Code`<= 94299,
         Duty == "Heavy")
sac <- CaliJan2018 %>%
  filter(`Zip Code` >= 95811,
         `Zip Code`<= 95864,
         Duty == "Heavy")
sacHeavy18 <- rbind(sacHeavy18, sac)
sacHeavy18 <- sacHeavy18 %>%
  arrange(`Zip Code`)
View(sacHeavy18)

#Finding the total number of light vehicles for each fuel type in Sacramento
sacLight18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#Finding the total number of heavy vehicles for each fuel type in Sacramento
sacHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Alpine
alpineLight18 <- CaliJan2018 %>%
  filter((`Zip Code` == 91901 |
            `Zip Code` == 91903 |
            `Zip Code` == 92019),
         Duty == "Light")
View(alpineLight18)

alpineHeavy18 <- CaliJan2018 %>%
  filter((`Zip Code` == 91901 |
            `Zip Code` == 91903 |
            `Zip Code` == 92019),
         Duty == "Heavy")
View(alpineHeavy18)

#Finding total number of light and heavy cars for each fuel type in Alpine
alpineLight18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

alpineHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Inyo
inyoLight18 <- CaliJan2018 %>%
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
View(inyoLight18)

inyoHeavy18 <- CaliJan2018 %>%
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
View(inyoHeavy18)

#Finding total number of light and heavy cars for each fuel type in Inyo
inyoLight18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

inyoHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Modoc
modocLight18 <- CaliJan2018 %>%
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
View(modocLight18)

modocHeavy18 <- CaliJan2018 %>%
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
View(modocHeavy18)

#Finding total number of light and heavy cars for each fuel type in Modoc
modocLight18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

modocHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))
  
#creating new data set for light and heavy cars in Sierra
sierraLight18 <- CaliJan2018 %>%
  filter((`Zip Code` == 95910 |
            `Zip Code` == 95936 |
            `Zip Code` == 95944 |
            `Zip Code` == 96118 |
            `Zip Code` == 96124 |
            `Zip Code` == 96125 |
            `Zip Code` == 96126),
         Duty == "Light")
View(sierraLight18)

sierraHeavy18 <- CaliJan2018 %>%
  filter((`Zip Code` == 95910 |
            `Zip Code` == 95936 |
            `Zip Code` == 95944 |
            `Zip Code` == 96118 |
            `Zip Code` == 96124 |
            `Zip Code` == 96125 |
            `Zip Code` == 96126),
         Duty == "Heavy")
View(sierraHeavy18)

#Finding total number of light and heavy cars for each fuel type in Sierra
sierraLight18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

sierraHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

#creating new data set for light and heavy cars in Trinity
trinityLight18 <- CaliJan2018 %>%
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
View(trinityLight18)

trinityHeavy18 <- CaliJan2018 %>%
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
View(trinityHeavy18)

#Finding total number of light and heavy cars for each fuel type in Trinity
trinityLight18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))

trinityHeavy18 %>%
  group_by(Fuel) %>%
  summarise(sum(Vehicles))
