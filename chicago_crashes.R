library(tidyverse)
library(dplyr)
library(magrittr)

# Filter by causes
chicago <- chicago %>%
  filter(`PRIM_CONTRIBUTORY_CAUSE` != "UNABLE TO DETERMINE")

#Plot frequency of each crash
chicago %>% 
  ggplot(aes(x = fct_infreq(PRIM_CONTRIBUTORY_CAUSE))) +
  geom_bar(position = "dodge",
           alpha = 1,
           fill = "lightblue") +
  theme_minimal() +
  #scale_fill_manual(values = c("blue")) +
  theme(panel.grid.major = element_blank()) +
  labs(title = "Frequency of Different Crash Reasons",
       x = "Reasons",
       y = "Count"
    ) + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 6))


    

  
  
  


  
  
      
    

    
    
  


    




