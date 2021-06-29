library(mlr3)
library(mlr)
library(tidyverse)
data(Ozone, package = "mlbench")
ozoneTib <-  as_tibble(Ozone)
names(ozoneTib) <- c("months", "date", "day", "ozone", "press_height", "wind", "humid",
                     "temp_sand", "tetmp_monte", "inv_height", "press_grad", "inv_temp", "visib")
ozoneTib
#Cleaning data
#mutate doesn't alter names only values
ozoneClean <- mutate_all(ozoneTib, as.numeric) %>% filter(is.na(Ozone) == FALSE)
ozoneClean
#Plot
ozoneUntidy <- gather(ozoneClean, key = "Variable", value = "Value", -ozone)
ggplot(ozoneUntidy, aes(Value, ozone)) +  
  facet_wrap(~ Variable, scale = "free_x") + 
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm", col = "red") +
  theme_bw()

#How to input data correctly




