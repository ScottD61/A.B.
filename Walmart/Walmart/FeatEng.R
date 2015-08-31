#Load data
Schedule <- read.csv("\\\\na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/RSheet.csv",
                     na.strings = "", header = TRUE)
#Feature engineering
library(dplyr)
library(tidyr)
Newvar <- Schedule %>% gather(Day, del, -Store.Number, -Grand.Total) %>%
    mutate(proportion = ifelse(del/Grand.Total >= 0.5, 1,
                               ifelse(del/Grand.Total >= 0.32, 2,
                                      ifelse(del/Grand.Total >= 0.25, 3,
                                             ifelse(del/Grand.Total >= 0.20, 4,
                                                    NA))))) %>%
    group_by(Store.Number) %>%
    summarise(days = paste0(substr(Day[which(
        proportion == min(proportion, na.rm = TRUE))],
        1, 2), collapse = "")) %>%
    merge(Schedule, ., by = "Store.Number")
#Export data.frame to .csv
Comp <- write.csv(file = "Newfeat.csv", x = Newvar)