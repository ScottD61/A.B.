#Reproducible example
Store_Num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#Total deliveries made per week
Sun_Del <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
Mon_Del <- c(10, 50, 51, 7, 80, 97, 21, 49, 30, 3)
Tue_Del <- c(7, NA, 2, 50, 5, 56, 1, 4, 35, 52)
Wed_Del <- c(49, 51, 1, 4, 51, 16, 2, 2, 1, 1)
Thu_Del <- c(3, 2, 47, 7, 40, 2, 6, 5, 1, 7)
Fri_Del <- c(50, 49, 3, 51, 53, 86, 9, 52, 25, 52)
Sat_Del <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
Total <- c(119, 152, 104, 119, 229, 257, 39, 112, 92, 115)
#Single dataset
Schedule <- data.frame(Store_Num, Sun_Del, Mon_Del, Tue_Del,
                       Wed_Del, Thu_Del, Fri_Del, Sat_Del, Total)
Schedule


library(dplyr)
library(tidyr)
Schedule %>% gather(Day, del, -Store_Num, -Total) %>%
    mutate(proportion = ifelse(del/Total >= 0.5, 1,
                               ifelse(del/Total >= 0.32, 2,
                                      ifelse(del/Total >= 0.25, 3,
                                             ifelse(del/Total >= 0.20, 4,
                                                    NA))))) %>%
    group_by(Store_Num) %>%
    summarise(days = paste0(substr(Day[which(
        proportion == min(proportion, na.rm = TRUE))],
        1, 2), collapse = "")) %>%
    merge(Schedule, ., by = "Store_Num")
