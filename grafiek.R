install.packages("tidyr")

library(ggplot2)



colnames(Studieschuld_Alle_Landen)

colnames(Studieschuld_Alle_Landen) <- c("year", "nederland", "engeland", "usa")
library(tidyr)

long_data <- pivot_longer(Studieschuld_Alle_Landen, 
                          cols = c(nederland, engeland, usa),
                          names_to = "land", 
                          values_to = "schuld")

library(ggplot2)

ggplot(long_data, aes(x = year, y = schuld, color = land)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Studieschuld per jaar per land",
       x = "Jaar",
       y = "Studieschuld (€)",
       color = "Land") +
  theme_minimal()

