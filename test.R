#Packages downloaden
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)

#Dataframe maken van excel-bestand
df1 <- read_excel("Studieschuld_2011-2024(goeie).xlsx", skip = 1)
df2 <- read_excel("England_figure_10.xlsx")
df3 <- read_excel("Average Total Student Debt USA.xlsx")



#Rijen en Kolommen die niet nodig zijn verwijderen
df1 <- df1[-c(1, 2, 3), ]
df1 <- df1[, -c(2, 3, 6)]
df1 <- df1[, -c(2) ]
df2 <- df2[, -c(3)]


#Veranderen van schooljaar naar kalenderjaar
df2$'Financial year' <- 2007:2024  


#Kolomnamen overal hetzelfde maken voor samenvoegen
colnames(df1)[1] <- "Year"
colnames(df1)[2] <- "NL"
colnames(df2)[1] <- "Year"
colnames(df2)[2] <- "UK"
colnames(df3)[1] <- "Year"
colnames(df3)[2] <- "US"

#Year was blijkaar numeriek dus een 'karakter' van maken
df2$Year <- as.character(df2$Year)
df3$Year <- as.character(df3$Year)

#Stond een * bij 2023 en 24 bij NL waardoor hij dat als aparte variabele zag
df1$Year[df1$Year == "2023*"] <- 2023
df1$Year[df1$Year == "2024*"] <- 2024

# df1,2,3 samenvoegen
df_merge <- full_join(df1, df2, by = "Year")
df_merge <- full_join(df_merge, df3, by = "Year")

# Sorteren van Oud naar Nieuw
df_sorted <- df_merge[order(df_merge$Year, decreasing = FALSE), ]


# Alles numeriek maken
df_sorted$NL <- as.numeric(df_sorted$NL)
df_sorted$UK <- as.numeric(df_sorted$UK)
df_sorted$US <- as.numeric(df_sorted$US)

# Nieuwe Variabele: Jaarlijkse groei berekenen (percentage)
df_sorted$Growth_NL <- c(NA, diff(df_sorted$NL) / head(df_sorted$NL, -1) * 100)
df_sorted$Growth_UK <- c(NA, diff(df_sorted$UK) / head(df_sorted$UK, -1) * 100)
df_sorted$Growth_US <- c(NA, diff(df_sorted$US) / head(df_sorted$US, -1) * 100)

#Kolommen omzetten in numeriek
df_numeric <- df_sorted %>%
  mutate(across(c(NL, UK, US, Growth_NL, Growth_UK, Growth_US), ~as.numeric(.)),
         Year = as.numeric(Year))

#Gemiddelde growth uitrekenen
NL_Mean_Growth = mean(df_sorted$Growth_NL, na.rm = TRUE)
UK_Mean_Growth = mean(df_sorted$Growth_UK, na.rm = TRUE)
US_Mean_Growth = mean(df_sorted$Growth_US, na.rm = TRUE)

df_numeric

#Grafiek
ggplot(df_numeric, aes(x = Year)) +
  geom_line(aes(y = NL, color = "NL"), size = 1.5) +
  geom_line(aes(y = UK, color = "UK"), size = 1.5) +
  geom_line(aes(y = US, color = "US"), size = 1.5) +
  geom_point(aes(y = (NL), color = "NL"), size = 3) +
  geom_point(aes(y = (UK), color = "UK"), size = 3) +
  geom_point(aes(y = (US), color = "US"), size = 3) +
  annotate(
    "text",
    x = 2022.8,
    y = as.numeric(df_numeric$NL[df_numeric$Year == "2024"]) -5000,
    label = paste0("Mean growth NL:\n", round(NL_Mean_Growth, 2), "%"),
    hjust = 0,
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  annotate(
    "text",
    x = 2022.8,
    y = as.numeric(df_numeric$UK[df_numeric$Year == "2024"]) -7000,
    label = paste0("Mean growth UK:\n", round(UK_Mean_Growth, 2), "%"),
    hjust = 0,
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  annotate(
    "text",
    x = 2022.8,
    y = as.numeric(df_numeric$US[df_numeric$Year == "2024"]) -5000,
    label = paste0("Mean growth US:\n", round(US_Mean_Growth, 2), "%"),
    hjust = 0,
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  
  
  scale_x_continuous(
    limits = c(2014, NA),
    breaks = seq(2014, max(as.numeric(df_numeric$Year), na.rm = TRUE), by = 1)  # alle jaren
  ) + 
  labs(title = "Groei in studieschuld per jaar per land",
       x = "Jaar",
       y = "Studieschuld (€)",
       color = "Land") +
  theme_bw()

#Opslaan als PNG
ggsave("plot.png", width = 8, height = 5) 


#Boxplot
library(tidyr)
library(dplyr)
library(ggplot2)

df_long <- df_sorted %>%
  pivot_longer(cols = c(Growth_NL, Growth_US, Growth_UK),
               names_to = "Country",
               values_to = "Growth")


ggplot() +
  geom_boxplot(data = df_sorted, aes(y = Growth_NL, x = "NL", fill = "NL")) +
  geom_boxplot(data = df_sorted, aes(y = Growth_US, x = "US", fill = "US")) +
  geom_boxplot(data = df_sorted, aes(y = Growth_UK, x = "UK", fill = "UK")) +
  labs(title = "Boxplot of Growth per Country", x = "Country", y = "Growth") +
  theme_minimal()


