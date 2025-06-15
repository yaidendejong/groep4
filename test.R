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
  geom_vline(xintercept = 2020.3, linetype = "dashed", color = "black") + 
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
  labs(title = "Growth in Student Debt per Country",
       x = "Year",
       y = "Student Debt",
       color = "Country") +
  theme_bw()

#Opslaan als PNG
ggsave("Temporal_Visualization.png", width = 8, height = 5) 




#Inkomens

library(dplyr)
library(tidyr)
library(stringr)


file_names <- list.files(path = "C:/Users/Yaide/Downloads/Mean_Income_Data", pattern = "income.*\\.csv", full.names = TRUE)

#Alle bestanden inlezen en jaar toevoegen
data_list <- lapply(file_names, function(f) {

    year <- str_extract(f, "\\d{4}")
  
  df <- read.csv(f, header = FALSE, sep = ";", skip = 2, stringsAsFactors = FALSE)
  

  names(df) <- c("Country", "Wages")
  

  df$Year <- as.integer(year)
  

  df$Wages <- as.numeric(gsub(",", ".", df$Wages))
  
  return(df)
})

#Alles samenvoegen
combined_df <- bind_rows(data_list)

#Naar wide format
wide_df <- combined_df %>%
  pivot_wider(names_from = Country, values_from = Wages) %>%
  arrange(Year) %>%
  rename(
    USA = `United States`,
    NL = Netherlands,
    UK = `United Kingdom`
  )

wide_df

inkomens_per_jaar <- wide_df[ , -2]

samengevoegd <- df_numeric %>%
  left_join(inkomens_per_jaar, by = "Year"

)

samengevoegd <- samengevoegd %>%
  rename(
    studieschuld_nl = NL.x,
    studieschuld_uk = UK.x,
    studieschuld_us = US,
    inkomen_nl = NL.y,
    inkomen_uk = UK.y,
    inkomen_us = USA
  )

View(samengevoegd)

write.csv(samengevoegd, "samengevoegd.csv", row.names = FALSE)





# een categorie met over alle landen heen de studieschuld gemiddeld van de 1e helft vanje tijdsperiode



# een categorie met per land de studieschuld gemiddeld van de 2e helft vanje tijdsperiode


data_selected <- data %>%
  select(Year, starts_with("studieschuld"))

# Data omvormen naar long format
data_long <- data_selected %>%
  pivot_longer(
    cols = -Year,
    names_to = c("variabele", "land"),
    names_pattern = "(studieschuld)_(.*)"
  ) %>%
  rename(studieschuld = value)

# Tijdsperiode labelen
data_long <- data_long %>%
  mutate(
    period = ifelse(Year <= 2015, "2007 - 2015", "2016 - 2024")
  )

# Gemiddelde per land per periode berekenen
data_avg <- data_long %>%
  group_by(land, period) %>%
  summarise(studieschuld_mean = mean(studieschuld, na.rm = TRUE), .groups = "drop")

# Klaarzetten voor de boxplot
ggplot(data_avg, aes(x = period, y = studieschuld_mean)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Mean Student Debt per Period (All Countries)",
       x = "Period",
       y = "Mean Student Debt") +

  theme_bw()

ggsave("Sub_Population.png", width = 8, height = 5) 




install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "viridis"))







#Gemiddelden 
gemiddeld <- data %>% 
  summarise(
    studieschuld_nl = mean(studieschuld_nl, na.rm=TRUE),
    studieschuld_uk = mean(studieschuld_uk, na.rm=TRUE),
    studieschuld_us = mean(studieschuld_us, na.rm=TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "land", values_to = "gemiddelde_schuld") %>%
  mutate(
    land = case_when(
      land == "studieschuld_nl" ~ "Netherlands",
      land == "studieschuld_uk" ~ "United Kingdom",
      land == "studieschuld_us" ~ "United States of America"
    )
  )


# Wereldkaart

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)



wereldkaart <- ne_countries(scale = "medium", returnclass = "sf")


wereldkaart_met_data <- wereldkaart %>%
  left_join(gemiddeld, by = c("admin" = "land"))


ggplot(wereldkaart_met_data) +
  geom_sf(aes(fill = gemiddelde_schuld), color = "black") +
  scale_fill_gradient(
    low = "lightblue", 
    high = "darkblue", 
    na.value = "lightgrey",      # <- grijs voor landen zonder data
    name = "Mean Student Debt"
  ) +
  coord_sf(
    xlim = c(-130, 10), 
    ylim = c(20, 70)
  ) +
  theme_bw() +
  labs(title = "Mean Student Debt (2007–2024)")



ggsave("Spatial_Visualization.png", width = 8, height = 5) 



#Procentuele groei per jaar
df_growth_long <- df_numeric %>%
  select(Year, Growth_NL, Growth_UK, Growth_US) %>%
  pivot_longer(
    cols = starts_with("Growth_"),
    names_to = "land",
    names_prefix = "Growth_",
    values_to = "groei_pct"
  )

df_growth_long %>%
  filter(Year >= 2015) %>%
  ggplot(aes(x = Year, y = groei_pct, color = land)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = 2015:2024) +
  theme_bw() +
  labs(
    title = "Procentual Growth of Student Debt (Since 2014)",
    x = "Year",
    y = "Growth compared to last year (%)",
    color = "Country"
  )

ggsave("Temporal_Visualization2.png", width = 8, height = 5) 





#Procentuele groei UK pre en post brexit
df_growth_long_UK <- df_numeric %>%
  select(Year, Growth_UK) %>%
  rename(groei_pct = Growth_UK)


ggplot(df_growth_long_UK, aes(x = Year, y = groei_pct)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue") +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = 2008:2024) +
  theme_bw() +
  labs(
    title = "Percentage Growth of UK Student Debt before and after Brexit (2016)",
    x = "Year",
    y = "Growth compared to last year (%)"
  )



    