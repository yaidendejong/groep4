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
colnames(df1)[2] <- "Studieschuld_NL"
colnames(df2)[1] <- "Year"
colnames(df2)[2] <- "Studieschuld_UK"
colnames(df3)[1] <- "Year"
colnames(df3)[2] <- "Studieschuld_US"

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
df_sorted$Studieschuld_NL <- as.numeric(df_sorted$Studieschuld_NL)
df_sorted$Studieschuld_UK <- as.numeric(df_sorted$Studieschuld_UK)
df_sorted$Studieschuld_US <- as.numeric(df_sorted$Studieschuld_US)

# Nieuwe Variabele: Jaarlijkse groei berekenen (percentage)
df_sorted$Growth_NL <- c(NA, diff(df_sorted$Studieschuld_NL) / head(df_sorted$Studieschuld_NL, -1) * 100)
df_sorted$Growth_UK <- c(NA, diff(df_sorted$Studieschuld_UK) / head(df_sorted$Studieschuld_UK, -1) * 100)
df_sorted$Growth_US <- c(NA, diff(df_sorted$Studieschuld_US) / head(df_sorted$Studieschuld_US, -1) * 100)

#Kolommen omzetten in numeriek
df_numeric <- df_sorted %>%
  mutate(across(c(Studieschuld_NL, Studieschuld_UK, Studieschuld_US, Growth_NL, Growth_UK, Growth_US), ~as.numeric(.)),
         Year = as.numeric(Year))

#Gemiddelde growth uitrekenen
NL_Mean_Growth = mean(df_sorted$Growth_NL, na.rm = TRUE)
UK_Mean_Growth = mean(df_sorted$Growth_UK, na.rm = TRUE)
US_Mean_Growth = mean(df_sorted$Growth_US, na.rm = TRUE)


#Alles in Euros

df_numeric <- df_numeric %>%
  mutate(Studieschuld_US = Studieschuld_US * 0.86)

df_numeric <- df_numeric %>%
  mutate(Studieschuld_UK = Studieschuld_UK * 1.17)




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
    x = 2023,
    y = as.numeric(df_numeric$NL[df_numeric$Year == "2024"]) -5000,
    label = paste0("Mean growth NL: ", round(NL_Mean_Growth, 2), "%"),
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  annotate(
    "text",
    x = 2023,
    y = as.numeric(df_numeric$UK[df_numeric$Year == "2024"]) -7000,
    label = paste0("Mean growth UK: ", round(UK_Mean_Growth, 2), "%"),
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  annotate(
    "text",
    x = 2023,
    y = as.numeric(df_numeric$US[df_numeric$Year == "2024"]) -5000,
    label = paste0("Mean growth US: ", round(US_Mean_Growth, 2), "%"),
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  
  
  scale_x_continuous(
    limits = c(2014, NA),
    breaks = seq(2014, max(as.numeric(df_numeric$Year), na.rm = TRUE), by = 1)  # alle jaren
  ) + 
  labs(title = "Growth in Student Debt per Country pre- and post-Covid-19",
       x = "Year",
       y = "Student Debt",
       color = "Country") +
  theme_bw()

#Opslaan als PNG
ggsave("Temporal_Visualization.png", width = 8, height = 5) 





# een categorie met over alle landen heen de studieschuld gemiddeld van de 1e helft vanje tijdsperiode



# een categorie met per land de studieschuld gemiddeld van de 2e helft vanje tijdsperiode


data_selected <- df_numeric %>%
  select(Year, starts_with("Studieschuld"))

# Data omvormen naar long format
data_long <- data_selected %>%
  pivot_longer(
    cols = -Year,
    names_to = c("variabele", "land"),
    names_pattern = "(Studieschuld)_(.*)"
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
gemiddelde <- data_selected %>% 
  summarise(
    Studieschuld_NL = mean(Studieschuld_NL, na.rm=TRUE),
    Studieschuld_UK = mean(Studieschuld_UK, na.rm=TRUE),
    Studieschuld_US = mean(Studieschuld_US, na.rm=TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "land", values_to = "gemiddelde_schuld") %>%
  mutate(
    land = case_when(
      land == "Studieschuld_NL" ~ "Netherlands",
      land == "Studieschuld_UK" ~ "United Kingdom",
      land == "Studieschuld_US" ~ "United States of America"
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
    name = "Mean Student Debt (€)"
  ) +
  coord_sf(
    xlim = c(-130, 10), 
    ylim = c(20, 70)
  ) +
  theme_bw() +
  labs(title = "Mean Student Debt (€) (2007–2024)")



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


#Plot 5
gdp_per_capita_data <- read_xlsx("gdp_per_capita2.xlsx")


head(gdp_per_capita_data)
str(gdp_per_capita_data)



# De derde rij bevat de jaar-kolomnamen. We extraheren deze.
jaar_kolommen <- as.character(unlist(gdp_per_capita_data[3, ]))
# Sommige kolommen heten nog NA, die maken we netjes
jaar_kolommen[is.na(jaar_kolommen)] <- paste0("V", which(is.na(jaar_kolommen)))

# De echte data begint vanaf rij 4
df_gdp <- gdp_per_capita_data[4:6, ]

# We zetten de kolomnamen goed
colnames(df_gdp) <- jaar_kolommen

# We selecteren alleen de relevante kolommen: Land en de jaren
df_gdp <- df_gdp %>%
  select(`Country Name`, `2007`:`2023`)

# Nu zetten we de data in long format
df_gdp_long <- df_gdp %>%
  pivot_longer(
    cols = -`Country Name`,
    names_to = "Year",
    values_to = "GDP_PC"
  )

# Eventueel converteren we de getallen van character naar numeric
df_gdp_long$GDP_PC <- as.numeric(df_gdp_long$GDP_PC)
df_gdp_long$Year <- as.integer(df_gdp_long$Year)

# Vervolgens naar breed formaat
df_gdp_wide <- df_gdp_long %>%
  pivot_wider(
    names_from = `Country Name`,
    values_from = GDP_PC
  ) %>%
  rename(
    GDP_PC_UK = `United Kingdom`,
    GDP_PC_NL = Netherlands,
    GDP_PC_US = `United States`
  ) %>%
  arrange(Year)

df_gdp_wide

df_gdp_wide <- df_gdp_wide %>%
  mutate(GDP_PC_UK = GDP_PC_UK * 0.85)

df_gdp_wide <- df_gdp_wide %>%
  mutate(GDP_PC_NL = GDP_PC_NL * 0.85)

df_gdp_wide <- df_gdp_wide %>%
  mutate(GDP_PC_US = GDP_PC_US * 0.85)


df_numeric

df_merged <- full_join(df_numeric, df_gdp_wide, by = "Year")

df_merged <- df_merged %>%
  mutate(
    Schuld_GDP_NL = 100 * Studieschuld_NL / GDP_PC_NL,
    Schuld_GDP_UK = 100 * Studieschuld_UK / GDP_PC_UK,
    Schuld_GDP_US = 100 * Studieschuld_US / GDP_PC_US
  )

view(df_merged)


library(tidyr)

df_long <- df_merged %>%
  select(Year, Schuld_GDP_NL, Schuld_GDP_UK, Schuld_GDP_US) %>%
  pivot_longer(
    cols = starts_with("Schuld_GDP"),
    names_to = "Land",
    values_to = "Schuld_GDP"
  ) %>%
  mutate(
    Land = case_when(
      Land == "Schuld_GDP_NL" ~ "NL",
      Land == "Schuld_GDP_UK" ~ "UK",
      Land == "Schuld_GDP_US" ~ "US"
    )
  )

df_long %>%
  filter(Year >= 2015, Year <= 2023) %>%
  ggplot(aes(x = Year, y = Schuld_GDP, color = Land)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Student Debt as percentage of GDP per capita",
    x = "Year",
    y = "Student Debt / GDP per capita (%)",
    color = "Land"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = 2014:2023) 
  theme_bw()

ggsave("Event_Analysis.png", width = 8, height = 5) 

