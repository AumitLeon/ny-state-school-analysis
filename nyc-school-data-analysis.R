library(tidyverse)
library(ggplot2)

district_data <- read_csv("~/Downloads/District2019AllStudents.csv")

View(district_data)

sizes <- district_data %>%
  select(`DISTRICT NAME`, `PK12 TOTAL`, `SCHOOL YEAR`) %>%
  arrange(`PK12 TOTAL`)

# Get average of NYC school district sizes:
district_data %>%
  select(`DISTRICT NAME`, `PK12 TOTAL`, `SCHOOL YEAR`) %>%
  filter(substr(`DISTRICT NAME`, 1, 3) == 'NYC') %>%
  group_by(`DISTRICT NAME`) %>%
  summarize(mean = mean(`PK12 TOTAL`))

# Average district size without NYC across the state
district_data %>%
  select(`DISTRICT NAME`, `PK12 TOTAL`) %>%
  filter(substr(`DISTRICT NAME`, 1, 3) != 'NYC') %>%
  group_by(`DISTRICT NAME`) %>%
  summarize(mean = mean(`PK12 TOTAL`)) 

# Average district size across the state including NYC
district_data %>%
  select(`DISTRICT NAME`, `PK12 TOTAL`, `SCHOOL YEAR`) %>%
  summarize(mean = mean(`PK12 TOTAL`))

# Brookfield district enrollment size
district_data %>%
  select(`DISTRICT NAME`, `PK12 TOTAL`, `SCHOOL YEAR`) %>%
  filter(`DISTRICT NAME` == 'BROOKFIELD') %>%
  summarize(mean_enrollment = mean(`PK12 TOTAL`))

# Largest district in NYC -- NYC GEOG DIST # 2 - MANHATTAN
district_data %>%
  select(`DISTRICT NAME`, `PK12 TOTAL`, `SCHOOL YEAR`) %>%
  filter(substr(`DISTRICT NAME`, 1, 3) == 'NYC') %>%
  group_by(`DISTRICT NAME`) %>%
  arrange(-`PK12 TOTAL`) %>%
  View()

# Average enrollment size in NYC district 2
district_data %>%
  select(`DISTRICT NAME`, `PK12 TOTAL`, `SCHOOL YEAR`) %>%
  filter(`DISTRICT NAME` == 'NYC GEOG DIST # 2 - MANHATTAN') %>%
  group_by(`DISTRICT NAME`) %>%
  summarize(mean_enrollment = mean(`PK12 TOTAL`))

# Average NYC district size
district_data %>%
  select(`DISTRICT NAME`, `PK12 TOTAL`, `SCHOOL YEAR`) %>%
  filter(substr(`DISTRICT NAME`, 1, 3) != 'NYC') %>%
  summarize(mean = mean(`PK12 TOTAL`))
  

smaller_districts <- sizes %>%
  arrange(`PK12 TOTAL`) %>%
  head(n = 50)

View(smaller_districts)

sizes %>%
  filter(`SCHOOL YEAR` == '2018-19') %>%
  arrange(`PK12 TOTAL`)

bigger_districts <- sizes %>%
  arrange(-`PK12 TOTAL`) %>%
  head(n = 50)

wider_big_districts <- bigger_districts %>%
  spread(`SCHOOL YEAR`, `PK12 TOTAL`)

wider_small_districts <- smaller_districts %>%
  spread(`SCHOOL YEAR`, `PK12 TOTAL`)

View(wider_small_districts)

write.csv(wider_small_districts, "~/Desktop/small_districts.csv", row.names = FALSE)

# Test district sizes 
sizes %>% 
  filter(`DISTRICT NAME` == 'RIPLEY')

# Construct graphs
smaller_districts %>%
  ggplot(aes(x = `SCHOOL YEAR`, y = `PK12 TOTAL`)) +
  geom_point(aes(color = `DISTRICT NAME`)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Enrollment for smaller districts") +
  xlab("School Year") +
  ylab("Enrollment (PK-12)") +
  labs(color = "District Name")

bigger_districts %>%
  ggplot(aes(x = `SCHOOL YEAR`, y = `PK12 TOTAL`)) +
  geom_point(aes(color = `DISTRICT NAME`)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Enrollment for larger districts") +
  xlab("School Year") +
  ylab("Enrollment (PK-12)") +
  labs(color = "District Name")