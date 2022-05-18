library(tidyverse)
library(here)
library(tidyr)
library(gtools)

#> Get data
data <- read_csv(here("data.csv"))


#> 1. CREATE NEW HABITAT COLUMNS ####

#> Solution here - https://stackoverflow.com/questions/64622696/create-new-columns-based-on-comma-separated-values-in-another-column-in-r

#> Create new data frame for subsequent merging with original data
df.hab <- data |> 
  select(ID, `Habitat Type`)

#> Remove rows where habitat type = NA
df.hab <- df.hab |> 
  drop_na(`Habitat Type`)

#> Remove semi colon if it appears at the end of a string
df.hab$`Habitat Type` <- gsub(";$", "", df.hab$`Habitat Type`)

#> Separate into rows
df.hab <- df.hab |> 
  mutate(Hab = `Habitat Type`) |> 
  separate_rows(Hab, sep = ";")

#> Create a presence/absence column
df.hab <- df.hab |> 
  mutate(pa = 1)

#> Pivot wider and use the presence/absence
#column as entries; fill with 0 if absent
df <- df.hab |> 
  pivot_wider(names_from = Hab, values_from = pa, values_fill = 0)

df <- df |> 
  select(order(colnames(df)))

#> Drop 'Habitat Type' column and add prefix to all cols (except ID)
df <- df |> 
  select(-`Habitat Type`) |> 
  rename_with(~ paste0("HabType_", .), -ID) 

#> Arrange HabType columns in order
df <- df[,order(as.numeric(sub("\\D*","",colnames(df))))]

#> Move ID column to first position
df <- df |> 
  select(ID, everything())

#> Merge dataframe with original data
data <- merge(data, df, by = "ID", all.x = TRUE)

#> Remove df
rm(df)


#> 2. CREATE NEW 'PAST MANAGEMENT' COLUMNS ####

#> Create new data frame for subsequent merging with original data
df.past <- data |> 
  select(ID, `Past Management`)


#> Remove rows where past management = NA
df.past <- df.past |> 
  drop_na(`Past Management`)

#> Remove semi colon if it appears at the end of a string
df.past$`Past Management`<- gsub(";$", "", df.past$`Past Management`)

#> Separate into rows
df.past <- df.past |> 
  mutate(past = `Past Management`) |> 
  separate_rows(past, sep = ";")

#> Create a presence/absence column
df.past <- df.past |> 
  mutate(pa = 1)

#> Pivot wider and use the presence/absence
#column as entries; fill with 0 if absent
df <- df.past |> 
  pivot_wider(names_from = past, values_from = pa, values_fill = 0)

df <- df |> 
  select(order(colnames(df)))

#> Drop 'pastitat Type' column and add prefix to all cols (except ID)
df <- df |> 
  select(-`Past Management`) |> 
  rename_with(~ paste0("PastType_", .), -ID) 

#> Arrange pastType columns in order
df <- df[,order(as.numeric(sub("\\D*","",colnames(df))))]

#> Move ID column to first position
df <- df |> 
  select(ID, everything())

#> Merge dataframe with original data
data <- merge(data, df, by = "ID", all.x = TRUE)

#> Remove df
rm(df)




#> 3. CREATE NEW 'REWILDING INTERVENTIONS' COLUMNS ####

#> Create new data frame for subsequent merging with original data
df.wild <- data |> 
  select(ID, `Rewilding Interventions`)


#> Remove rows where wild management = NA
df.wild <- df.wild |> 
  drop_na(`Rewilding Interventions`)

#> Remove semi colon if it appears at the end of a string
df.wild$`Rewilding Interventions`<- gsub(";$", "", df.wild$`Rewilding Interventions`)

#> Separate into rows
df.wild <- df.wild |> 
  mutate(wild = `Rewilding Interventions`) |> 
  separate_rows(wild, sep = ";")

#> Create a presence/absence column
df.wild <- df.wild |> 
  mutate(pa = 1) 
  
#> Trim white space
df.wild$wild <- trimws(df.wild$wild)

df.wild <- unique(df.wild)

#> Pivot wider and use the presence/absence
#column as entries; fill with 0 if absent
df <- df.wild |> 
  pivot_wider(names_from = wild, values_from = pa, values_fill = 0)


#> Drop 'wild Type' column and add prefix to all cols (except ID)
df <- df |> 
  select(-`Rewilding Interventions`) |> 
  rename_with(~ paste0("WildType_", .), -ID) 

#> Arrange wildType columns in order
df <- df[,order(as.numeric(sub("\\D*","",colnames(df))))]

#> Move ID column to first position
df <- df |> 
  select(ID, everything())

#> Merge dataframe with original data
data <- merge(data, df, by = "ID", all.x = TRUE)


#> Export as CSV
write_csv(data, here("Chenie_Rewilding_Processed_R.csv"))



