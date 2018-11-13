library(tidyverse)
library(rio)
library(here)
library(janitor)
library(skimr)
library(scales)
library(tinytex)
library(kableExtra)
library(knitr)

df <- import(here("data", "voweldata.xlsx"),
             setclass = "tbl_df")


View(df)

df %>%
  slice(x_1)

# df %>%
#   clean_names(case = "snake") %>%


  # gather(val, value, 2:11) %>%
  # View()
