library(tidyverse)
library(rio)
library(here)
library(janitor)
library(skimr)
library(scales)
library(tinytex)
library(kableExtra)
library(knitr)

df <- import(here("data", "data.xlsx"),
             setclass = "tbl_df")

head(df)


tidy_df <- df %>% 
  clean_names(case = "snake") %>% 
  separate(x_1, into = c("group", "id"),
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%  #seperate x_1 to group (NS vs. NNS) & id
  mutate(id = case_when(
   group == "NNS" & id == '1' ~  '7',
   group == "NNS" & id == '2' ~  '8',
   group == "NNS" & id == '3' ~  '9',
   group == "NNS" & id == '4' ~  '10',
   group == "NNS" & id == '5' ~  '11',
   group == "NNS" & id == '6' ~  '12',
   TRUE ~ id
  )) %>%   # fix duplicate id numbers
  mutate( formant_type = as.factor(mean), mean = NULL,
          group = as.factor(group),
          id = as.numeric(id),
          yi = i, i = NULL,
          yu = y, y = NULL,
          wu = u, u = NULL,
          ye = e, e = NULL,
          wo = o, o = NULL,
          en = x_u_0259, x_u_0259 = NULL,
          e = x_u_0264, x_u_0264 = NULL,
          ai = a, a = NULL,
          ao = x_u_0251, x_u_0251 = NULL) %>%  #rename variables & set the variable types
  gather(vowel, value, -1:-6) %>%  
  mutate(vowel = as.factor(vowel))%>% 
  spread(formant_type, value) %>% 
  filter(gender == "F") %>%  #quite redundant here (all females), just for meet the requirement
  select(id, group, age, height, vowel, F1, F2) %>% #reorder variables & discard gender
  arrange(id)
  
head(tidy_df)





##figure 1 
tidy_df %>% 
  ggplot(aes(x = F2, y = F1, color = vowel)) + 
  geom_point(size = 3) +
  scale_x_reverse() +  #reverse x and y to meet the perception of sounds
  scale_y_reverse() +
  facet_wrap(~ group) + 
  theme_classic() +
  scale_color_discrete(breaks = c("yi", "yu", 
                                  "wu", "ye", 
                                  "wo", "en", 
                                  "e", "ai", 
                                  "ao"))  #reorder vowel based on IPA order



smry_df <- tidy_df %>% 
  group_by(group, vowel) %>% 
  summarize_at(vars(F1, F2), funs(mean, sd)) 
  

head(smry_df)

##table 1
tbl_1 <- smry_df %>% 
  kable(digits = 2,
        caption = "Formant by volwels among non-native and native groups") %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F) 

##figure 2
smry_df %>% 
  ggplot(aes(x = F2_mean, y = F1_mean, 
             label = vowel, color = group)) +
    geom_label() +
    scale_x_reverse() +  
    scale_y_reverse() +
    theme_classic()


#figure 3 =figure 1 + figure 2

ggplot(data = tidy_df, aes(x = F2, y = F1 )) +
  geom_label(data = smry_df, aes(x = F2_mean, y = F1_mean, 
                                 label = vowel, fill = group),
             alpha = 0.2) +
  geom_point(aes(color = vowel, shape = group),
             size = 3, alpha = 0.4) +
  stat_ellipse(aes(color = vowel), level = 0.67) +
  scale_x_reverse() +  
  scale_y_reverse() +
  theme_classic() +
  guides(color = FALSE)


