library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(leaflet)
library(viridis)
library(hrbrthemes)
library(tools)
library(directlabels)
library(gghighlight)
library(ggplot2)
library(RColorBrewer)


# figures 

# Life expectancy --------------------------

life_exp_males = read_excel("data/raw/life-exp.xls", sheet = "lifeexpmales") %>%
  clean_names() %>%
  filter(area_code != is.na(area_code)) %>%
  gather(year, life_exp_males, x1991_1993:x2010_2012, -area_code, -region)


  
life_exp_females = read_excel("data/raw/life-exp.xls", sheet = "lifeexpfemales") %>%
  clean_names() %>%
  filter(area_code != is.na(area_code)) %>%
  gather(year, life_exp_females, x1991_1993:x2010_2012, -area_code, -region)


life_exp = full_join(life_exp_males, life_exp_females, by = c("area_code", "region", "year"))

life_exp_long = life_exp %>% 
     mutate(year_date = str_sub(year, start = -10, end = -6),
           year_date = str_sub(year_date, start = 2, end = 5)) %>%
     select(-year) %>%
     rename(year = year_date) %>%
     gather(gender, life_exp, life_exp_males:life_exp_females) %>%
     mutate(gender = gsub("life_exp_", "", gender), 
                    gender = toTitleCase(gender)) %>%
     arrange(region, year) %>%
     ggplot(., aes(year, life_exp, group = region, linetype=region, color=region)) + 
     geom_line() +
     scale_x_discrete(breaks=c("1991","1995","2000", "2005", "2010")) +
     facet_wrap(~ gender, nrow = 1) + 
     theme_minimal() + 
     labs(y = "", x = "",  legend = "") +
     theme(legend.title=element_blank(),
           legend.text=element_text(size=10),
           legend.position = "bottom", 
           legend.spacing.x = unit(0.25, 'cm'), 
           strip.text.x = element_text(size = 12))
  
 

 ggsave("./output/sep-18/figures/life_exp.png", life_exp_long) # life expectancy
  
# Ageing ----------------------------------

england = sf::read_sf("./data/geography/england_lad_2017/Local_Authority_Districts_December_2017_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp") %>% 
  filter(str_detect(lad17cd, "^E0")) 


ageing = read_excel("data/raw/ons_uk_ageing.xls", sheet = "Old Age Dependency Ratio" ) %>%
  clean_names() %>%
  filter(str_detect(area_code, "^E0")) %>% 
  gather(year, dependency, x1996:x2036, -area_code, -area_name) %>%
  mutate(year = gsub("x", "", year)) %>%
  arrange(area_code, year) %>%
  mutate(quintile_dep = ntile(dependency, 7), 
         lab_dep = as.factor(ordered(case_when(quintile_dep == 1 ~ "[81 - 218)", 
                             quintile_dep == 2 ~ "[218 - 250)", 
                             quintile_dep == 3 ~ "[250 - 283)",
                             quintile_dep == 4 ~ "[283 - 329)",
                             quintile_dep == 5 ~ "[329 - 392)", 
                             quintile_dep == 6 ~ "[392 - 478)", 
                             quintile_dep == 7 ~ "[478 - 928)")))) 
  









england_ext = full_join(england, ageing, by = c("lad17cd" = "area_code", "lad17nm" ="area_name") )



# Old age dependency ratio (1996 - 2036)


plot = england_ext %>%
  mutate(lab_dep = gsub("[[:alpha:]]", "", lab_dep)) %>%
  ggplot(., aes(x = long, y = lat, fill = as.factor(quintile_dep))) + 
  geom_sf(colour = alpha("grey", 1 /3), size = 0.2) +
  coord_sf( datum = NA) +
  scale_fill_viridis(option = "viridis",
                     labels = c("[81 - 218)", "[218 - 250)", "[250 - 283)", "[283 - 329)", "[329 - 392)", "[392 - 478)","[478 - 928)"),
                     alpha = 0.85, 
                     discrete = T, 
                     direction = -1) +
  facet_wrap(~ year, ncol = 3) +
  theme(axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,axis.line=element_blank()
        ,panel.grid = element_blank()
        ,legend.title = element_blank()
        ,legend.text = element_text(size = 10)
        ,legend.key.width = unit(0.35,"cm")
        ,legend.key.height = unit(0.35,"cm")
        ,plot.title = element_text(size= 6)
        ,legend.position = "bottom"
        ,plot.caption = element_text()
        ,legend.background = element_blank()
        ,panel.background = element_blank()
        ,legend.spacing.x = unit(0.25, 'cm'))


ggsave("./output/sep-18/figures/old-age-dependency-ratio.png", plot)



# Age population in the UK 
age_pop = read_excel("data/raw/ons_pop_age_prop.xls", sheet = 2) %>% clean_names()


# stacked bar plots
 p = age_pop %>% 
  gather(age, share, x0_to_15_years_percent:uk_population, -x_1) %>%
  arrange(x_1) %>%
  filter(age != "uk_population") %>%
  mutate(age = gsub("_years_percent", "", age), 
         age = gsub("aged_", "", age), 
         age = gsub("x", "", age), 
         age = gsub("_", " ", age),
         age = gsub("percent", " ", age)) %>%
  ggplot(aes(x_1, share, fill = age)) +
   geom_bar(stat = "identity")  +
   scale_x_continuous(breaks=c(1975, 1985, 1995, 2005, 2015, 2025, 2035, 2045)) +
   labs(y= "Share population (%)", x= " ")
 
 
 
 fill <- c("#5F9EA0", "#E1B378","#8B8B8B")

 p =  p + scale_fill_manual(values=fill) + 
    theme(,axis.line=element_blank()
          ,panel.grid = element_blank()
          ,legend.title = element_blank()
          ,legend.position = "right"
          ,plot.caption = element_text()
          ,legend.background = element_blank()
          ,panel.background = element_blank()
          ,legend.spacing.x = unit(0.25, 'cm')
          ,legend.text=element_text(size=10)) # age distribution in the UK
  

 ggsave("./output/sep-18/figures/age-population-uk.png", p)

# OECD population ----------------------

oecd = read_excel("data/raw/oecd_demographics.xlsx", sheet = 2) %>%
  gather(year, pop_65, `2000`: `2017`, -X__1) %>%
  rename(country = X__1) %>%
  arrange(country, year)

 
  
old_oecd = oecd %>%
  filter(country %in% c("Germany", "France", "United Kingdom", "United States", "Canada", "Japan", "Italy", "Netherlands", "Spain")) %>%
gghighlight_line(., aes(year, pop_65, colour = country, group= country), predicate = max(pop_65) > 0, label_key = " ", use_direct_label = FALSE) +
  scale_x_discrete( breaks = c(2000, 2005, 2010, 2015)) +
  #scale_y_continuous( breaks = c(10,15,20, 25)) +
   facet_wrap(~ country) +
  theme_minimal()  +
  theme(legend.position = "none") +
  labs(y= "Share population 65 over total population (%)", x= " ") 
 

ggsave("./output/sep-18/figures/oecd-share-old.png", old_oecd)


# ONS families -----------------------------


families = read_excel("data/raw/familieshouseholds2017.xls", sheet = "numberhouseholds") %>% clean_names()

families_long = families %>% 
  gather(year, houses, x1996:x2017 ) %>%
  arrange(number_of_households, year) %>%
  group_by(number_of_households) %>%
  mutate(growth = (houses - lag(houses))/lag(houses)*100) 

families_long %>%
  filter(number_of_households %in% c("One person", "Two people")) %>%
  ggplot(., aes(year, growth, colour = number_of_households, group = number_of_households)) + 
  geom_line()
