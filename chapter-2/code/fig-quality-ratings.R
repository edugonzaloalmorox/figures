############
# Figure quality ratings - see the proportion of care homes corresponding to each rating 
# Oct - 2018
# @EduGonzalo
############


library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(lubridate)

# select overall care homes 
care_homes = read_excel("chapter-2/data/raw/01_October_2018_Latest_ratings.xlsx", 
                                   sheet = "Locations") %>% 
  clean_names() %>% 
  filter(care_home == "Y") %>%
  filter(key_question == "Overall") %>%
  mutate(postcode = gsub("[[:blank:]]", "", location_post_code))



# add information from the cpostcode

geo = read_csv("data/geography/ONSPD_MAY_2018_UK/Data/ONSPD_MAY_2018_UK.csv")
  
  
geo_care =  geo %>%
  mutate(postcode = gsub("[[:blank:]]", "", pcd)) %>%
  filter(postcode %in% unique(care_homes$postcode)) %>%
  select(postcode, oslaua, lsoa11, msoa11, streg, imd)

care_homes_ext = left_join(care_homes, geo_care, by = "postcode")


# obtain relative frequencies

care_homes_counts = care_homes_ext %>%
  #mutate(rating_recoded = ifelse(latest_rating %in% c("Outstanding", "Good"), "good", "bad")) %>%
  group_by(latest_rating) %>%
  tally() %>%
  ungroup() %>%
  #arrange(oslaua) %>%
  #group_by(oslaua) %>%
  mutate(sum_insp = sum(n), 
         freq = n/sum_insp) %>% ungroup() 

  
care_homes_counts %>%
  summarise(mean(sum_insp), 
            min(sum_insp)) 


oslauas = rep(unique(care_homes_ext$oslaua), 2)

qualities = rep(c("good", "bad"), each = length(unique(care_homes_ext$oslaua)))

df = data.frame(oslauas, qualities) %>% 
  arrange(oslauas)




df_test = left_join(df, care_homes_counts, by = c("oslauas" = "oslaua", "qualities" = "rating_recoded")) %>%
  unique() %>%
  mutate(freq = ifelse(is.na(freq), 0, freq))


df_test = df_test %>%
  group_by(qualities) %>%
  mutate(quint = ntile(freq, 5))




# geo information 

england = sf::read_sf("./data/geography/england_lad_2017/Local_Authority_Districts_December_2017_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp") %>% 
  filter(str_detect(lad17cd, "^E0")) 
  

# link data
england_ext = full_join(england, df_test, by = c("lad17cd" = "oslauas") )


england_ext = england_ext %>%
  mutate(quality_clean = ifelse(qualities == "good", "Outstanding and Good ratings", "Requires improvement and Inadequate ratings"))



england_ext %>%
  filter(qualities != is.na(qualities)) %>%
  #mutate(lab_dep = gsub("[[:alpha:]]", "", lab_dep)) %>%
  ggplot(., aes(x = long, y = lat, fill = as.factor(quint))) + 
  geom_sf(colour = alpha("grey", 1 /3), size = 0.2) +
  coord_sf( datum = NA) +
  scale_fill_viridis(option = "cividis",
                     labels = c("Top 20% of LAs", "Upper 20-40% LAs", "Middle 20% of LAs", "Lower 20-40% LAs", "Bottom 20% of LAs"),
                     alpha = 0.85, 
                     discrete = T, 
                     direction = -1) +
  facet_wrap(~ quality_clean, ncol = 4, labeller = labeller(labs_quality)) +
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
        ,legend.spacing.x = unit(0.25, 'cm')) + 
  labs(caption = "Source: CQC Care Directory (October 2018) | @EdudinGonzalo")
  



