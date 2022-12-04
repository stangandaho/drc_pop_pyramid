# load packages
library(tidyverse)
library(ggimage)
library(ggtext)
library(showtext)

####### DATA WRANGLING ##########
# import data downloaded from https://www.portal.worldpop.org/demographics/data/global_agesex_proportions_totals_2020.zip
drc_age_sex <- read.csv("COD_agesex_proportions_totals_2020.csv")
# data wrangling by sex
male <- drc_age_sex %>% 
  select(M_1_2020:M_80_2020, total_pop) %>% 
  mutate(across(.cols = M_1_2020:M_80_2020, ~ .x*total_pop/100)) %>% 
  select(-total_pop) %>%
  pivot_longer(cols = everything(), names_to = "Age class", values_to = "Male count") %>% 
  group_by(`Age class`) %>% 
  summarise(`Male count` = sum(`Male count`)) %>% ungroup()
# substitute some class patern
male$`Age class`[1] <- "M_01_2020"; male$`Age class`[8] <- "M_04_2020";
male$`Age class`[11] <- "M_05_2020"
## repeat the process for female gender

female <- drc_age_sex %>%
  select(F_1_2020:F_80_2020, total_pop) %>% 
  mutate(across(.cols = F_1_2020:F_80_2020, ~.x*total_pop/100)) %>% 
  select(-total_pop) %>% 
  pivot_longer(cols = everything(), names_to = "Age class", values_to = "Female count") %>% 
  group_by(`Age class`) %>% 
  summarise(`Female count` = sum(`Female count`)) %>% ungroup()
female$`Age class`[1] <- "F_01_2020"; female$`Age class`[8] <- "F_04_2020";
female$`Age class`[11] <- "F_05_2020"

## combine the datasets into a data frame and compute age group proportion

df <- cbind(male, `Female count` = female$`Female count`) %>% 
  mutate(total = `Male count` + `Female count`) %>% 
  mutate(round(across(.cols = 2:3, ~ .x*100/sum(total)), 2))
# select class part
df$`Age class` <- str_sub(df$`Age class`, start = 3L, end = 4L)
# change 80 to '80 +'
df$`Age class` <- if_else(df$`Age class` == "80",paste0(df$`Age class`, "+"),
                            paste0(df$`Age class`))
# relevel
df$`Age class` <- fct_relevel(df$`Age class`, levels = c("01", "04", "05", "10", "15", 
                                                             "20", "25", "30", "35", "40",
                                                             "45", "50", "55", "60", "65",
                                                             "70", "75", "80+"))

# font set
font_add("nbr","./Nexa/Nexa-Book.otf") # font downloaded in local
font_add("nbb","./Nexa/Nexa-bold.otf") # font downloaded in local
font_add("agfb", "AGENCYR.TTF") # windows font
font_add("onyx", "ONYX.TTF") # windows font
font_add("neb", "./Nexa/Nexa-ExtraBold.otf")
showtext_opts(dpi = 300) # dot per inch setting
showtext_auto()

################# PLOT ##############
ggplot(data = df)+
  geom_col(aes(x = `Age class`, y = `Male count`), fill = "#49083f", 
           color = "white")+
  #scale_y_continuous(expand = c(0,80000))+
  #new_scale_fill()+
  geom_col(aes(x = `Age class`, y = -`Female count`), fill = "#fd890b", 
           color = "white")+
  geom_label(aes(x = `Age class`, y = `Female count`-`Female count`,
                              label = `Age class`),
             fill = "white", color = "white", 
             size = if_else(df$`Age class` == "80+", 3, if_else(df$`Age class` == "75", 4.5, 6)),
             label.padding = unit(0.12, "lines"),
             label.size = 1.2)+
  geom_image(data = data.frame(x = "50", ym = mean(df$`Male count`), imm =  "man.png"), 
             aes(x = x, y = ym, image = imm), size = 0.10)+
  geom_image(data = data.frame(x = "50", yf = mean(df$`Female count`),imf = "woman.png"), 
             aes(x = x, y = -yf, image = imf), size = 0.10)+
  labs(y = "", x = "Proportion of individuals (%) by age group (1-80+ years)", 
       title = "<p><span style='color:#17007e'>Democratic Republic of Congo</span> - Population pyramid (2020) </p>", 
       caption = paste0("<p>Graphic: Stanislas Mahussi GANDAHO | <img src='./social/instagram.png' width='16' height='16'/> <img src='./social/twitter.png' width='16' height='16'/> <img src='./social/linkedin.png' width='16' height='16'/> <span style='font-family:agfb; font-size:22px'>  @stangandaho</span> <br> Data source: www.worldpop.org</p><br>")
       )+
  geom_text(data = df, aes(x = `Age class`, y = `Female count`-`Female count`,
                               label = `Age class`),color = "#090909", 
            size = if_else(df$`Age class` == "80+", 3, if_else(df$`Age class` == "75", 4.5, 6)), family = "nbr")+
  #female count label
  geom_text(data = data.frame(x = df$`Age class`, 
                              y = -rep(max(df$`Female count`)+1, nrow(df)),
                              label = df$`Female count`),
            aes(x = x, y = y, label = label), color = "gray30", size = 6, family = "nbr", hjust = 0)+
  #male count label
  geom_text(data = data.frame(x = df$`Age class`, 
                              y = rep(max(df$`Male count`)+1, nrow(df)),
                              label = df$`Male count`),
            aes(x = x, y = y, label =label), color = "gray30", size = 6,
            family = "nbr", hjust = 1)+
  annotate(geom = "text", x = "70", y = 4, label = "Global DRC's Population:\n", family = "nbr", size = 8, color = "gray45")+
  annotate(geom = "text", x = "65", y = 5.1, label = paste("≈", glue::glue("{round(sum(df$total)/1000000, 2)}"), "M"),
           family = "neb", size = 10, color = "gray30")+
  coord_flip()+
  theme_void()+
theme(
  #background
  plot.background = element_rect(fill = "white", colour = "white"),
  panel.background = element_rect(fill = "white", colour = "white"),
  #plot.margin = margin(0,5,0,5),
  #grid
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "gray90"),
  #title and caption
  plot.title = element_markdown(colour = "gray45", hjust = 0.5, size = 30, family = "nbb"),
  plot.title.position = "plot",
  plot.caption = element_markdown(size = 14, color = "#818181", family = "nbr", hjust = 0.90),
  # axis
  axis.title.y = element_text(size = 20, family = "nbr", color = "gray30",
                              margin = margin(l = 2, r = 1, unit = "lines"), angle = 90),
  axis.line.y = element_line(linetype = 1, lineend = "square", color = "gray60", size = 2,
                             arrow = arrow(angle = 8, length = unit(0.4, "inches"),
                                           ends = "last", type = "closed"))
)

# save plot
ggsave(filename = "drc_pyramid.jpg", width = 35, height = 22, units = "cm", dpi = 300)
