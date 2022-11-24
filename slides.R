## ----setup-----------------------------------------------------
library(knitr)
options(htmltools.dir.version = FALSE, tibble.width = 45)
opts_chunk$set(
  echo = TRUE, 
  warning = FALSE, 
  message = FALSE, 
  error=FALSE, 
  comment = "#>",
  fig.align = 'center', 
  fig.width = 12, 
  fig.height = 11, 
  fig.show = 'hold', 
  fig.retina = 5,
  cache = FALSE
)

# libraries
library(tidyverse)
library(ggthemes)

# for fonts
library(showtext)
font_add_google("Lato", "Lato")
showtext_auto()
theme_set(theme_minimal(base_family = "Lato") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_rect(
          colour = "black", fill = NA)))


## ----read------------------------------------------------------
library(tidyverse)
# tb <- read_csv("https://extranet.who.int/tme/generateCSV.asp?ds=notifications")
tb <- read_csv("data/TB_notifications_2022-08-31.csv") %>%
  dplyr::select(country, iso3, year, contains("new_sp_"))



## ----glimpse---------------------------------------------------
glimpse(tb)


## ----tb-wrangle------------------------------------------------
tb_tidy <- tb %>% 
  dplyr::select(country, iso3, year, 
         new_sp_m04:new_sp_fu) %>%
  pivot_longer(cols=contains("new_sp"),
               names_to="stuff", 
               values_to="count") %>%
  separate(stuff, c("stuff1", 
                    "stuff2",
                    "sexage")) %>%
  dplyr::select(-stuff1, -stuff2) %>%
  mutate(sex=substr(sexage, 1, 1), 
         age=substr(sexage, 2, length(sexage))) %>%
  dplyr::select(-sexage) %>%
  select(country, iso3, year, sex, age, count) %>%
  dplyr::filter(country == "Australia")
glimpse(tb_tidy)


## ----tb-more-clean---------------------------------------------
tb_tidy <- tb_tidy %>% 
  dplyr::filter(!(age %in% c("04", "014", "514", "u"))) %>%
  dplyr::filter(year > 1996, year < 2013) %>%
  mutate(
    age_group = factor(age, labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  )


## ----tb-trend, echo=TRUE---------------------------------------
p <- tb_tidy %>% 
  group_by(year) %>%
  summarise(count = sum(count)) %>%
  ggplot(aes(x=year, y=count)) +
    geom_col()


## ----tb-lineup-code, fig.width=8, fig.height=6, out.width="100%", message=TRUE----
library(nullabor)
tb_tidy_yr <- tb_tidy %>% 
  group_by(year) %>%
  summarise(count = sum(count)) 
tb_lineup_1 <- ggplot(
  lineup(null_permute("count"), 
    tb_tidy_yr, n=12), 
       aes(x=year, y=count)) +
    geom_col() +
  facet_wrap(~.sample) +
  theme(strip.text=element_text(size=24, colour = "#C2185B"),
        axis.text = element_blank(),
        axis.title = element_blank())


## ----tb-lineup-plot, fig.width=8, fig.height=6, out.width="100%", echo=FALSE----
tb_lineup_1


## ----echo=FALSE------------------------------------------------
options(digits=3)


## ----pvisual1, eval=TRUE---------------------------------------
pvisual(x=10, K=40, m=12)


## ----tb-yr-sex-trend1, echo=TRUE-------------------------------
tb_yr_sex2 <- tb_tidy %>% 
  group_by(year, sex) %>%
  summarise(count = sum(count)) %>%
  ggplot(aes(x=year, y=count, colour=sex)) +
    # geom_point() +
    geom_smooth(se=FALSE) +
    scale_colour_brewer("", palette="Dark2") 


## ----tb-yr-sex-trend2, echo=TRUE-------------------------------
tb_yr_sex1 <- tb_tidy %>% 
  group_by(year, sex) %>%
  summarise(count = sum(count)) %>%
  ggplot(aes(x=year, y=count, fill=sex)) +
    geom_col(position="fill") +
    scale_fill_brewer("", palette="Dark2")


## ----eval=FALSE------------------------------------------------
#> tb_yr_sex_null <- tb_tidy %>%
#>   group_by(year, sex) %>%
#>   summarise(count = sum(count)) %>%
#>   ungroup() %>%
#>   group_by(sex) %>%
#>   mutate(year = sample(year))


## ----eval=FALSE------------------------------------------------
#> tb_yr_sex_data <- tb_tidy %>%
#>   group_by(year, sex) %>%
#>   summarise(count = sum(count))


## ----tb-lineup2, echo=FALSE, fig.width=8, fig.height=6, out.width="100%"----
set.seed(2441)
tb_yr_sex_l <- NULL
pos <- sample(1:12)
for (i in 1:11) {
  tb_yr_sex_null <- tb_tidy %>% 
    group_by(year, sex) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    group_by(sex) %>%
    mutate(year = sample(year), .sample = pos[i])
  tb_yr_sex_l <- bind_rows(tb_yr_sex_l, 
                                tb_yr_sex_null)
}
tb_yr_sex_data <- tb_tidy %>% 
  group_by(year, sex) %>%
  summarise(count = sum(count)) %>%
  mutate(.sample = pos[12])
tb_yr_sex_l <- bind_rows(tb_yr_sex_l,
                              tb_yr_sex_data)
tb_yr_sex_l %>%
  ggplot(aes(x=year, y=count, colour=sex)) +
    # geom_point() +
    geom_smooth(se=FALSE) +
    scale_colour_brewer("", palette="Dark2")  +
    facet_wrap(~.sample, ncol=4) +
    theme(legend.position = "none",  
          strip.text=element_text(size=24, 
                                  colour = "#C2185B"),
        axis.text = element_blank(),
        axis.title = element_blank())



## ----tb-yr-sex-trend3, echo=TRUE-------------------------------
tb_yr_sex1 <- tb_tidy %>% 
  group_by(year, sex) %>%
  summarise(count = sum(count)) %>%
  ggplot(aes(x=year, y=count, fill=sex)) +
    geom_col(position="fill") +
    scale_fill_brewer("", palette="Dark2")


## --------------------------------------------------------------
prop_f <- tb_tidy %>% 
  group_by(year, sex) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(names_from = "sex", values_from = count) %>%
  mutate(p = f/(f+m)) %>%
  ungroup() %>%
  summarise(p = mean(p)) %>%
  pull()
prop_f


## ----eval=FALSE------------------------------------------------
#> tb_yr_sex_null <- tb_tidy %>%
#>   group_by(year) %>%
#>   summarise(count = sum(count)) %>%
#>   mutate(f = 0, m = 0)
#> for (i in 1:length(tb_yr_sex_null$year)) {
#>   new_sex <- rbinom(tb_yr_sex_null$count[i], 1, prop_f)
#>   tb_yr_sex_null$f[i] <- sum(new_sex)
#>   tb_yr_sex_null$m[i] <- length(new_sex) - sum(new_sex)
#> }
#> tb_yr_sex_null <- tb_yr_sex_null %>%
#>   dplyr::select(-count) %>%
#>   pivot_longer(cols = c("f", "m"), names_to="sex", values_to="count")


## ----eval=FALSE, echo=FALSE------------------------------------
#> tb_yr_sex_data <- tb_tidy %>%
#>   group_by(year, sex) %>%
#>   summarise(count = sum(count))
#> tb_yr_psex_l <- lineup(null_dist("sex", "binom",
#>             params = list(n=1, size=200, prob=prop_f)),
#>             tb_yr_sex_data) %>%
#>     ggplot(aes(x=year, y=count, fill=sex)) +
#>     geom_col(position="fill") +
#>     scale_fill_brewer("", palette="Dark2") +
#>   facet_wrap(~.sample, ncol=4) +
#>     theme(legend.position = "none",
#>           strip.text=element_text(size=24,
#>                                   colour = "#C2185B"),
#>         axis.text = element_blank(),
#>         axis.title = element_blank())
#> 


## ----tb-lineup3, echo=FALSE, fig.width=8, fig.height=6, out.width="100%"----
set.seed(2446)
pos2 <- sample(1:12)
tb_yr_sex_l2 <- NULL
for (i in 1:11) {
  tb_yr_sex_null <- tb_tidy %>% 
    group_by(year) %>%
    summarise(count = sum(count)) %>%
    mutate(f = 0, m = 0)
  for (j in 1:length(tb_yr_sex_null$year)) {
    new_sex <- rbinom(tb_yr_sex_null$count[j], 1, prop_f)
    tb_yr_sex_null$f[j] <- sum(new_sex)
    tb_yr_sex_null$m[j] <- length(new_sex) - sum(new_sex)
  }
  tb_yr_sex_null <- tb_yr_sex_null %>%
    dplyr::select(-count) %>%
    pivot_longer(cols = c("f", "m"), 
                 names_to="sex", 
                 values_to="count") %>%
    mutate(.sample = pos2[i])
  tb_yr_sex_l2 <- bind_rows(tb_yr_sex_l2, 
                                tb_yr_sex_null)
}

tb_yr_sex_data <- tb_tidy %>% 
  group_by(year, sex) %>%
  summarise(count = sum(count)) %>%
  mutate(.sample = pos2[12])
tb_yr_sex_l2 <- bind_rows(tb_yr_sex_l2,
                              tb_yr_sex_data)
tb_yr_sex_l2 %>%
    ggplot(aes(x=year, y=count, fill=sex)) +
    geom_col(position="fill") +
    scale_fill_brewer("", palette="Dark2") +
  facet_wrap(~.sample, ncol=4) +
    theme(legend.position = "none",  
          strip.text=element_text(size=24, 
                                  colour = "#C2185B"),
        axis.text = element_blank(),
        axis.title = element_blank()) 


