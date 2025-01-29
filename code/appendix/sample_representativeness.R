library(dplyr)
library(kableExtra)
library(stringr)
# 1. Sample representativenes ----
data_dir <- "~/Box/osa_networks/data_combined/"
surveys <- readRDS(paste0(data_dir, "surveys/surveys.rds"))
ppltn <- read.csv(paste0(data_dir, "representation/ppltn.csv"))

table(surveys$survey_group, surveys$snowball)
table(ppltn$survey_group, ppltn$snowball)

unique(surveys$geo_scale)
unique(ppltn$geo_scale)
levels = data.frame(geo_scale = c("West", "North Central", "Northeast", "South", "USA", "Canada", "Other country", "International"))
order <- c("Total", "West", "North Central", "Northeast", "South", "USA", "Canada", "Other country", "International")
get_representation <- function(grp){
  n <- surveys %>% 
    filter(survey_group == grp) %>% 
    mutate(geo_scale = ifelse(geo_scale %in% c('Atlantic Regional', 'Prairie Regional',
                                               'Pacific Regional', 'Central Regional'),
                              'Canada', geo_scale)) %>% 
    mutate(geo_scale = str_remove_all(geo_scale, ' Regional| National')) %>% 
    group_by(geo_scale) %>% 
    count() %>% 
    right_join(levels) %>% 
    mutate(n = ifelse(is.na(n), 0, n))
  N <- ppltn %>% 
    filter(survey_group == grp) %>% 
    mutate(geo_scale = ifelse(geo_scale %in% c('Atlantic Regional', 'Prairie Regional',
                                               'Pacific Regional', 'Central Regional'),
                              'Canada', geo_scale)) %>% 
    mutate(geo_scale = str_remove_all(geo_scale, ' Regional| National')) %>% 
    group_by(geo_scale) %>% 
    count() %>% 
    rename(N = n) %>% 
    right_join(levels) %>% 
    mutate(N = ifelse(is.na(N), 0, N))
  df <- left_join(n, N) %>% 
    mutate(percent_response = round(100*(n/N)))
  df <- df %>% 
    rbind(data.frame(geo_scale = "Total", n = sum(as.numeric(df$n)), 
                     N = sum(as.numeric(df$N)),
                     percent_response = round(100*(sum(as.numeric(df$n))/sum(as.numeric(df$N)))))) %>% 
    arrange(factor(geo_scale, order))
  return(df)
}

rep_p <- get_representation('producer')
rep_c <- get_representation('company')
rep_u <- get_representation('academic')
rep_o <- get_representation('organization')

tablec1 <- cbind(rep_p, rep_c) %>% 
  cbind(rep_u) %>% 
  cbind(rep_o) 
tablec1 <- tablec1[,-c(5,9,13)]
colnames(tablec1) <- c("Region", "n", "N", "% response",
                       "n", "N", "% response",
                       "n", "N", "% response",
                       "n", "N", "% response")

write.csv(tablec1, 'tables/tableS1.csv', row.names = F)
