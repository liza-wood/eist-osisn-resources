library(tidyverse)
library(stringr)
library(cowplot)
pal2 <- viridis::mako(n = 9)
data_dir <- "~/Box/osa_networks/"

# Here I do an assessment of challenges, but to do that I need to go back
# early on to identify where I made challenge and expertise numeric, and I
# need to make sure I am clear on what is challenge and what is expertise

# --- 1. Read in older OG data before merging expertise ----
## I keep the NAs
survey.p <- read.csv(paste0(data_dir, "data_producers/03_transformed/combined_survey_identifiable.csv")) 
survey.c <- read.csv(paste0(data_dir, "data_companies/03_transformed/combined_survey_identifiable.csv"))
survey.r <- read.csv(paste0(data_dir, "data_academic/03_transformed/combined_survey_identifiable.csv")) 
survey.o <- read.csv(paste0(data_dir, "data_organization/03_transformed/combined_survey_identifiable.csv"))

npchall_cols <- colnames(select(survey.p, contains("npchall_")))
pchall_cols <- colnames(select(survey.p, contains("pchall_"))) 
pchall_cols <- pchall_cols[!(pchall_cols %in% c(npchall_cols, "pchall_info"))]
npchall_cols <- npchall_cols[npchall_cols != "npchall_info"]

# Making sure to keep NAs 
## Researchers ----
Rid <- survey.r$ResponseId
survey.rpf <- survey.r %>% 
  dplyr::select(pchall_key:pchall_ow) %>% 
  map_dfr(~ factor(., levels =
                     c("No expertise",
                       "Low expertise",
                       "Moderate expertise",
                       "Strong expertise"))) %>%
  mutate(ResponseId = Rid) 

survey.rpn <- survey.rpf %>% 
  map_dfr(~ as.numeric(.)) %>% 
  mutate(ResponseId = Rid)
colnames(survey.rpn)[1:(ncol(survey.rpn)-1)] <- paste0(colnames(survey.rpn)[1:(ncol(survey.rpn)-1)], "_n")

survey.rnpf <- survey.r %>% 
  dplyr::select(npchall_labor:npchall_ipr) %>% 
  map_dfr(~ factor(., levels =
                     c("No expertise",
                       "Low expertise",
                       "Moderate expertise",
                       "Strong expertise"))) %>%
  mutate(ResponseId = Rid) 

survey.rnpn <- survey.rnpf %>% 
  map_dfr(~ as.numeric(.)) %>% 
  mutate(ResponseId = Rid)
colnames(survey.rnpn)[1:(ncol(survey.rnpn)-1)] <- paste0(colnames(survey.rnpn)[1:(ncol(survey.rnpn)-1)], "_n")

survey.r <- survey.r %>% 
  select(-c(pchall_key:pchall_ow, npchall_labor:npchall_ipr)) %>% 
  left_join(survey.rpf) %>% 
  left_join(survey.rnpf) %>% 
  left_join(survey.rpn) %>% 
  left_join(survey.rnpn) 

## Organizations ----
Rid <- survey.o$ResponseId
survey.opf <- survey.o %>% 
  dplyr::select(pchall_key:pchall_ow) %>% 
  map_dfr(~ factor(., levels =
                     c("Never",
                       "Rarely",
                       "Sometimes",
                       "Often"))) %>%
  mutate(ResponseId = Rid) 

survey.opn <- survey.opf %>% 
  map_dfr(~ as.numeric(.)) %>% 
  mutate(ResponseId = Rid)
colnames(survey.opn)[1:(ncol(survey.opn)-1)] <- paste0(colnames(survey.opn)[1:(ncol(survey.opn)-1)], "_n")

survey.onpf <- survey.o %>% 
  dplyr::select(npchall_labor:npchall_ipr) %>% 
  map_dfr(~ factor(., levels =
                     c("Never",
                       "Rarely",
                       "Sometimes",
                       "Often"))) %>%
  mutate(ResponseId = Rid) 

survey.onpn <- survey.onpf %>% 
  map_dfr(~ as.numeric(.)) %>% 
  mutate(ResponseId = Rid)
colnames(survey.onpn)[1:(ncol(survey.onpn)-1)] <- paste0(colnames(survey.onpn)[1:(ncol(survey.onpn)-1)], "_n")

survey.o <- survey.o %>% 
  select(-c(pchall_key:pchall_ow, npchall_labor:npchall_ipr)) %>% 
  left_join(survey.opf) %>% 
  left_join(survey.onpf) %>% 
  left_join(survey.opn) %>% 
  left_join(survey.onpn) 

## Producers ----
Rid <- survey.p$ResponseId
survey.ppf <- survey.p %>% 
  dplyr::select(pchall_key:pchall_ow) %>% 
  map_dfr(~ factor(., levels =
                     c("Not a challenge",
                       "Somewhat of a challenge",
                       "Moderate challenge",
                       "Serious challenge"))) %>%
  mutate(ResponseId = Rid) 

survey.ppn <- survey.ppf %>% 
  map_dfr(~ as.numeric(.)) %>% 
  mutate(ResponseId = Rid)
colnames(survey.ppn)[1:(ncol(survey.ppn)-1)] <- paste0(colnames(survey.ppn)[1:(ncol(survey.ppn)-1)], "_n")

survey.pnpf <- survey.p %>% 
  dplyr::select(npchall_labor:npchall_ipr) %>% 
  map_dfr(~ factor(., levels =
                     c("Not a challenge",
                       "Somewhat of a challenge",
                       "Moderate challenge",
                       "Serious challenge"))) %>%
  mutate(ResponseId = Rid) 

survey.pnpn <- survey.pnpf %>% 
  map_dfr(~ as.numeric(.)) %>% 
  mutate(ResponseId = Rid)
colnames(survey.pnpn)[1:(ncol(survey.pnpn)-1)] <- paste0(colnames(survey.pnpn)[1:(ncol(survey.pnpn)-1)], "_n")

survey.p <- survey.p %>% 
  select(-c(pchall_key:pchall_ow, npchall_labor:npchall_ipr)) %>% 
  left_join(survey.ppf) %>% 
  left_join(survey.pnpf) %>% 
  left_join(survey.ppn) %>% 
  left_join(survey.pnpn) 

## Companies ----
Rid <- survey.c$ResponseId
survey.cpf <- survey.c %>% 
  dplyr::select(pchall_key:pchall_ow) %>% 
  map_dfr(~ factor(., levels =
                     c("Not a challenge",
                       "Somewhat of a challenge",
                       "Moderate challenge",
                       "Serious challenge"))) %>%
  mutate(ResponseId = Rid) 

survey.cpn <- survey.cpf %>% 
  map_dfr(~ as.numeric(.)) %>% 
  mutate(ResponseId = Rid)
colnames(survey.cpn)[1:(ncol(survey.cpn)-1)] <- paste0(colnames(survey.cpn)[1:(ncol(survey.cpn)-1)], "_n")

survey.cnpf <- survey.c %>% 
  dplyr::select(npchall_labor:npchall_ipr) %>% 
  map_dfr(~ factor(., levels =
                     c("Not a challenge",
                       "Somewhat of a challenge",
                       "Moderate challenge",
                       "Serious challenge"))) %>%
  mutate(ResponseId = Rid) 

survey.cnpn <- survey.cnpf %>% 
  map_dfr(~ as.numeric(.)) %>% 
  mutate(ResponseId = Rid)
colnames(survey.cnpn)[1:(ncol(survey.cnpn)-1)] <- paste0(colnames(survey.cnpn)[1:(ncol(survey.cnpn)-1)], "_n")

survey.c <- survey.c %>% 
  select(-c(pchall_key:pchall_ow, npchall_labor:npchall_ipr)) %>% 
  left_join(survey.cpf) %>% 
  left_join(survey.cnpf) %>% 
  left_join(survey.cpn) %>% 
  left_join(survey.cnpn) 


# In previous script (osisn-processes) I saved these for network regression experiment

# --- 2. Reduce the skills ----
## This is grabbing all of the columns with challenges for each
chall_check_p <- survey.p %>% select(matches('chall_.*_n'))
chall_check_p[,1:ncol(chall_check_p)] <- sapply(chall_check_p[,1:ncol(chall_check_p)],
                                                function(x){ifelse(is.na(x), 0, 1)})
sort(colSums(chall_check_p))
colnames(chall_check_p)

chall_check_c <- survey.c %>% select(matches('chall_.*_n'))
chall_check_c[,1:ncol(chall_check_c)] <- sapply(chall_check_c[,1:ncol(chall_check_c)],
                                                function(x){ifelse(is.na(x), 0, 1)})
sort(colSums(chall_check_c))
colnames(chall_check_c)

chall_check_r <- survey.r %>% select(matches('chall_.*_n'))
chall_check_r[,1:ncol(chall_check_r)] <- sapply(chall_check_r[,1:ncol(chall_check_r)],
                                                function(x){ifelse(is.na(x), 0, 1)})
sort(colSums(chall_check_r))
colnames(chall_check_r)
all <- colnames(chall_check_p)[which(colnames(chall_check_p) %in% colnames(chall_check_r))]

chall_check_o <- survey.o %>% select(matches('chall_.*_n'))
chall_check_o[,1:ncol(chall_check_o)] <- sapply(chall_check_o[,1:ncol(chall_check_o)],
                                                function(x){ifelse(is.na(x), 0, 1)})
sort(colSums(chall_check_o))
colnames(chall_check_o)
all <- all[which(all %in% colnames(chall_check_o))]

# And there were 35 total
length(unique(c(colnames(chall_check_p), colnames(chall_check_c), colnames(chall_check_r), colnames(chall_check_o))))
# But only 29 skills that all got listed
colnames(chall_check_p)[(colnames(chall_check_p) %in% all)]
# And 5 that are not in all
colnames(chall_check_p)[!(colnames(chall_check_p) %in% all)]

rm <- c(# these were not in all surveys -- not in researchers, espcially
  # So 35 different topics drops to 29
  'npchall_contracts_n', 
  'npchall_bactivity_n', 
  'npchall_relations_n',
  'npchall_certcost_n', 'npchall_certrecords_n', 'npchall_geneticsourcing_n',
  # these irrelevant to knowledge and markets, so down to 27
  'npchall_certreq_n', 'npchall_gecontam_n', 
  # mark recommended the costs ones and business planning should also be thrown out down to 24
  'pchall_pcosts_n', 'pchall_hcosts_n', 'pchall_ccosts_n', #'npchall_bplanning_n',
  # these feel redundant or niche, so down to 20
  'pchall_cc_n', 'pchall_vern_n', 'pchall_ow_n', # 'pchall_poll_n',
  'pchall_estyield_n')


df_chall <- survey.p %>% 
  full_join(survey.c) %>% 
  select(Region, matches('chall_.*_n'),
         cropcat_veg, acres_orgseed, income20_c)
df_exp <- survey.r %>% 
  full_join(survey.o) %>% 
  select(matches('chall_.*_n'))
df_chall[df_chall == ""] <- NA
df_exp[df_exp == ""] <- NA

df_chall_l <- df_chall %>% 
  select(-matches(paste(rm, collapse = "|"))) %>% 
  rename("Identifying varieties with key traits" = "pchall_key_n",
         "Finding high quality stock seed" = "pchall_stock_n",
         "Achieving adequate seed yields" = "pchall_yield_n",
         "Sourcing seed harvest equipment" = "pchall_hequip_n",
         "Sourcing seed cleaning equipment" = "pchall_cequip_n",
         "Isolation distances" = "pchall_iso_n",
         "Soil fertility and crop nutrition" = "pchall_fert_n",
         "Irrigation and water use" = "pchall_irr_n",
         "Controlling weeds" = "pchall_weeds_n",
         "Controlling insect pests" = "pchall_pests_n",
         "Controlling disease pressure" = "pchall_disease_n",
         "Managing climatic effects" = "pchall_climate_n",
         "Managing pollinator habitats" = "pchall_poll_n"
  ) %>% 
  rename("Accessing labor" = "npchall_labor_n",
         "Accessing land" = "npchall_land_n",
         "Accessing capital" = "npchall_capital_n",
         "Farm business planning" = "npchall_bplanning_n",
         "Developing infrastructure" = "npchall_infas_n",
         "Finding/developing markets" = "npchall_markets_n",
         "Managing intellectual property rights" = "npchall_ipr_n") %>% 
  pivot_longer(cols = `Identifying varieties with key traits`:`Managing intellectual property rights`, names_to = 'chall', values_to = 'level') %>% 
  select(chall, level) %>% 
  mutate(resource = case_when(
    chall %in% c("Managing intellectual property rights",
                 "Finding/developing markets",
                 "Managing seed companies contracts",
                 "Farm business planning") ~ "Markets",
    chall %in% c("Developing infrastructure",
                 "Accessing capital",
                 "Accessing land", "Accessing labor",
                 "Sourcing seed cleaning equipment",
                 "Sourcing seed harvest equipment",
                 "Finding high quality stock seed") ~ "Capital",
    T ~ 'Knowledge'
  ))

df_exp_l <- df_exp %>% 
  select(-matches(paste(rm, collapse = "|"))) %>% 
  rename("Identifying varieties with key traits" = "pchall_key_n",
         "Finding high quality stock seed" = "pchall_stock_n",
         "Achieving adequate seed yields" = "pchall_yield_n",
         "Sourcing seed harvest equipment" = "pchall_hequip_n",
         "Sourcing seed cleaning equipment" = "pchall_cequip_n",
         "Isolation distances" = "pchall_iso_n",
         "Soil fertility and crop nutrition" = "pchall_fert_n",
         "Irrigation and water use" = "pchall_irr_n",
         "Controlling weeds" = "pchall_weeds_n",
         "Controlling insect pests" = "pchall_pests_n",
         "Controlling disease pressure" = "pchall_disease_n",
         "Managing climatic effects" = "pchall_climate_n",
         "Managing pollinator habitats" = "pchall_poll_n"
  ) %>% 
  rename("Accessing labor" = "npchall_labor_n",
         "Accessing land" = "npchall_land_n",
         "Accessing capital" = "npchall_capital_n",
         "Farm business planning" = "npchall_bplanning_n",
         "Developing infrastructure" = "npchall_infas_n",
         "Finding/developing markets" = "npchall_markets_n",
         "Managing intellectual property rights" = "npchall_ipr_n") %>% 
  pivot_longer(cols = 1:ncol(.), names_to = 'chall', values_to = 'level') %>% 
  select(chall, level) %>% 
  mutate(resource = case_when(
    chall %in% c("Managing intellectual property rights",
                 "Finding/developing markets",
                 "Farm business planning") ~ "Markets",
    chall %in% c("Developing infrastructure",
                 "Accessing capital",
                 "Accessing land", "Accessing labor",
                 "Sourcing seed cleaning equipment",
                 "Sourcing seed harvest equipment",
                 "Finding high quality stock seed") ~ "Capital",
    T ~ 'Knowledge'
  ))


# 3. Visualize ----
df_exp_l$type = 'Expertise'
df_chall_l$type = 'Challenge'
df_stacked <- rbind(df_exp_l, df_chall_l)

## By knowledge, capital, and market each ----
df_k_l <- filter(df_stacked, resource == 'Knowledge')
df_c_l <- filter(df_stacked, resource == 'Capital')
df_m_l <- filter(df_stacked, resource == 'Markets')

# Average challenge and expertise scores across challenge groups
# Low challenges and moderate expertise, across all resource groups
df_stacked %>% 
  group_by(type) %>% 
  summarize(mean(level, na.rm = T))
aggr_avg <- df_stacked %>% 
  group_by(resource, type) %>% 
  summarize(mean(level, na.rm = T))

alpha = 0.05
kchallorder_avg <- df_k_l %>% 
  filter(type == "Challenge") %>% 
  group_by(chall) %>% 
  summarize(avg = mean(level, na.rm = T),
            sd = sd(level, na.rm = T),
            n = n(),
            se = sd/sqrt(n),
            tscore = qt(p=alpha/2, df=n - 1,lower.tail=F),
            margin.error = tscore * se,
            #uci = avg + margin.error,
            #lci = avg - margin.error
            uci = avg + sd,
            lci = avg - sd)  %>% 
  arrange(-avg) %>% 
  mutate(type = 'Challenge')

kexporder_avg <- df_k_l %>% 
  filter(type == "Expertise") %>% 
  group_by(chall) %>% 
  summarize(avg = mean(level, na.rm = T),
            sd = sd(level, na.rm = T),
            n = n(),
            se = sd/sqrt(n),
            tscore = qt(p=alpha/2, df=n - 1,lower.tail=F),
            margin.error = tscore * se,
            #uci = avg + margin.error,
            #lci = avg - margin.error
            uci = avg + sd,
            lci = avg - sd)  %>% 
  arrange(-avg) %>% 
  mutate(type = 'Expertise')

kavg <- rbind(kchallorder_avg, kexporder_avg)

cchallorder_avg <- df_c_l %>% 
  filter(type == "Challenge") %>% 
  group_by(chall) %>% 
  summarize(avg = mean(level, na.rm = T),
            sd = sd(level, na.rm = T),
            n = n(),
            se = sd/sqrt(n),
            tscore = qt(p=alpha/2, df=n - 1,lower.tail=F),
            margin.error = tscore * se,
            #uci = avg + margin.error,
            #lci = avg - margin.error
            uci = avg + sd,
            lci = avg - sd)  %>% 
  arrange(-avg) %>% 
  mutate(type = 'Challenge')

cexporder_avg <- df_c_l %>% 
  filter(type == "Expertise") %>% 
  group_by(chall) %>% 
  summarize(avg = mean(level, na.rm = T),
            sd = sd(level, na.rm = T),
            n = n(),
            se = sd/sqrt(n),
            tscore = qt(p=alpha/2, df=n - 1,lower.tail=F),
            margin.error = tscore * se,
            #uci = avg + margin.error,
            #lci = avg - margin.error
            uci = avg + sd,
            lci = avg - sd)  %>% 
  arrange(-avg) %>% 
  mutate(type = 'Expertise')

cavg <- rbind(cchallorder_avg, cexporder_avg)

mchallorder_avg <- df_m_l %>% 
  filter(type == "Challenge") %>% 
  group_by(chall) %>% 
  summarize(avg = mean(level, na.rm = T),
            sd = sd(level, na.rm = T),
            n = n(),
            se = sd/sqrt(n),
            tscore = qt(p=alpha/2, df=n - 1,lower.tail=F),
            margin.error = tscore * se,
            #uci = avg + margin.error,
            #lci = avg - margin.error
            uci = avg + sd,
            lci = avg - sd)  %>% 
  arrange(-avg) %>% 
  mutate(type = 'Challenge')

mexporder_avg <- df_m_l %>% 
  filter(type == "Expertise") %>% 
  group_by(chall) %>% 
  summarize(avg = mean(level, na.rm = T),
            sd = sd(level, na.rm = T),
            n = n(),
            se = sd/sqrt(n),
            tscore = qt(p=alpha/2, df=n - 1,lower.tail=F),
            margin.error = tscore * se,
            #uci = avg + margin.error,
            #lci = avg - margin.error
            uci = avg + sd,
            lci = avg - sd)  %>% 
  arrange(-avg)  %>% 
  mutate(type = 'Expertise')

mavg <- rbind(mchallorder_avg, mexporder_avg)

## Facet together ----
dfml <- df_m_l %>% 
  left_join(mavg) 
dfcl <- df_c_l %>% 
  left_join(cavg) 
dfkl <- df_k_l %>% 
  left_join(kavg) 
dfl <- rbind(dfml,dfcl,dfkl)

# I'm using 1 sd as error bars
p_combined <- dfl %>% 
  ggplot(aes(x = factor(chall, c(kchallorder_avg$chall,
                                 cchallorder_avg$chall,
                                 mchallorder_avg$chall)),
             y = avg, color = type)) +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd,
                    group = type), 
                width=.1, color = "lightgray",
                position = position_dodge(.5)) +
  geom_point(position = position_dodge(width = .5)) +
  facet_grid(~factor(resource, c("Knowledge", "Capital", "Markets")),
             scales = "free_x") +
  ggplot2::theme_minimal() +
  scale_y_continuous(
    breaks = 1:4,
    labels = c('None', 'Low',
               'Moderate', 'Strong')) +
  labs(y = "", x = "", title = "", color = "") +
  scale_color_viridis_d(begin = .4, end = .8, option = "B") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        text = element_text(family = "Times", size = 12),
        plot.title = element_text(hjust = .5, size = 12)) +
  ggh4x::force_panelsizes(cols = c(1,.7,.3)) ; p_combined

ggsave(filename = 'figures/figure3.png', width = 8, height = 4.5)


# 4. Text: Differences in challenge/expertise by resource ----
## Availability of expertise across resources ----
k_summ <- kavg %>% 
  select(chall, avg, type) %>% 
  pivot_wider(names_from = type, values_from = avg) %>% 
  mutate(Difference = Expertise - Challenge) %>% 
  mutate(avg_diff = mean(Difference))

# On average expertise related to knowledge resources score about [] higher
unique(k_summ$avg_diff) # .42
k_summ$chall[k_summ$Difference < 0]

c_summ <- cavg %>% 
  select(chall, avg, type) %>% 
  pivot_wider(names_from = type, values_from = avg) %>% 
  mutate(Difference = Expertise - Challenge) %>% 
  mutate(avg_diff = mean(Difference))

# On average expertise related to capital resources score about [] higher
unique(c_summ$avg_diff) # .28
c_summ$chall[c_summ$Difference < 0]

m_summ <- mavg %>% 
  select(chall, avg, type) %>% 
  pivot_wider(names_from = type, values_from = avg) %>% 
  mutate(Difference = Expertise - Challenge) %>% 
  mutate(avg_diff = mean(Difference))

# On average expertise related to capital resources score about [] higher
unique(m_summ$avg_diff) # .35
m_summ$chall[m_summ$Difference < 0]

## Distribution of expertise/low challenging within individuals ----

### Actors facing lowest challenges (i.e. higher resourced) ----

cap <- c('npchall_capital_n', 'npchall_land_n', 'npchall_labor_n',
          'pchall_hequip_n', 'pchall_cequip_n', 'pchall_stock_n',
          'npchall_infas_n')
mark <- c('npchall_ipr_n',  'npchall_markets_n',
        'npchall_bplanning_n')
know <- c('pchall_key_n', 'pchall_yield_n', 'pchall_iso_n', 
          'pchall_fert_n', 'pchall_irr_n', 'pchall_weeds_n', 'pchall_pests_n', 'pchall_disease_n', 'pchall_climate_n',"pchall_poll_n")

df_chall_dist <- survey.p %>% full_join(survey.c) %>% select(ResponseId,c(know, cap, mark))

# Let's look at average challenge scores across all challenge groups
df_chall_dist$mean <- rowMeans(df_chall_dist[,2:ncol(df_chall_dist)], na.rm = T)
summary(df_chall_dist$mean)
IQR(df_chall_dist$mean, na.rm = T)
table(round(df_chall_dist$mean)) # 9 people have an average of 1 (no challenges)
table(round(df_chall_dist$mean))[[1]]/sum(table(round(df_chall_dist$mean))) # which is 7% of the group
# So let's take those and analyze them individually
nochall <- which(df_chall_dist$mean < 1.5)
df_chall_dist[nochall[1],]
df_chall_dist[nochall[2],]
df_chall_dist[nochall[3],]
df_chall_dist[nochall[4],]
df_chall_dist[nochall[5],]
df_chall_dist[nochall[6],]
df_chall_dist[nochall[7],]
df_chall_dist[nochall[8],]
df_chall_dist[nochall[9],]

### Actors with highest skills (i.e. higher resourced) ----
df_exp_dist <- survey.o %>% full_join(survey.r) %>% select(ResponseId,c(c(know, cap, mark)))
df_exp_dist$mean <- rowMeans(df_exp_dist[,2:ncol(df_exp_dist)], na.rm = T)
summary(df_exp_dist$mean)
IQR(df_exp_dist$mean, na.rm = T)
table(round(df_exp_dist$mean)) # Only 3 have pure expertise
table(round(df_exp_dist$mean))[[4]]/sum(table(round(df_chall_dist$mean)))
allexp <- which(df_exp_dist$mean > 3.5)
df_exp_dist[allexp[1],]
df_exp_dist[allexp[2],]
df_exp_dist[allexp[3],]


