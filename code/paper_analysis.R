library(statnet)
library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)
library(tidyr)
library(ggraph)
library(grid)
#library(ggplotify)
library(cowplot)
library(multigraph)
library(extrafont)

data_dir <- "~/Box/osa_networks/data_combined/"
source('code/functions.R')
mako_2col <- viridis::mako(n = 12)[c(10,6)]
mako_4col <- viridis::mako(n = 12)[c(10, 8, 6, 3)]
inferno_5col <- viridis::inferno(n = 20)[c(7,10,13,16,19)]

# ---- THEORETICAL BACKGROUND ----

# ---- Figure 1. Network typologies ----
## Manually created powerpoint, available in figures/

# ---- Table 1. Hypotheses ----
## Manually created in Word

# ---- METHODS ----
# ---- Setup ----
##
## Read in data and clean up a few features
surveys <- readRDS(paste0(data_dir, "surveys/surveys_cleaned_features.rds")) %>% 
  filter(!is.na(geography))
table(is.na(surveys$geo_scale))
ppltn <- read.csv(paste0(data_dir, "representation/ppltn.csv")) 
table(is.na(ppltn$geo_scale))
edges <- read.csv(paste0(data_dir, "nodes-edges/edges_universityindividual_nogeneric_long_cleanedscale.csv"))
nodes <- read.csv(paste0(data_dir,"nodes-edges/nodes_universityindividual_nogeneric_cleanedscale.csv")) %>% 
  filter(!duplicated(Operation))
graphs <- readRDS(paste0(data_dir,'networks/fx_gstack_noiso_undir_universityindividual_4subnetworks.rds'))
networks <- readRDS(paste0(data_dir,'networks/fx_netstack_noiso_undir_universityindividual_4subnetworks.rds'))
matrices <- readRDS(paste0(data_dir,'networks/fx_matrices_undir_universityindividual_4subnetworks.rds'))
networks_iso <- readRDS(paste0(data_dir,'networks/fx_netstack_iso_undir_universityindividual_4subnetworks.rds'))

for(i in 1:length(networks)){
  networks[[i]] %v% 'lvl' <- ifelse(networks[[i]] %v% 'scale' %in% 
                                      c("National", "International"), 2,
                                    1)
  networks[[i]] %v% 'role_refined' <- ifelse(networks[[i]] %v%
                                               'role_refined' ==
                                               "production + post-production", 
                                             'post-production',
                                             networks[[i]] %v% 'role_refined')
}

name_levels = c("Producers", "Company", "Organization", 
                "University & extension","Government")

# ---- Table 2: Actor representativeness ----
##
## Looking at population and survey sample by **actor**
## For representativeness by Region, see SM
names <- tools::toTitleCase(names(table(surveys$survey_group)))
names[1] <- "Researcher"
n <- c(table(surveys$survey_group))
N <- c(table(ppltn$survey_group))

table <- data.frame("Survey group" = names,
                    "N" = N, "n" = n,
                    "Response rate" = round(100*(n/N)))
table <- table %>% 
  rbind(c("Total", sum(as.numeric(table$N)), sum(as.numeric(table$n)),
          round(100*(sum(as.numeric(table$n))/sum(as.numeric(table$N))))))
colnames(table)[1] <- "Survey group"
colnames(table)[4] <- "Response rate (%)"

write.csv(table, 'tables/table2.csv', row.names = F)

# ---- Table 3: Operationalizing functional networks ----
##
## Defining the relationships within each functional sub-network
table <- data.frame(
  "Functions" = c("Knowledge development",
                  "Knowledge diffusion",
                  "Resource mobilization",
                  "Market formation"),
  "Types of relationships" = c("Research partnerships, stakeholder project involvement, and academic collaborations", 
                               "Information acquisition and exchange",
                               "Seed acquisition, seed exchange, research funding",
                               "Licensing agreements, contracts, rent, buy, and/or sell to/from"))
colnames(table)<- c("Functional subsystems", 
                    "Types of relationships")
write.csv(table, 'tables/table3.csv', row.names = F)

# ---- In-text: Network construction ----
##
## Out of those surveyed, we have responses from []
nrow(surveys) # 247

####

# Internal understanding: 
## [] of them answered network questions
egos_pre <- edges %>% select(from) %>% unique() 
nrow(egos_pre) # 217 egos
## Identifying [] other entities
nodes_generic <- read.csv(paste0(data_dir,"nodes-edges/nodes_universityindividual.csv")) %>% 
  filter(!is.na(Operation))
nrow(nodes_generic) - nrow(egos_pre) # 609
## through [] connections
edges_generic <- read.csv(paste0(data_dir,"nodes-edges/edges_universityindividual_generic_long.csv"))
nrow(edges_generic) # 2696
nrow(unique(select(edges_generic, from, to, edge_type_2))) # 2243

self_refs <- edges_generic %>% 
  filter(to == from)

# Refining condition:  First, [] of the connections were to generic
nrow(edges_generic)-nrow(edges)
1-(nrow(edges)/nrow(edges_generic))

####

# The whole innovation system networks includes a total of [] uniquely identifiable nodes
nrow(nodes) # 784
# Connected by []
nrow(edges) # 2140 weighted edges
nrow(unique(select(edges, from, to))) # 1440 unweighted, but this includes reciprocal ties
network.edgecount(networks[[1]]) # so really 1389 unweighted

# Check
nrow(edges)== igraph::gsize(graphs[[1]])
nrow(nodes)== network.size(networks[[1]])

# ---- RESULTS ----

# ---- Table 4: Summary statistics ----
##
## Part A: Descriptive statistics ----
table4a <- data.frame("Network" = c("Innovation system", 
                                    "Knowledge development",
                                    "Knowledge diffusion", 
                                    "Resource mobilization",
                                    "Market formation"),
                      "Connected actors" = unlist(lapply(networks, network.size)),
                      # These are unweighted counts
                      "Connections" = unlist(lapply(graphs, igraph::gsize)),
                      # unlist(lapply(networks, network.edgecount)),
                      "Average # connections" = unlist(lapply(networks, function(x)
                        round(mean(degree(x)), 2))), 
                      "Density" = unlist(lapply(networks, function(x) round(gden(x), 2))),
                      "Transitivity" = unlist(lapply(networks, function(x) round(gtrans(x), 2))),
                      "Centralization" = unlist(lapply(networks, function(x) round(centralization(x, degree),2)))
)

# Weighted ties
colnames(table4a)[c(4)] <- c("Avg. # connections")

## Part B: Actor representation ----
table4b <- data.frame("Network" = c("Innovation system", 
                                    "Knowledge development",
                                    "Knowledge diffusion", 
                                    "Resource mobilization",
                                    "Market formation"),
                      "Producers" = unlist(lapply(networks, function(x) length(which(x %v% 
                                                                                       'role_refined' == "production")))), 
                      "Company" = unlist(lapply(networks, function(x) length(which(x %v% 
                                                                                     'role_refined' == "post-production")))),
                      "Organization" = unlist(lapply(networks, function(x) length(which(x %v% 
                                                                                          'role_refined' == "org")))),
                      "University & extension" = unlist(lapply(networks, function(x) length(which(x %v% 
                                                                                                    'role_refined' == "university_ext")))),
                      "Government" = unlist(lapply(networks, function(x) length(which(x %v% 
                                                                                        'role_refined' == "govt"))))
)
colnames(table4b)[c(5)] <- c("University &\nextension")

## Combining and getting percentages
## The number of stats in Table 4a, so everything other than network
nstats <- ncol(table4a)-1
## Which rows will table b be in
tbl_b_rows <- (nstats+1):((nstats)+(ncol(table4b)-1))
table4 <- left_join(table4a, table4b) %>% 
  pivot_longer(cols = c(Connected.actors:Government), names_to = "Counts", values_to = "Count") %>% 
  pivot_wider(names_from = "Network", values_from = "Count") %>% 
  mutate(whole_percent = paste0("(", round(100*c(rep(NA, nstats), `Innovation system`[tbl_b_rows]/`Innovation system`[1])), "%)"),
         kdev_percent =  paste0("(", round(100*c(rep(NA,nstats), `Knowledge development`[tbl_b_rows]/`Knowledge development`[1])), "%)"),
         kdif_percent = paste0("(", round(100*c(rep(NA,nstats), `Knowledge diffusion`[tbl_b_rows]/`Knowledge diffusion`[1])), "%)"),
         rmob_percent = paste0("(", round(100*c(rep(NA,nstats), `Resource mobilization`[tbl_b_rows]/`Resource mobilization`[1])), "%)"),
         sc_percent = paste0("(", round(100*c(rep(NA,nstats), `Market formation`[tbl_b_rows]/`Market formation`[1])), "%)")) %>% 
  mutate(whole_percent = ifelse(whole_percent == "(NA%)", "", whole_percent),
         kdev_percent = ifelse(kdev_percent == "(NA%)", "", kdev_percent),
         kdif_percent = ifelse(kdif_percent == "(NA%)", "", kdif_percent),
         rmob_percent = ifelse(rmob_percent == "(NA%)", "", rmob_percent),
         sc_percent = ifelse(sc_percent == "(NA%)", "", sc_percent)) %>% 
  mutate(`Innovation system` = paste(`Innovation system`, whole_percent),
         `Knowledge development` = paste(`Knowledge development`, kdev_percent),
         `Knowledge diffusion` = paste(`Knowledge diffusion`, kdif_percent),
         `Resource mobilization` = paste(`Resource mobilization`, rmob_percent),
         `Market formation` = paste(`Market formation`, sc_percent)) %>% 
  select(-c(contains("percent"))) %>% 
  rename(" " = "Counts")

# Excluding network summary statistics, in the end
table4 <- table4[-c(3:6),]

write.csv(table4, 'tables/table4.csv', row.names = F)

####

# Internal exploration: Why is it that government, which accounts for the fewest nodes, is the so active? What is the average degree for each group? Government has the highest average degree, likely because of GRIN

for(i in 1:length(networks)){
  networks[[i]] %v% 'degree' <- sna::degree(networks[[i]], rescale = F)
  networks[[i]] %v% 'log_degree' <- log(networks[[i]] %v% 'degree')
}

mean((networks[[1]] %v% 'degree')[networks[[1]] %v% 'role_refined' == "production"])
mean((networks[[1]] %v% 'degree')[networks[[1]] %v% 'role_refined' == "post-production"])
mean((networks[[1]] %v% 'degree')[networks[[1]] %v% 'role_refined' == "org"])
mean((networks[[1]] %v% 'degree')[networks[[1]] %v% 'role_refined' == "university_ext"])
mean((networks[[1]] %v% 'degree')[networks[[1]] %v% 'role_refined' == "govt"])


# --- In-text: network correlations ----

adj_kdev <- as.matrix.network.adjacency(networks_iso[[2]]) 
adj_kdif <- as.matrix.network.adjacency(networks_iso[[3]])
adj_rmob <- as.matrix.network.adjacency(networks_iso[[4]])
adj_sc <- as.matrix.network.adjacency(networks_iso[[5]])

cor(c(adj_kdev), c(adj_kdif)) # .41
cor(c(adj_rmob), c(adj_sc)) # .29
cor(c(adj_kdev), c(adj_rmob)) # .18
cor(c(adj_kdif), c(adj_sc)) # .39
cor(c(adj_kdev), c(adj_sc)) # .16
cor(c(adj_kdif), c(adj_rmob)) # .31

####
# Internal: How many egos are alters?
tbl <- table(unique(edges$from) %in% unique(edges$to))
# [] Egos are alters
tbl[[2]] # 139
# and this is []% of all egos
tbl[[2]]/sum(tbl)


# --- Figure 2. Network graphs ----
##
## Cleaning up the data  ----
for(i in 1:length(networks)){
  networks[[i]] %v% 
    'role_refined_rename' <- ifelse(networks[[i]] %v% 'role_refined' ==
                                      "production", "Producers",
                              ifelse(networks[[i]] %v% 'role_refined' ==
                                             "post-production", "Company",
                              ifelse(networks[[i]] %v% 'role_refined' ==
                                                    "govt", "Government",
                              ifelse(networks[[i]] %v% 'role_refined' ==
                                                           "org", "Organization",
                              ifelse(networks[[i]] %v% 'role_refined' ==
                                                                  "university_ext", "University & extension",
                                                                "error")))))
}

name_levels = c("Company", "Producers", "Organization", 
                "University & extension","Government")

## Global settings and function ----
set.seed(22)
fixed_coord <- create_layout(networks[[1]], layout = 'fr')

## Combining plots ----
legend_font_size = 11
title_font_size = 7
legend <- cowplot::get_plot_component(subplot_viz_fixed(fixed_coord, 
                                                        networks[[1]])  +
                                        labs(color = "", size = "Degree") +
                                        guides(colour = "legend",
                                               size = "none", alpha = "none") +
                                        theme(text=element_text(family="Times",
                                                                size = legend_font_size),
                                              legend.key.size = unit(.6,"cm"),
                                              legend.position = "bottom"),
                                      'guide-box-bottom', return_all = TRUE)

p2a <- subplot_viz_fixed(fixed_coord, networks[[2]]) +
  theme(legend.position = "none") +
  labs(title = "") +
  theme(text=element_text(family="Times", size = title_font_size),
        plot.title = element_text(hjust = 0.5))
p2b <- subplot_viz_fixed(fixed_coord, networks[[3]]) +
  theme(legend.position = "none") +
  labs(title = "") +
  theme(text=element_text(family="Times", size = title_font_size),
        plot.title = element_text(hjust = 0.5))
p2c <- subplot_viz_fixed(fixed_coord, networks[[4]]) +
  theme(legend.position = "none") +
  labs(title = "") +
  theme(text=element_text(family="Times", size = title_font_size),
        plot.title = element_text(hjust = 0.5))
p2d <- subplot_viz_fixed(fixed_coord, networks[[5]]) +
  theme(legend.position = "none") +
  labs(title = "") +
  theme(text=element_text(family="Times", size = title_font_size),
        plot.title = element_text(hjust = 0.5))

ggdraw() + draw_plot(p2a, x = -.01, y = 0.05, width = .26, height = 1.) +
  draw_plot(p2b, x = .24, y = 0.05, width = .26, height = 1.) +
  draw_plot(p2c, x = .48, y = 0.05, width = .26, height = 1.) + 
  draw_plot(p2d, x = .72, y = 0.05, width = .26, height = 1.) + 
  draw_plot(legend, x = 0, y = 0, width = 1, height = .1) + 
  draw_plot_label(label = c("A. Knowledge development",
                            "B. Knowledge diffusion",
                            "C. Resource mobilization", 
                            "D. Market formation"), 
                  x = c(-0.09, 0.2, 0.43, 0.7), y = c(1, 1, 1,1),
                  family = "Times", fontface = "plain", size = 8)

ggsave('figures/figure2.jpeg', width = 6, height = 2, dpi = 600)

# ---- Figure 3 ----
## See challenges_expertise.R

# --- Model  ----
## Reading in model results directly, run in appendix, and setting re-run to F
mb <- readRDS('code/appendix/models/m0b4_egoalter_gwd01_gwe08.rds')
rerun_bergm = F
## Suggest reading in because they take several [] each to run
lapply(mb, function(x) x$Time) #5-15

if(rerun_bergm == T){
  geo_levels <- c('North Central Regional', 'Northeast Regional', 'USA National', 
                  'South Regional', 'West Regional')
  ## But the code to create is:
  # 1. Use networks with isolates, so that we are considering the entire sample
  #    which is []
  network.size(networks[[1]]) #784
  networks_m4 <- networks[2:5]
  
  # 2. Identify those who are non-respondents (alters only)
  alters_only <- nodes$Operation[!(nodes$Operation %in% edges$from)]
  
  # 3. Assign NA to the connections that are between non-respondents
  matrices_m4 <- list()
  networks_missing4 <- list()
  for(n in 1:4){
    mat_m <- as.matrix(networks_m4[[n]])
    # Put NA for non-respondents
    for(i in 1:nrow(mat_m)){
      for(j in 1:ncol(mat_m)){
        mat_m[i,j] <- ifelse(rownames(mat_m)[i] %in% alters_only &
                               colnames(mat_m)[j] %in% alters_only, NA, mat_m[i,j])
      }
    }
    matrices_m4[[n]] <- mat_m
    networks_missing4[[n]] <- network(mat_m, directed = F)
    # Here, use the initial network to keep names the same
    check <- table(networks_m4[[n]] %v% 'vertex.names' == networks_missing4[[n]] %v% 'vertex.names')
    if(length(check) > 1 | "FALSE" %in% names(check)){
      print("Stop: you need to check that your vertex attr align with network size")
    } else {
      networks_missing4[[n]] %v% 'geo_scale' <- networks_m4[[n]] %v% 'geo_scale'
      networks_missing4[[n]] %v% 'role_refined' <- networks_m4[[n]] %v% 'role_refined'
      networks_missing4[[n]] %v% 'lvl' <- networks_m4[[n]] %v% 'lvl'
      networks_missing4[[n]] %v% 'respondent' <- networks_m4[[n]] %v% 'respondent'
    }
  }
  
  mb <- list()
  for(i in 1:length(networks_missing4)){
    base_actor = which(levels(factor(networks_missing4[[i]] %v% 'role_refined')) == 'production')
    m <- Bergm::bergmM(networks_missing4[[i]] ~ edges +
                         gwdegree(.1, fixed = T) + 
                         gwesp(.8, fixed = T) +
                         nodefactor('role_refined', base = base_actor)+
                         nodematch('geo_scale', diff = T, 
                                   levels = geo_levels) +
                         nodefactor('lvl') +
                         nodefactor('respondent', base = 2),
                       burn.in     = 100,
                       main.iters  = 2000,
                       aux.iters   = 2000,
                       nchains     = 4,
                       gamma       = 0.5,
                       missingUpdate = 500,
                       seed = 21)
    mb[[i]] <- m
  }
}

## Set order of coefficients for plot
coef_order <- c("Anti-centralization", 
                "Triadic closure", 
                "Actor-type: Government", 
                "Actor-type: University & extension", 
                "Actor-type: Organization", 
                "Actor-type: Company", 
                "Actor-type: Producer", 
                "Regional homophily: Northeast", 
                "Regional homophily: South", 
                "Regional homophily: North Central", 
                "Regional homophily: West",
                "National homophily: US National", 
                "Scale: National/International (vs. Regional)",
                "Multi-functional tie",
                "Survey non-respondent", 
                "N respondents per node", 
                "N generic connections", 
                "Edges", "Fixed -Inf")

## And identify the coefficients we don't want
rm_coefs <- c("Regional homophily: Northeast", 
              "Regional homophily: South", 
              "Regional homophily: North Central", 
              "Regional homophily: West",
              "National homophily: US National", 
              "Scale: National/International (vs. Regional)",
              "Survey non-respondent", 
              "N respondents per node", 
              "Edges")

# Figure 4 ----
## Convert model results into a data frame (see functions.R for custom function)
fx_models <- c("Knowledge development", "Knowledge diffusion", 
               "Resource mobilization", "Market formation")

full_fx_models_df <- prep_models(mb, fx_models, 4)
manual_coef_plot(full_fx_models_df, mako_4col)
ggsave('figures/figure4.png', width = 5, height = 3.7, dpi = 500)

results_table <- full_fx_models_df %>% 
  mutate(Estimate = round(Estimate, 2),
         CI = paste(round(LCI, 2), round(UCI, 2), sep = ",")) %>% 
  select(model, term, Estimate, CI) %>% 
  tidyr::pivot_wider(names_from = model,
                     values_from = Estimate:CI)
tbl_order <- c(sapply(fx_models, function(x) which(str_detect(colnames(results_table), x))))
results_table <- results_table[,c(1,tbl_order)]
# Give then an asterisk if their credible intervals do not cross zero
assign_asterisk <- function(x, y){
  CI <- data.frame(do.call('rbind', str_split(y, ',')))
  ifelse(CI$X1 > 0 & CI$X2 > 0 | CI$X1 < 0 & CI$X2 < 0, paste0(x, "*"), x)
}
results_table$`Estimate_Knowledge development` <- assign_asterisk(results_table$`Estimate_Knowledge development`, results_table$`CI_Knowledge development`)
results_table$`Estimate_Knowledge diffusion` <- assign_asterisk(results_table$`Estimate_Knowledge diffusion`, results_table$`CI_Knowledge diffusion`)
results_table$`Estimate_Resource mobilization` <- assign_asterisk(results_table$`Estimate_Resource mobilization`, results_table$`CI_Resource mobilization`)
results_table$`Estimate_Market formation` <- assign_asterisk(results_table$`Estimate_Market formation`, results_table$`CI_Market formation`)
write.csv(results_table, 'tables/tableS3.csv', row.names = F)


# In-text: RESULTS ----
## Levy's functions for gwdegree ----
## https://github.com/michaellevy/ViticultureNetworks-REC/blob/master/ViticultureNetworks.Rmd
deltaGWD = function(k, theta_s) {
  (1 - exp(-theta_s))^k
}

textVals = function(vals) {
  vals = round(vals, 1)
  paste0("Knowledge development: ", vals[1], " %, Knowledge diffusion: ", vals[2], 
         " %, Resource mobilization: ", vals[3], " %, Market formation: ", vals[4], " %")
}

GWDOddsChange = function(k, theta, theta_s = .1) {
  # k is the lower-degree node's degree. I.e. calculate change in odds for k+1 vs k
  # theta is vector of gwd estimates (across networks)
  relOdds = 
    deltaGWD(c(k, k + 1), theta_s) %>%
    diff() %>%  # Diff is second minus first -- want difference between k + 1 and k
    `*`(theta) %>%
    exp()
  textVals((relOdds - 1) * 100)
}

ergms <- mb
if(class(ergms[[1]]) == "bergm"){
  # Need to re-write for BERGM output, which is finicky-er. Recreate the CIs from 
  # posterior distribution
  coefs <- list()
  for(i in 1:length(ergms)){
    post_dist <- data.frame(ergms[[i]]$Theta)
    ci_est <- apply(post_dist, 2, bayestestR::ci)
    df <- data.frame('term' = ergms[[i]]$specs,
                     'estimate' = apply(post_dist, 2, mean),
                     'LCI' = unlist(lapply(ci_est, function(x) x[[2]])),
                     'UCI' = unlist(lapply(ci_est, function(x) x[[3]])),
                     "p_val" = NA)
    coefs[[i]] <- df
  }
} else{
  coefs = lapply(ergms, broom::tidy)
}

gwds = sapply(coefs, function(m) m$estimate[grep("gwd", m$term)])
gwesps = sapply(coefs, function(m) m$estimate[grep("gwesp", m$term)])

# Change statistics based on k (number of ties, degree parameter, decay)
GWDOddsChange(1, gwds) # between ...
GWDOddsChange(2, gwds) # between ...
GWDOddsChange(3, gwds) # between ...

# Generic understanding of coefficient
textVals((exp(gwesps) - 1) * 100)

rev(unique(full_fx_models_df$model))

## Actor activity 
govts = select(filter(full_fx_models_df, term == "Actor-type: Government"),
               Estimate)
orgs = select(filter(full_fx_models_df, term == "Actor-type: Organization"),
              Estimate)
unis = select(filter(full_fx_models_df, term == "Actor-type: University & extension"),
              Estimate)

co = select(filter(full_fx_models_df, term == "Actor-type: Company"),
            Estimate)
# Reversing just to present in the order I am used to
rev(unique(full_fx_models_df$model))
rev((exp(govts$Estimate) - 1) * 100)
rev((exp(unis$Estimate) - 1) * 100)
rev((exp(orgs$Estimate) - 1) * 100)
rev((exp(co$Estimate) - 1) * 100)

# Functional differentiation
## Knowledge development
rev((exp(govts$Estimate) - 1) * 100)[1]
rev((exp(unis$Estimate) - 1) * 100)[1]
rev((exp(orgs$Estimate) - 1) * 100)[1]

## Knowledge diffusion
rev((exp(govts$Estimate) - 1) * 100)[2]
rev((exp(unis$Estimate) - 1) * 100)[2]
rev((exp(orgs$Estimate) - 1) * 100)[2]

## Resource mobilization
rev((exp(govts$Estimate) - 1) * 100)[3]
rev((exp(unis$Estimate) - 1) * 100)[3]
rev((exp(orgs$Estimate) - 1) * 100)[3]

## Market formation
rev((exp(govts$Estimate) - 1) * 100)[4]
rev((exp(unis$Estimate) - 1) * 100)[4]
rev((exp(orgs$Estimate) - 1) * 100)[4]
rev((exp(co$Estimate) - 1) * 100)[4]

rev((exp(co$Estimate) - 1) * 100)[1]
rev((exp(co$Estimate) - 1) * 100)[2]
rev((exp(co$Estimate) - 1) * 100)[3]
rev((exp(co$Estimate) - 1) * 100)[4]

