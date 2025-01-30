library(network)
library(ergm)
library(dplyr)
library(Bergm)
library(ggplot2)
library(jtools)
library(stringr)
source('code/functions.R')

data_dir <- "~/Box/osa_networks/data_combined/"

geo_levels <- c('North Central Regional', 'Northeast Regional', 'USA National', 
                'South Regional', 'West Regional')

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

rm_coefs <- "Fixed -Inf"
rm_coefs <- c("Regional homophily: Northeast", 
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


## Reduce networks down to their respondent only form
## Start with ego-alter
networks4 <- readRDS(paste0(data_dir,'networks/fx_netstack_noiso_undir_universityindividual_4subnetworks.rds'))
for(i in 1:length(networks4)){
  networks4[[i]] %v% 'lvl' <- ifelse(networks4[[i]] %v% 'scale' %in% 
                                       c("National", "International"), 2,
                                     1)
  networks4[[i]] %v% 'role_refined' <- ifelse(networks4[[i]] %v%
                                                'role_refined' ==
                                                "production + post-production", 
                                              'post-production',
                                              networks4[[i]] %v% 'role_refined')
}

edges <- read.csv(paste0(data_dir, "nodes-edges/edges_universityindividual_nogeneric_long_cleanedscale.csv"))
nodes <- read.csv(paste0(data_dir,"nodes-edges/nodes_universityindividual_nogeneric_cleanedscale.csv")) %>% 
  filter(!duplicated(Operation))
egos_only <- nodes$Operation[nodes$Operation %in% edges$from]

## Use networks with isolates to identify egos
networks_iso4 <- readRDS(paste0(data_dir,'networks/fx_netstack_iso_undir_universityindividual_4subnetworks.rds'))
for(i in 1:length(networks_iso4)){
  networks_iso4[[i]] %v% 'lvl' <- ifelse(networks_iso4[[i]] %v% 'scale' %in% 
                                           c("National", "International"), 2,
                                         1)
  networks_iso4[[i]] %v% 'role_refined' <- ifelse(networks_iso4[[i]] %v%
                                                    'role_refined' ==
                                                    "production + post-production", 
                                                  'post-production',
                                                  networks_iso4[[i]] %v% 'role_refined')
}

adj_kdev <- as.matrix.network.adjacency(networks_iso4[[2]]) 
adj_kdif <- as.matrix.network.adjacency(networks_iso4[[3]])
adj_rmob <- as.matrix.network.adjacency(networks_iso4[[4]])
adj_sc <- as.matrix.network.adjacency(networks_iso4[[5]])

adj_kdev_ego <- adj_kdev[rownames(adj_kdev) %in% egos_only, colnames(adj_kdev) %in% egos_only]
adj_kdif_ego <- adj_kdif[rownames(adj_kdif) %in% egos_only, colnames(adj_kdif) %in% egos_only]
adj_rmob_ego <- adj_rmob[rownames(adj_rmob) %in% egos_only, colnames(adj_rmob) %in% egos_only]
adj_sc_ego <- adj_sc[rownames(adj_sc) %in% egos_only, colnames(adj_sc) %in% egos_only]

kdev_egos <- names(which(rowSums(adj_kdev_ego) != 0))
kdif_egos <- names(which(rowSums(adj_kdif_ego) != 0))
rmob_egos <- names(which(rowSums(adj_rmob_ego) != 0))
sc_egos <- names(which(rowSums(adj_sc_ego) != 0))
egos_only4 <- list(egos_only, kdev_egos, kdif_egos, rmob_egos, sc_egos)

networks4_reduced <- list()
for(i in 1:length(networks4)){
  egos_keep <- which(networks4[[i]] %v% 'vertex.names' %in% egos_only4[[i]])
  networks4_reduced[[i]] <- get.inducedSubgraph(networks4[[i]], v = egos_keep)
}


# 2. Comparing model estimates with different missing data techniques 

gwd_decay <- 0.1
gwe_decay <- 0.8
gwd_save <- ifelse(gwd_decay > 0.69 & gwd_decay < .7, 0.6, gwd_decay)
gwe_save <- ifelse(gwe_decay > 0.69 & gwe_decay < .7, 0.6, gwe_decay)

## Method 1 ---- 
m_reduced4 <- list()
for(i in 2:length(networks4_reduced)){
  base_actor = which(levels(factor(networks4_reduced[[i]] %v% 'role_refined')) == 'production')
  m_reduced4[[(i-1)]] <- ergm(networks4_reduced[[i]] ~ 
                        edges + 
                        gwdegree(gwd_decay, fixed = T) +
                        gwesp(gwe_decay, fixed = T) +
                        nodefactor('role_refined', base = base_actor) +
                        nodematch('geo_scale', diff = T, 
                                  levels = geo_levels) +
                        nodefactor('lvl', base = 1), 
                        control = control.ergm(seed = 21, parallel = 4,
                                               MCMLE.termination = 'Hummel',
                                               MCMLE.effectiveSize = NULL))
}

#saveRDS(m_reduced4, paste0('code/appendix/models/m_reduced4_gwd', 
#                     str_remove(gwd_save, '\\.'),
#                     '_gwe', str_remove(gwe_save, '\\.'), '.rds'))

## Method 2 ----
matrices4 <- readRDS(paste0(data_dir,'networks/fx_matrices_undir_universityindividual_4subnetworks.rds'))

# Re-run without a few params
m_offset4 <- list()
for(i in 2:length(networks4)){
  base_actor = which(levels(factor(networks4[[i]] %v% 'role_refined')) == 'production')
  m_offset4[[(i-1)]] <- ergm(networks4[[i]] ~ 
                               edges + 
                               gwdegree(gwd_decay, fixed = T) +
                               gwesp(gwe_decay, fixed = T) +
                               nodefactor('role_refined', base = base_actor) +
                               nodematch('geo_scale', diff = T, 
                                         levels = geo_levels) +
                               nodefactor('lvl', base = 1) +
                               nodefactor('respondent', base = 2) +
                               offset(edgecov(matrices4[[i]])),
                             offset.coef = -Inf,
                             control = control.ergm(seed = 21, parallel = 4,
                                                    MCMLE.termination = 'Hummel',
                                                    MCMLE.effectiveSize = NULL))
}

#saveRDS(m_offset4, paste0('code/appendix/models/m_offset4_gwd', 
#                          str_remove(gwd_save, '\\.'),
#                          '_gwe', str_remove(gwe_save, '\\.'), '.rds'))

## Method 3 ----
alters_only <- unique(nodes$Operation[!(nodes$Operation %in% edges$from)])

# First, make the networks the same as ego-alter, so changing size
networks_m4 <- networks4[2:5]
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

mb4 <- list()
for(i in 1:length(networks_missing4)){
  base_actor = which(levels(factor(networks_missing4[[i]] %v% 'role_refined')) == 'production')
  m0b <- Bergm::bergmM(networks_missing4[[i]] ~ edges +
                         gwdegree(gwd_decay, fixed = T) + 
                         gwesp(gwe_decay, fixed = T) +
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
  mb4[[i]] <- m0b
}

#saveRDS(mb4, paste0('code/appendix/models/m0b4_egoalter_gwd', 
#                    str_remove(gwd_save, '\\.'),
#                    '_gwe', str_remove(gwe_save, '\\.'), '.rds'))

## BGOF plots ----
# If I run this line by line, it does not work. But if I run the whole chunk, it does
names <- c("kdev", "kdif", 
           "rmob", "sc")
#for(i in 1:length(names)){
#  pdf(paste0("figures/bgof_m0b4_egoalter_",
#             names[i], "_gwd",
#             str_remove(gwd_save, '\\.'),
#             '_gwe', str_remove(gwe_save, '\\.'), '.pdf'),
#      width = 7, height = 3.5)
#  par(mfrow = c(1,1))
#  bgof(mb4[[i]], n.deg = 20, n.dist = 20, n.esp = 7)
#  dev.off()
#}

