#################################################
#### Stats: Multiple Correspondance Analysis ####
#################################################


# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"
plot <- "~/Schreibtisch/Thesis/final_paper/Figures/results"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(vegan)
library(cluster)
library(FactoMineR)
library(factoextra)
library(beepr)      # beep functions
library(RColorBrewer)
library(ggpubr)     # "Publication Ready" theme
library(ggsci)      # Scientific journal palettes


# --------------------------------------------------------------------------------------------------------------- #
#### Change names of categories ####
source("~/Schreibtisch/Thesis/scripts/stats/cleanUp.R")


# --------------------------------------------------------------------------------------------------------------- #
#### Check Trait Modalities for Double Entries ####
check <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin_fancy.csv"), sep = ",", row.names = 1, header = TRUE)
names(check)

check <- check %>%
  mutate(ph_sum = rowSums(.[grepl("ph_", names(check))]),
         feed_sum = rowSums(.[grepl("feed_", names(check))]),
         loc_sum = rowSums(.[grepl("loc_", names(check))]),
         resp_sum = rowSums(.[grepl("resp_", names(check))]),
         drift_sum = rowSums(.[grepl("drift_", names(check))]),
         life_sum = rowSums(.[grepl("life", names(check))]),
         size_sum = rowSums(.[grepl("size_", names(check))]),
         volt_sum = rowSums(.[grepl("volt_", names(check))]),
         stage_sum = rowSums(.[grepl("stage_", names(check))]),
         rep_sum = rowSums(.[grepl("rep_", names(check))]),
         ph_sum = rowSums(.[grepl("temp_", names(check))]),
         total_sum = rowSums(.[4:ncol(check)])) %>%
  filter(total_sum > 10)


# --------------------------------------------------------------------------------------------------------------- #
#### NMDS ####
# --- Load data
trait_fin <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin_fancy.csv"), sep = ",", row.names = 1, header = TRUE)
names(trait_fin)

# --- Distance Matrix
trait_fin <- trait_fin %>%
  mutate(rowSums = rowSums(.[4:ncol(trait_fin)]))

# Delete rows with no information
trait_fin <- trait_fin %>%
  slice(which(rowSums > 0)) %>%
  select(-rowSums)

# --- Calculate distance matrix
dist_trait <- vegdist(trait_fin[, 4:ncol(trait_fin)], method = 'jaccard')

# --- Run NMDS
nmds <- metaMDS(dist_trait); beep(4)
stressplot(nmds)
plot(nmds, type = 'text') 


# Outlier: 110, and maybe 17
trait_fin <- trait_fin[-c(9, 98), ]
dist_trait <- vegdist(trait_fin[, 4:ncol(trait_fin)], method = 'jaccard')

set.seed(123)
nmds <- metaMDS(dist_trait); beep(4)

stressplot(nmds) # Stress: 0.2
plot(nmds, type = 'text') 

# --- Plot NMDS
scores <- as.data.frame(scores(nmds)) # Site scores
scores$site <- rownames(scores) # Site scores

reg <- trait_fin$Region # Extract region levels
scores$Region <- reg # Add region levels to the site scores
head(scores)

grp_EUR <- scores[scores$Region == "Europe", ][chull(scores[scores$Region == "Europe", c("NMDS1", "NMDS2")]), ] # hull values EUR
grp_NAM <- scores[scores$Region == "North America", ][chull(scores[scores$Region == "North America", c("NMDS1", "NMDS2")]), ] # hull values NAM
grp_AUS <- scores[scores$Region == "Australia", ][chull(scores[scores$Region == "Australia", c("NMDS1", "NMDS2")]), ] # hull values AUS
hull <- rbind(grp_EUR, grp_NAM, grp_AUS)

ggplot() + 
  geom_polygon(data = hull, aes(x = NMDS1, y = NMDS2, fill = Region, group = Region), alpha = 0.2) +
  geom_point(data = scores, aes(x = NMDS1, y = NMDS2, colour = Region, shape = Region), size = 1.5) +
  scale_colour_manual(values = c("Europe" = "darkgreen", "North America" = "blue", "Australia" = "red")) +
  coord_equal() +
  theme_bw()

ggsave("trait_NMDS.pdf", path = plot)


# --------------------------------------------------------------------------------------------------------------- #
#### PERMANOVA ####
trait_pmv <- adonis(dist_trait ~ Region, data = trait_fin, 
                    permutations = 999, 
                    method = 'bray'); beep(4)
trait_pmv

# The traits are statistically different between the regions. 
# But the regions (EUR, NAM, AUS) explain only 11.1 % of the variance 

# --- Check assumptions

pdf(file = "~/Schreibtisch/Thesis/final_paper/Figures/results/permutation_plot.pdf")
densityplot(permustats(trait_pmv))
dev.off()

# --------------------------------------------------------------------------------------------------------------- #
#### MCA ####

# ---- Load Data
ALL <- read.table(file.path(path, "final", "macroinvertebrate_ALL_mod_fancy.csv"), sep = ",", check.names = FALSE)
summary(ALL)

# Remove outliers (see above NMDS)
ALL <- ALL[, -c(17, 110)]

# --- MCA
mca_ALL <- MCA(ALL[3:ncol(ALL)], graph = FALSE)

# Biplot
cats = apply(ALL[3:ncol(ALL)], 2, function(x) nlevels(as.factor(x)))
cats

MCA_vars <- data.frame(mca_ALL$var$coord, Variable = rep(names(cats), cats))
MCA_obs <- data.frame(mca_ALL$ind$coord)

# ggplot(data = MCA_vars, aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars))) +
#   geom_hline(yintercept = 0, colour = "gray70") +
#   geom_vline(xintercept = 0, colour = "gray70") +
#   geom_text(aes(colour = Variable)) +
#   ggtitle("MCA plot of traits")
# 
# dev.copy(pdf, "MCA biplot.pdf")
# dev.off()

# --- Screeplot
fviz_screeplot(mca_ALL, addlabels = TRUE, ylim = c(0, 15))
ggsave("MCA_ALL_screeplot.pdf", path = plot)

# --- Biplot with density curves (1st and 2nd dimenstion)

ggplot(data = MCA_obs, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.1) +
  xlim(-1.7, 2) +
  ylim(-1, 2.5) +
  # geom_density2d(colour = "gray80") +
  
  geom_text(data = MCA_vars,
            aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars), colour = Variable)) +
  # scale_colour_discrete(name = "Variable") +
  theme_bw() +
  
  # ggtitle("MCA plot of traits and their modalities") +
  xlab("Dim 1 %%%") +
  ylab("Dim 2 %%%")

ggsave("MCA_ALL_biplot_density_dim12.pdf", path = plot)

# ---- Biplot with density curves (3rd and 4th dimension)
ggplot(data = MCA_obs, aes(x = Dim.3, y = Dim.4)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.1) +
  xlim(-2, 2) +
  ylim(-2, 2) +
  # geom_density2d(colour = "gray80") +
  
  geom_text(data = MCA_vars,
            aes(x = Dim.3, y = Dim.4, label = rownames(MCA_vars), colour = Variable)) +
  # scale_colour_discrete(name = "Variable") +
  theme_bw() +
  
  # ggtitle("MCA plot of traits and their modalities") +
  xlab("Dim 3 %%%") +
  ylab("Dim 4 %%%")


ggsave("MCA_ALL_biplot_density_dim34.pdf", path = plot)

# --- Contribution of variables
fviz_mca_var(mca_ALL, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             alpha.var = "cos2", 
             ggtheme = theme_minimal())

ggsave("MCA_ALL_vars_contribution.png", path = plot)


# --- Correlation between principal dimenions and variables
fviz_mca_var(mca_ALL, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# --- Ellipse plot
fviz_mca_ind(mca_ALL, label = "none", habillage = "Region", addEllipses = TRUE, ellipse.type = "confidence", 
             palette = c("red", "darkgreen", "blue"), ggtheme = theme_minimal()) 

fviz_ellipses(mca_ALL, c(2, 3), geom = "point")
fviz_ellipses(mca_ALL, c(4, 5), geom = "point")
fviz_ellipses(mca_ALL, c(6, 7), geom = "point")
fviz_ellipses(mca_ALL, c(8, 9), geom = "point")
fviz_ellipses(mca_ALL, 10, geom = "point", palette = "startrek")


ggsave("MCA_ellipse.pdf", path = plot)


# --- Test
fviz_mca_var(mca_ALL, choice = "mca.cor", repel = TRUE)


# ------------------------------------------------------------- #
# ---- Alternative ----
str(ALL)

# Classic MCA with plots
mca_ALL <- MCA(ALL[2:ncol(ALL)])
summary(mca_ALL)

# Plot of individual rows
plot(mca_ALL, invisible = c("var"), cex=0.7)

# Plot with levels colored homogenous for each cateory
plot(mca_ALL, invisible = c("ind"), hab = "quali") 

# Plots with confidence ellipses
plotellipses(mca_ALL, keepvar = c("region", "ph", "temperature", "feed_mode"))

# Top 10 most important categories
plot(mca_ALL, invisible = c("ind"), hab = "quali", selectMod = "cos2 10") 

# Top 20 categories contributing to MCA
plot(mca_ALL, invisible = c("ind"), hab = "quali", selectMod = "contrib 20")

# Top 10 most important categories and most important individuals
plot(mca_ALL, hab = "quali", select = "cos2 10", selectMod = "cos2 10", xlim = c(0, 2), ylim = c(-1, 2)) 
