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
#### NMDS ####
# --- Load data
trait_fin <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin.csv"), sep = ",", row.names = 1, header = TRUE)
str(trait_fin)

# --- Distance Matrix
trait_fin <- trait_fin %>%
  mutate(rowSums = rowSums(.[4:ncol(trait_fin)]))

# Delete rows with no information
trait_fin <- trait_fin %>%
  slice(which(rowSums > 0)) %>%
  select(-rowSums)

# --- Calculate distance matrix
dist_trait <- vegdist(trait_fin[, 4:ncol(trait_fin)], method = 'bray')

# --- Run NMDS
nmds <- metaMDS(dist_trait); beep(4)
stressplot(nmds)
plot(nmds, type = 'text') 


# Outlier: 110, and maybe 17
trait_fin <- trait_fin[-c(17, 110), ]
dist_trait <- vegdist(trait_fin[, 4:ncol(trait_fin)], method = 'bray')

set.seed(123)
nmds <- metaMDS(dist_trait); beep(4)
stressplot(nmds)
plot(nmds, type = 'text') 

# --- Plot NMDS
scores <- as.data.frame(scores(nmds)) # Site scores
scores$site <- rownames(scores) # Site scores

reg <- trait_fin$region # Extract region levels
scores$region <- reg # Add region levels to the site scores
head(scores)

grp_EUR <- scores[scores$region == "EUR", ][chull(scores[scores$region == "EUR", c("NMDS1", "NMDS2")]), ] # hull values EUR
grp_NAM <- scores[scores$region == "NAM", ][chull(scores[scores$region == "NAM", c("NMDS1", "NMDS2")]), ] # hull values NAM
grp_AUS <- scores[scores$region == "AUS", ][chull(scores[scores$region == "AUS", c("NMDS1", "NMDS2")]), ] # hull values AUS
hull <- rbind(grp_EUR, grp_NAM, grp_AUS)

ggplot() + 
  geom_polygon(data = hull, aes(x = NMDS1, y = NMDS2, fill = region, group = region), alpha = 0.2) +
  geom_point(data = scores, aes(x = NMDS1, y = NMDS2, colour = region, shape = region), size = 1.5) +
  scale_colour_manual(values = c("EUR" = "green", "NAM" = "blue", "AUS" = "red")) +
  coord_equal() +
  theme_bw() +
  ggtitle("NMDS of trait data (Databases: EUR, NAM, AUS)")

ggsave("trait_NMDS.png", path = plot)

# op <- ordiplot(nmds, type = 'n')
# 
# cols = c('darkred', 'green')
# points(nmds, pch = 16, col = cols[trait_fin$region])
# 
# ordispider(nmds, groups = trait_fin$region, label = TRUE, col = 'lightgray')
# ordihull(nmds, groups = trait_fin$region, lty = 'dotted')
# legend("bottomright", pch = 16, col = cols, legend = levels(rait_fin$region))


# --------------------------------------------------------------------------------------------------------------- #
#### PERMANOVA ####
trait_pmv <- adonis(dist_trait ~ region, data = trait_fin, 
              permutations = 999, 
              method = 'bray'); beep(4)
trait_pmv

# The traits are statistically different between the regions. 
# But the regions (EUR, NAM, AUS) explain only 20.35 % of the variance 

# --- Check assumptions
densityplot(permustats(trait_pmv))

# --------------------------------------------------------------------------------------------------------------- #

#### MCA ####

# ---- Load Data ----
EUR <- read.table(file.path(path, "final", "macroinvertebrate_EUR_mod.csv"), sep = ",", row.names = 1, header = TRUE)
NAM <- read.table(file.path(path, "final", "macroinvertebrate_NAM_mod.csv"), sep = ",", row.names = 1, header = TRUE)
AUS <- read.table(file.path(path, "final", "macroinvertebrate_AUS_mod.csv"), sep = ",", row.names = 1, header = TRUE)
ALL <- read.table(file.path(path, "final", "macroinvertebrate_ALL_mod.csv"), sep = ",", row.names = 1, header = TRUE)

# ------------------------------------------------------------- #
# ---- Europe ----
str(EUR)

mca_EUR <- MCA(EUR[3:ncol(trait_mca)], graph = FALSE)

# Biplot
cats = apply(EUR[3:ncol(trait_mca)], 2, function(x) nlevels(as.factor(x)))
cats

MCA_vars <- data.frame(mca_EUR$var$coord, Variable = rep(names(cats), cats))
MCA_obs <- data.frame(mca_EUR$ind$coord)

# ggplot(data = MCA_vars, aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars))) +
#   geom_hline(yintercept = 0, colour = "gray70") +
#   geom_vline(xintercept = 0, colour = "gray70") +
#   geom_text(aes(colour = Variable)) +
#   ggtitle("MCA plot of traits")
# 
# dev.copy(pdf, "MCA biplot.pdf")
# dev.off()

# --- Screeplot
fviz_screeplot(mca_EUR, addlabels = TRUE, ylim = c(0, 20))
ggsave("MCA_EUR_screeplot.png", path = plot)

# --- Biplot with density curves
ggplot(data = MCA_obs, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = MCA_vars, aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars), colour = Variable)) +
  ggtitle("MCA plot of traits and their modalities") +
  scale_colour_discrete(name = "Variable") +
  theme_bw() +
  xlab("Dim 1 (14.5 %)") +
  ylab("Dim 2 (9.5 %)")

ggsave("MCA_EUR_biplot_density.png", path = plot)

# --- Contribution of variables
fviz_mca_var(mca_EUR, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())

ggsave("MCA_EUR_vars_contribution.png", path = plot)


# --- Ellipse plot
fviz_ellipses(mca_EUR, 1:4, geom = "point")
fviz_ellipses(mca_EUR, 5:9, geom = "point")

# ggsave("MCA_ellipse.png", path = plot)


# ------------------------------------------------------------- #
# ---- North America  ---- 
str(NAM)

mca_NAM <- MCA(NAM[3:ncol(trait_mca)], graph = FALSE)

# Biplot
cats = apply(NAM[3:ncol(trait_mca)], 2, function(x) nlevels(as.factor(x)))
cats

MCA_vars <- data.frame(mca_NAM$var$coord, Variable = rep(names(cats), cats))
MCA_obs <- data.frame(mca_NAM$ind$coord)

# ggplot(data = MCA_vars, aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars))) +
#   geom_hline(yintercept = 0, colour = "gray70") +
#   geom_vline(xintercept = 0, colour = "gray70") +
#   geom_text(aes(colour = Variable)) +
#   ggtitle("MCA plot of traits")
# 
# dev.copy(pdf, "MCA biplot.pdf")
# dev.off()

# --- Screeplot
fviz_screeplot(mca_NAM, addlabels = TRUE, ylim = c(0, 20))
ggsave("MCA_NAM_screeplot.png", path = plot)

# --- Biplot with density curves
ggplot(data = MCA_obs, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = MCA_vars, aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars), colour = Variable)) +
  ggtitle("MCA plot of traits and their modalities") +
  scale_colour_discrete(name = "Variable") +
  theme_bw() +
  xlab("Dim 1 (16.3 %)") +
  ylab("Dim 2 (10.2 %)")

ggsave("MCA_NAM_biplot_density.png", path = plot)

# --- Contribution of variables
fviz_mca_var(mca_NAM, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())

ggsave("MCA_NAM_vars_contribution.png", path = plot)


# --- Ellipse plot
fviz_ellipses(mca_NAM, 1:4, geom = "point")
fviz_ellipses(mca_NAM, 5:9, geom = "point")

# ggsave("MCA_ellipse.png", path = plot)


# ------------------------------------------------------------- #
# ---- Australia  ---- 
str(AUS)

mca_AUS <- MCA(AUS[3:ncol(trait_mca)], graph = FALSE)

# Biplot
cats = apply(AUS[3:ncol(trait_mca)], 2, function(x) nlevels(as.factor(x)))
cats

MCA_vars <- data.frame(mca_AUS$var$coord, Variable = rep(names(cats), cats))
MCA_obs <- data.frame(mca_AUS$ind$coord)

# ggplot(data = MCA_vars, aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars))) +
#   geom_hline(yintercept = 0, colour = "gray70") +
#   geom_vline(xintercept = 0, colour = "gray70") +
#   geom_text(aes(colour = Variable)) +
#   ggtitle("MCA plot of traits")
# 
# dev.copy(pdf, "MCA biplot.pdf")
# dev.off()

# --- Screeplot
fviz_screeplot(mca_AUS, addlabels = TRUE, ylim = c(0, 50))
ggsave("MCA_AUS_screeplot.png", path = plot)

# --- Biplot with density curves
ggplot(data = MCA_obs, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = MCA_vars, aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars), colour = Variable)) +
  ggtitle("MCA plot of traits and their modalities") +
  scale_colour_discrete(name = "Variable") +
  theme_bw() +
  xlab("Dim 1 (16.3 %)") +
  ylab("Dim 2 (10.2 %)")

ggsave("MCA_AUS_biplot_density.png", path = plot)

# --- Contribution of variables
fviz_mca_var(mca_AUS, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())

ggsave("MCA_AUS_vars_contribution.png", path = plot)


# --- Ellipse plot
fviz_ellipses(mca_AUS, 1:4, geom = "point")
fviz_ellipses(mca_AUS, 5:9, geom = "point")

# ggsave("MCA_ellipse.png", path = plot)


# ------------------------------------------------------------- #
# ---- All Regions  ----
str(ALL)
names(ALL) <- c("Order", "Family", "Region", "pH Preference", "Temperature Preference", "Feeding Mode", "Locomotion",
                "Respiration", "Drift", "Size", "Reproduction", "Aquatic Stages", "Voltinism") 

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
# Prepare plotting
row.names(MCA_vars) <- c("Australia", "Europe", "North America", 
                         "Acidic", "Neutral/Alkaline", 
                         "Cold", "Indifferent", "Moderate", "Very Cold", "Warm",
                         "Filter-feeder", "Gatherer", "Parasite", "Predator", "Scraper", "Shredder",
                         "Burrower", "Sessil", "Skater", "Sprawler", "Swimmer",
                         "Atmospheric", "Gills", "Plastron", "Spiracle", "Tegument",
                         "High", "Low",
                         "Large", "Medium", "Small",
                         "Aquatic Eggs", "Terrestric Eggs", "Ovoviparity",
                         "Eggs only", "-Larva", "-Pupa", "-Adult",
                         "Semivoltine", "Univoltine", "Multivoltine")


# Plot
ggplot(data = MCA_obs, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.1) +
  # geom_density2d(colour = "gray80") +
  
  geom_text(data = MCA_vars,
            aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars), colour = Variable)) +
  # scale_colour_discrete(name = "Variable") +
  theme_bw() +
  scale_color_discrete() +
  
  ggtitle("MCA plot of traits and their modalities") +
  xlab("Dim 1 %%%") +
  ylab("Dim 2 %%%")

ggsave("MCA_ALL_biplot_density_dim12.pdf", path = plot)

# ---- Biplot with density curves (3rd and 4th dimension)
ggplot(data = MCA_obs, aes(x = Dim.3, y = Dim.4)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.1) +
  # geom_density2d(colour = "gray80") +
  
  geom_text(data = MCA_vars,
            aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars), colour = Variable)) +
  # scale_colour_discrete(name = "Variable") +
  theme_bw() +
  scale_color_ucscgb() +
  
  ggtitle("MCA plot of traits and their modalities") +
  xlab("Dim 1 %%%") +
  ylab("Dim 2 %%%")


ggsave("MCA_ALL_biplot_density_dim34.pdf", path = plot)

# --- Contribution of variables
fviz_mca_var(mca_ALL, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())

ggsave("MCA_ALL_vars_contribution.png", path = plot)


# --- Ellipse plot
par(mfrow = c(3,1))
fviz_ellipses(mca_ALL, 1, geom = "point", palette = "aaas")
fviz_ellipses(mca_ALL, c(2, 3), geom = "point")
fviz_ellipses(mca_ALL, c(4, 5), geom = "point")
fviz_ellipses(mca_ALL, c(6, 7), geom = "point")
fviz_ellipses(mca_ALL, c(8, 9), geom = "point")
fviz_ellipses(mca_ALL, 10, geom = "point", palette = "startrek")


ggsave("MCA_ellipse.pdf", path = plot)

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
