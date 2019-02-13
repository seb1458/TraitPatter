#################################################
#### Stats: Multiple Correspondance Analysis ####
#################################################


# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"
plot <- "~/Schreibtisch/Thesis/final_paper/Figures/results/MCA"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(data.table)
library(vegan)
library(cluster)
library(FactoMineR)
library(factoextra)
library(beepr)      # beep functions
library(RColorBrewer)
# library(ggpubr)     # "Publication Ready" theme
# library(ggsci)      # Scientific journal palettes


# --------------------------------------------------------------------------------------------------------------- #
#### Change names of categories ####
source("~/Schreibtisch/Thesis/scripts/stats/cleanUp.R")
source("~/Schreibtisch/Thesis/scripts/stats/prepDat.R")


# --------------------------------------------------------------------------------------------------------------- #
#### Prepare Data ####



# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
NMDS <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin_final.csv"), sep = ",", row.names = 1, header = TRUE)
MCA <- read.table(file.path(path, "final", "macroinvertebrate_ALL_mod_final.csv"), sep = ",", check.names = FALSE)


# --------------------------------------------------------------------------------------------------------------- #
#### Preparation ####
checkdt <- data.table(check)
checkdt[Region == "Australia", lapply(.SD, function(y) sum(y)/nrow(checkdt[grepl("Australia", check$Region), ])), .SDcols = names(checkdt) %like% ".*sum"]


# --------------------------------------------------------------------------------------------------------------- #
#### NMDS ####
trait_fin <- NMDS
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
nmds <- metaMDS(dist_trait, k = 2); beep(4)
stressplot(nmds) # Stress 0.167
plot(nmds, type = 'text') 

# No extreme outliers

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


#### PERMANOVA ####
# --------------------------------------------------------------------------------------------------------------- #
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
summary(MCA)

# --- MCA
mca_ALL <- MCA(MCA[3:ncol(MCA)], graph = FALSE)

# --- Investigate via FactoInvestigate
# Investigate(mca_ALL, file = "MCA.Rmd", document = c("word_document", "pdf_document"))

# Biplot
cats <- apply(MCA[3:ncol(MCA)], 2, function(x) nlevels(as.factor(x)))
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
  xlim(-1.5, 2.5) +
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
  # xlim(-2, 2) +
  # ylim(-2, 2) +
  # geom_density2d(colour = "gray80") +
  
  geom_text(data = MCA_vars,
            aes(x = Dim.3, y = Dim.4, label = rownames(MCA_vars), colour = Variable)) +
  # scale_colour_discrete(name = "Variable") +
  theme_bw() +
  
  # ggtitle("MCA plot of traits and their modalities") +
  xlab("Dim 3 %%%") +
  ylab("Dim 4 %%%")


ggsave("MCA_ALL_biplot_density_dim34.pdf", spath = plot)

# --- Contribution of variables
fviz_mca_var(mca_ALL, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             alpha.var = "cos2", 
             ggtheme = theme_minimal())

ggsave("MCA_ALL_vars_contribution.png", path = plot)


# --- Contribution of variables to dimensions
fviz_contrib(mca_ALL, choice = "var", axes = 1:2, top = 20)


# --- Representation of variables by dimensions
fviz_cos2(mca_ALL, choice = "var", axes = 1:4)


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


ggsave("MCA_ellipse.pdf", path = plot)


# --- Clustering of MCA
mca_hcpc <- HCPC(mca_ALL)
