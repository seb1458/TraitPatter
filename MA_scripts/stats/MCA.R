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
library(ggsci)      # Scientific journal palettes
library(ggrepel)
# library(ggpubr)     # "Publication Ready" theme



# --------------------------------------------------------------------------------------------------------------- #
#### Change names of categories ####
source("~/Schreibtisch/Thesis/scripts/stats/cleanUp.R")
source("~/Schreibtisch/Thesis/scripts/stats/prepDat.R")


# --------------------------------------------------------------------------------------------------------------- #
#### Prepare Data ####



# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
NMDS <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin_final.csv"), sep = ",", row.names = 1, header = TRUE)
NMDS[is.na(NMDS)] <- 0

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
stressplot(nmds) # Stress 0.163

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

# Dimension 1+2
ggplot() + 
  geom_polygon(data = hull, aes(x = NMDS1, y = NMDS2, fill = Region, group = Region), alpha = 0.2) +
  geom_point(data = scores, aes(x = NMDS1, y = NMDS2, colour = Region, shape = Region), size = 1.5) +
  scale_colour_manual(values = c("Europe" = "forestgreen", "North America" = "blue", "Australia" = "red")) +
  coord_equal() +
  theme_bw()

ggsave("trait_NMDS_12.pdf", path = plot)

# Dimension 2+3
ggplot() + 
  geom_polygon(data = hull, aes(x = NMDS3, y = NMDS4, fill = Region, group = Region), alpha = 0.2) +
  geom_point(data = scores, aes(x = NMDS3, y = NMDS4, colour = Region, shape = Region), size = 1.5) +
  scale_colour_manual(values = c("Europe" = "forestgreen", "North America" = "blue", "Australia" = "red")) +
  coord_equal() +
  theme_bw()

ggsave("trait_NMDS_23.pdf", path = plot)


#### PERMANOVA ####
# --------------------------------------------------------------------------------------------------------------- #
trait_pmv <- adonis(dist_trait ~ Region, data = trait_fin, 
                    permutations = 999, 
                    method = 'bray'); beep(4)

trait_pmv

# The traits are statistically different between the regions. 
# But the regions (EUR, NAM, AUS) explain only 17.3 % of the variance 

# --- Check assumptions

pdf(file = "~/Schreibtisch/Thesis/final_paper/Figures/results/permutation_plot.pdf")
densityplot(permustats(trait_pmv))
dev.off()

# --------------------------------------------------------------------------------------------------------------- #
#### MCA ####
summary(MCA)

mca_ALL <- MCA(MCA[3:ncol(MCA)], ncp = 11, graph = FALSE)

# --- Screeplot
fviz_screeplot(mca_ALL, addlabels = TRUE, ylim = c(0, 15), ncp = 11) + ggtitle(" ")
ggsave("MCA_screeplot.pdf", path = plot)

# --- Overview: Biplot
# Dimension 1+2
fviz_mca_biplot(mca_ALL, geom.ind = "point", repel = TRUE,
                alpha.ind = 0.5,
                title = "MCA - Biplot: Trait Data",
                axes = c(1,2))
ggsave("MCA_biplot_dim12.pdf", path = plot)

# Dimension 1+3
fviz_mca_biplot(mca_ALL, geom.ind = "point", repel = TRUE,
                alpha.ind = 0.5,
                title = "MCA - Biplot: Trait Data",
                axes = c(1,3))
ggsave("MCA_biplot_dim34.pdf", path = plot)

# Dimension 1+4
fviz_mca_biplot(mca_ALL, geom.ind = "point", repel = TRUE,
                alpha.ind = 0.5,
                title = "MCA - Biplot: Trait Data",
                axes = c(1,4))
ggsave("MCA_biplot_dim34.pdf", path = plot)

# Dimension 5+6
fviz_mca_biplot(mca_ALL, geom.ind = "point", repel = TRUE,
                alpha.ind = 0.5,
                title = "MCA - Biplot: Trait Data",
                axes = 5:6)
ggsave("MCA_biplot_dim56.pdf", path = plot)

# Dimension 7+8
fviz_mca_biplot(mca_ALL, geom.ind = "point", repel = TRUE,
                alpha.ind = 0.5,
                title = "MCA - Biplot: Trait Data",
                axes = 7:8)
ggsave("MCA_biplot_dim78.pdf", path = plot)

# Dimension 9+10
fviz_mca_biplot(mca_ALL, geom.ind = "point", repel = TRUE,
                alpha.ind = 0.5,
                title = "MCA - Biplot: Trait Data",
                axes = 9:10)
ggsave("MCA_biplot_dim910.pdf", path = plot)

# Dimension 10+11
fviz_mca_biplot(mca_ALL, geom.ind = "point", repel = TRUE,
                alpha.ind = 0.5,
                title = "MCA - Biplot: Trait Data",
                axes = 10:11)
ggsave("MCA_biplot_dim1011.pdf", path = plot)

# --- Quality of Representation of the first two axes
fviz_cos2(mca_ALL, choice = "var", axes = 1:2) + ggtitle(" ")
ggsave("MCA_quality.pdf", path = plot)

# --- Variables contributing the most to dimensions
fviz_contrib(mca_ALL, choice = "var", axes = 1, top = 10) + ylim(0, 20) + ggtitle("Dimension 1")
ggsave("MCA_contrib_dim1.pdf", path = plot)

fviz_contrib(mca_ALL, choice = "var", axes = 2, top = 10) + ylim(0, 20) + ggtitle("Dimension 2") + ylab(" ")
ggsave("MCA_contrib_dim2.pdf", path = plot)

# ------------------------------------------ #
# Biplot with ggplot2
cats <- apply(MCA[3:ncol(MCA)], 2, function(x) nlevels(as.factor(x)))
cats

MCA_vars <- data.frame(mca_ALL$var$coord, Variable = rep(names(cats), cats))
MCA_obs <- data.frame(mca_ALL$ind$coord)

# Define palette
cols <- c("blue", "blue", "blue", "red", "blue",
          "blue", "blue", "blue", "blue")

# Plot 1+2 dimension
ggplot(data = MCA_obs, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.5) +
  xlim(-2, 2.5) +
  ylim(-1.2, 3) +
  # geom_density2d(colour = "gray80") +
  
  geom_text_repel(data = MCA_vars, aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars), colour = Variable), show.legend = FALSE) +
  geom_point(data = MCA_vars, aes(x = Dim.1, y = Dim.2, colour = Variable), shape = 17, show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  theme_bw() +
  guides(fill = FALSE) +
  
  # ggtitle("MCA plot of traits and their modalities") +
  xlab("Dim 1 14%") +
  ylab("Dim 2 10%")

ggsave("MCA_ALL_biplot_density_dim12.pdf", path = plot)

# Plot 1+3 dimension
ggplot(data = MCA_obs, aes(x = Dim.1, y = Dim.3)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.5) +
  xlim(-2, 2.5) +
  ylim(-1.2, 3) +
  # geom_density2d(colour = "gray80") +
  
  geom_text_repel(data = MCA_vars, size = 5, aes(x = Dim.1, y = Dim.3, label = rownames(MCA_vars), colour = Variable), show.legend = FALSE) +
  geom_point(data = MCA_vars, aes(x = Dim.1, y = Dim.3, colour = Variable), shape = 17, show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  theme_bw(base_size = 18) +
  guides(fill = FALSE) +
  
  # ggtitle("MCA plot of traits and their modalities") +
  xlab("Dim 1 14%") +
  ylab("Dim 3 8%")

ggsave("MCA_ALL_biplot_density_dim13.pdf", path = plot)

# Plot 1+4 dimension
ggplot(data = MCA_obs, aes(x = Dim.1, y = Dim.4)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.5) +
  xlim(-2, 2.5) +
  ylim(-1.2, 3) +
  # geom_density2d(colour = "gray80") +
  
  geom_text_repel(data = MCA_vars, size = 5, aes(x = Dim.1, y = Dim.4, label = rownames(MCA_vars), colour = Variable), show.legend = FALSE) +
  geom_point(data = MCA_vars, aes(x = Dim.1, y = Dim.4, colour = Variable), shape = 17, show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  theme_bw(base_size = 18) +
  guides(fill = FALSE) +
  
  # ggtitle("MCA plot of traits and their modalities") +
  xlab("Dim 1 14%") +
  ylab("Dim 4 6.4%")

ggsave("MCA_ALL_biplot_density_dim14.pdf", path = plot)


# --- Contribution of variables to dimensions
fviz_contrib(mca_ALL, choice = "var", axes = 1:2, top = 10)

# --- Ellipse plot
# Dimension 1+2: Region
fviz_mca_ind(mca_ALL, label = "none", habillage = "Region", addEllipses = FALSE, ellipse.type = "confidence", 
             palette = c("red", "forestgreen", "blue"), axes = 1:2) + 
  ggtitle(" ") +
  theme(legend.position = "top")
  
ggsave("MCA_ellipse_region_dim12.pdf", path = plot)

# pH + temperature
fviz_ellipses(mca_ALL, c(2, 3),
              geom = "point", pointsize = 0.5,
              base_size = 20,
              addEllipses = FALSE,
              ggtheme = theme_bw(base_size = 13)) + 
  ggtitle(" ")
ggsave("MCA_ellipse1_dim12.pdf", path = plot)

# Feeding mode + locomotion
fviz_ellipses(mca_ALL, c(4, 5),
              geom = "point", pointsize = 1,
              addEllipses = FALSE,
              ggtheme = theme_bw(base_size = 13)) + 
  ggtitle(" ") + 
  ylab(" ") + 
  theme(axis.text.y = element_blank(),  axis.ticks.y = element_blank())
ggsave("MCA_ellipse2_dim12.pdf", path = plot)

# Respiration + size
fviz_ellipses(mca_ALL, c(6, 7),
              geom = "point", pointsize = 0.5,
              addEllipses = FALSE,
              ggtheme = theme_bw(base_size = 13)) + 
  ggtitle(" ") 
ggsave("MCA_ellipse3_dim12.pdf", path = plot)

# Reproduction + voltinism
fviz_ellipses(mca_ALL, c(8, 9),
              geom = "point", pointsize = 0.5,
              addEllipses = FALSE,
              ggtheme = theme_bw(base_size = 13)) + 
  ggtitle(" ") + 
  ylab(" ") + 
  theme(axis.text.y = element_blank(),  axis.ticks.y = element_blank())

ggsave("MCA_ellipse4_dim12.pdf", path = plot)
