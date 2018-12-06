#################################################
#### Stats: Multiple Correspondance Analysis ####
#################################################


# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"
plot <- "~/Schreibtisch/Thesis/data/final/plots"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(dplyr)
library(vegan)
library(cluster)
library(FactoMineR)
library(factoextra)
library(beepr)


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
trait_fin <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin.csv"), sep = ",", row.names = 1, header = TRUE)
str(trait_EUR)


# --------------------------------------------------------------------------------------------------------------- #
#### NMDS ####
# --- Distance Matrix
trait_fin <- trait_fin %>%
  mutate(rowSums = rowSums(.[3:ncol(trait_fin)]))

# Delete rows with no information
zero <- filter(trait_fin, rowSums == 0)

trait_fin <- trait_fin %>%
  slice(which(rowSums > 0)) %>%
  select(-rowSums)

# --- Calculate distance matrix
dist_trait <- vegdist(trait_fin[, 3:ncol(trait_fin)], method = 'bray')

# --- Run NMDS
nmds <- metaMDS(dist_trait); beep(4)
plot(nmds, type = 'text') 

# Outliers: 103, 225, 322)

# --- Calculate new distance matrix and NMDS
trait_fin <- trait_fin[-c(103, 225, 322), ]
dist_trait <- vegdist(trait_fin[, 3:ncol(trait_fin)], method = 'bray')
nmds <- metaMDS(dist_trait); beep(4)
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
  geom_polygon(data = hull, aes(x = NMDS1, y = NMDS2, fill = region, group = region), alpha = 0.20) +
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
# But the regions (EU, NoA) explain only 6.8 % of the variance 

# --- Check assumptions
densityplot(permustats(trait_pmv))


# --------------------------------------------------------------------------------------------------------------- #
#### MCA ####

# --- Load data
trait_mca <- read.table(file.path(path, "final", "macroinvertebrate_ALL_mod.csv"), sep = ",", row.names = 1, header = TRUE)
head(trait_mca)

test_MCA <- MCA(trait_mca[2:ncol(trait_mca)], graph = FALSE)
test_MCA

# Biplot
cats = apply(trait_mca[2:ncol(trait_mca)], 2, function(x) nlevels(as.factor(x)))
cats

MCA_vars <- data.frame(test_MCA$var$coord, Variable = rep(names(cats), cats))
MCA_obs <- data.frame(test_MCA$ind$coord)

# ggplot(data = MCA_vars, aes(x = Dim.1, y = Dim.2, label = rownames(MCA_vars))) +
#   geom_hline(yintercept = 0, colour = "gray70") +
#   geom_vline(xintercept = 0, colour = "gray70") +
#   geom_text(aes(colour = Variable)) +
#   ggtitle("MCA plot of traits")
# 
# dev.copy(pdf, "MCA biplot.pdf")
# dev.off()

# --- Screeplot
fviz_screeplot(test_MCA, addlabels = TRUE, ylim = c(0, 20))
ggsave("MCA_screeplot.png", path = plot)

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

ggsave("MCA_biplot_density.png", path = plot)

# --- Contribution of variables
fviz_mca_var(test_MCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())

ggsave("MCA_vars_contribution.png", path = plot)


# --- Ellipse plot
fviz_ellipses(test_MCA, 1:4, geom = "point")
fviz_ellipses(test_MCA, 5:8, geom = "point")

ggsave("MCA_ellipse.png", path = plot)
