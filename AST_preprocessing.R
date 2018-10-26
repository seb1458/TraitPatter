# Packages
library(data.table)


# data location
data_in <- "/home/kunz/Dokumente/Trait DB"


# Load AST DB
AST_trait_DB <- read.table(file.path(
  data_in, "Scripts_TP_stage1", "MA_Seba", "macroinvertebrate_AUS.csv"
),
sep = ",", dec = "."
)


# 
AST_trait_DB <- as.data.table(AST_trait_DB)



# Trying to resolve crude taxa names
# differentiate into species column and unresolved taxa
AST_trait_DB[, `:=`(Species = lapply(Genus_and_species, function(y) {
      val <- grep("Genus|.*sp.*|group.*|unknown|.*unidentif.*|undifferentiated|.*SO[1-9]|.*L10.*|Ecnomina|Kingolus / Simsonia.*", 
                  y, value = TRUE, invert = TRUE, ignore.case = TRUE)
      val <- grep("[A-z] [A-z]", val, value = TRUE)
      ifelse(identical(val, character(0)), NA, val)
    }
  ), 
  Unresolved_taxa = lapply(Genus_and_species, function(y) {
    val <- grep("Genus|.*sp.*|group.*|unknown|.*unidentif.*|undifferentiated|.*SO[1-9]|.*L10.*|Ecnomina|Kingolus / Simsonia.*", 
                y, value = TRUE, ignore.case = TRUE)
    ifelse(identical(val, character(0)), NA, val)
      }
    )
)]


# Delete Genus_and_species column
AST_trait_DB[, Genus_and_species := NULL]


# set order of columns
setcolorder(AST_trait_DB, c("Order", "Family", "Genus", "Species", "long_code", "short_code","Unresolved_taxa"))
