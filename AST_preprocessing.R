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


# compare Unresolved_taxa column with Genus column with regard to sp. and spp. 
# AST_trait_DB[Unresolved_taxa %like% "sp.$|spp.$", (lapply(.SD, function(y) ifelse(is.na(y), Unresolved_taxa,
#                                                                                        as.character(y)))), 
#              .SDcols = "Genus"]


# Delete Genus_and_species column
AST_trait_DB[, Genus_and_species := NULL]


# set order of columns
setcolorder(AST_trait_DB, c("Order", "Family", "Genus", "Species", "long_code", "short_code","Unresolved_taxa"))


# Some colnames contian "." or ".." -> needs to be changed
setnames(x = AST_trait_DB, old = names(AST_trait_DB), 
         new = gsub(pattern = "\\.", replacement = "_", x = names(AST_trait_DB)))


# still some colnames have two "_" 
setnames(x = AST_trait_DB, old = names(AST_trait_DB), 
         new = gsub(pattern = "\\__", replacement = "_", x = names(AST_trait_DB)))


# Some columns have list as type -> change to write table
grep("list", sapply(AST_trait_DB, typeof), value = TRUE)


# write
write.table(x = AST_trait_DB, file = "/home/kunz/Dokumente/Trait DB/Australian/Australian_macroinv_trait_DB_upd.csv",
            sep = ",", dec = ".")