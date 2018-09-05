#### Freshwaterecology.info DB 2018 Macroinvertebrates ####

# path
data_in = "/home/kunz/Dokumente/Trait DB/Europe/Freshwater_info"

# getting links to the files
filelinks = list.files(data_in, pattern = "taxadbexport", full.names = TRUE)

# Read in 
output_db = replicate( length(filelinks),data.frame())

for(i in seq_along(filelinks)){

  # fetch line where actual DB starts
  ind = grep("Taxon", readLines(filelinks[1]))  
  
  # read in
  db = read.table(filelinks[1], skip = ind, sep = ";", header = TRUE, na.strings = (""), fill = TRUE)  
  
  # put into list
  output_db[[i]] = db

}  

# bind
freshecol_db = do.call(rbind, output_db)

# delete EU column
freshecol_db = freshecol_db[, !names(freshecol_db) == "EU"]

# rename taxa column
names(freshecol_db)[1] = "Taxon"

# delete Family names
freshecol_db1 = freshecol_db[-grep("[[:upper:]]{3}", freshecol_db$Taxon), ]

# Delete rows with just NA values
ind = apply(freshecol_db1[,-1], MARGIN = 1, function(y) all(is.na(y)) )
freshecol_db1[-ind,]

# 
