# Returns dataset (ordered) only with the duplicated entries
# x is data.frame/data.table with respective column
# dat is data.frame/data.table
# e.g. .fetch_dupl(dat = US_trait_DB, x = US_trait_DB[,.(Taxa)])
.fetch_dupl <- function(
                        dat,
                        x) {
  # dat as data.frame
  dat <- as.data.frame(dat)

  # Search for duplicates
  n_occur <- data.frame(table(x))
  id <- n_occur[n_occur$Freq > 1, "x"]

  # filter for id in original data
  col <- names(x)
  output <- dat[dat[, c(col)] %in% id, ]

  # order alphabetically
  output <- output[order(output[, c(col)]), ]
  return(output)
}

#### Aggregate duplicate taxa entries ####
# x can look like trait_test[, c("Feed_mode_prim", "Feed_mode_sec", "Habit_prim")] 
# index like taxa_test[, "Taxa"]
.agg_dupl <- function(x, index) {
    # Conversion prohibitet by I()
    # -> otherwise char vec. would be represented as their factor levels
    aggregate(I(x),
        by = list(index),
        function(y) {
            y <- y[!is.na(y)]
            if (!length(y)) {
                y <- NA
            }
            y <- y[!duplicated(y)]
            y <- sample(y, 1)
            return(y)
        }
    )
}
