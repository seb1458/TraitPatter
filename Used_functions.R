# Returns dataset (ordered) only with the duplicated entries
.fetch_dupl = function(
  dat,
  x  
)
{  
  # dat as data.frame
  if(is.data.table(dat))
    dat = as.data.frame(dat)
  
  # Search for duplicates 
  n_occur = data.frame(table(x))
  id = n_occur[n_occur$Freq > 1, "x"]
  
  # filter for id in original data
  col = names(x)
  output = dat[dat[, c(col)] %in% id,]
  
  # order alphabetically
  output = output[order(output[,c(col)]),]
  return(output)
}
