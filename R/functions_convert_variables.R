#' convert a genomic range from CHR:START-END format to the actual values
#' 
#' @param genomic_range a string in the format CHR:START-END
convert_range_to_list <- function(genomic_range = character()) {
  # convert a genomic range of from CHR:START-END to the values
  # CHR, START, and END
  
  # split into chromosome (first value) and positions (second value)
  # they should be separated by a colon (:)
  split <- unlist(stringr::str_split(genomic_range, ":"))
  
  if (length(split) != 2){
    return (list(chr = "chr0", start = 0, end = 0))
  }
  
  chrome <- split[[1]]
  position <- split[[2]]
  
  # split position into start (first value) and end (second value)
  # they should be separated by a dash (-)
  split <- unlist(stringr::str_split(position, "-"))
  
  if (length(split) != 2){
    return (list(chr = chrome, start = 0, end = 0))
  }
  
  start <- split[[1]]
  end <- split[[2]]
  
  # convert values into integers
  start <- tryCatch({
    as.integer(start)
  }, error=function(cond) {
    0
  })
  end <- tryCatch({
    as.integer(end)
  }, error=function(cond) {
    0
  })
  
  # make sure that end is bigger than start
  if (end < start){
    return(list(chr = chrome, start = end, end = start))
  }
  
  return(list(chr = chrome, start = start, end = end))
}