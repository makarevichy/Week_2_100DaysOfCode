#a function that looks for different missing values in a data frame and
#returns a table with a view of the missing data in each column.
find_na <- function(df) {
  require(pryr)
  if (!is.data.frame(df)) 
    stop("\"df\" not a data.frame", call. = F)
  na_value <- vapply(df, partial(is.na), logical(length = nrow(df)))
  l_char_0 <- vapply(df, function(x) ifelse(nchar(x) == 0, 
                                            TRUE, FALSE), logical(length = nrow(df)))
  emp_line <- vapply(df, partial(grepl, pattern = "\\s+"), 
                     logical(length = nrow(df)))
  tab <- list(na_value = na_value, l_char_0 = l_char_0, emp_line = emp_line)
  tab <- lapply(tab, function(x) apply(x, 2, sum, na.rm = T))
  t(data.frame(na_value = tab[[1]], l_char_0 = tab[[2]], emp_line = tab[[3]]))
}

#test
mtcars2 <- mtcars
for(i in seq_along(mtcars2)){
  x <- sample(1:nrow(mtcars2), sample(2:5, 2))
  for(j in 1:length(x)){
    mtcars2[j, i] <- sample(c(NA, '', ' ', '    '), 1) 
  }
}

find_na(mtcars2)
find_na(airquality)