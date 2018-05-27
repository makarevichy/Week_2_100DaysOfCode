waf <- function(val, n_row = NULL, legend, main = NULL) {
  require(RColorBrewer)
  if (!is.numeric(val)) 
    stop("'val' must be numeric vector", call. = FALSE)
  if (is.null(n_row)) {
    n_row <- 10
  }
  n_col <- ceiling(sum(val)/n_row)
  x <- 0:(n_col + 2)
  y <- 0:(n_row + 2)
  co <- vector("character", 0L)
  col_vec <- brewer.pal(8, name = "Set2")
  for (i in seq_along(val)) {
    y <- rep(col_vec[i], val[i])
    co <- c(co, y)
  }
  col_matrix <- matrix(c(co, rep("white", n_row - (sum(val)%%n_row))), 
                       ncol = n_row)
  # plot
  op <- par(bty = "n", xaxt = "n", yaxt = "n")
  if (dim(col_matrix)[2] > dim(col_matrix)[1]) {
    plot(1:(dim(col_matrix)[2] + 1), 1:(dim(col_matrix)[2] + 1), type = "n", 
         xlab = "", ylab = "", main = main)
    legend("top", border = "white", legend = legend, bty = "n", horiz = T, fill = unlist(unique(as.list(col_matrix))))
    for (j in 0:(dim(col_matrix)[2] - 1)) {
      rect(xleft = j + 1, ybottom = 1, xright = j + 1.95, ytop = 1.95, 
           border = "white")
      for (i in 1:(dim(col_matrix)[1])) {
        rect(xleft = 1 + j, ybottom = i, xright = 1 + 0.95 + j, 
             ytop = i + 0.95, col = col_matrix[i, j + 1], border = "white")
      }
    }
  } else {
    plot(1:(dim(col_matrix)[1] + 1), 1:(dim(col_matrix)[1] + 1), type = "n", 
         xlab = "", ylab = "", main = main)
    legend("right", border = "white", legend = legend, bty = "n", fill = unlist(unique(as.list(col_matrix))))
    for (j in 0:(dim(col_matrix)[2] - 1)) {
      rect(xleft = j + 1, ybottom = 1, xright = j + 1.95, ytop = 1.95, 
           border = "white")
      for (i in 1:(dim(col_matrix)[1])) {
        rect(xleft = 1 + j, ybottom = i, xright = 1 + 0.95 + j, 
             ytop = i + 0.95, col = col_matrix[i, j + 1], border = "white")
      }
    }
  }
  par(op)
}

#test
#jpeg('weekday//img//russian_parliament.jpg', width = 360, height = 360)
waf(c(343, 42, 39, 23, 2, 4), n_row = 15, 
    legend = c('United Russia', 'Communist Party', 'Liberal-democratic', 'Just Russia', 
               'Independent', 'Vacant'),
    main = 'Russian parliament')
#dev.off()
#jpeg('weekday//img//usa_congress.jpg', width = 360, height = 360)
waf(c(51, 47, 2), n_row = 13, 
    legend = c('Republican', 'Democratic', 'Independent'),
    main = 'United States Congress')
#dev.off()
