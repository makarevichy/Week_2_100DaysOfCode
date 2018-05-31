library(keras)
library(pracma)
library(abind)
library(magrittr)
library(purrr)

mnist <- dataset_mnist()
x_test <- mnist$test$x

#rotation can greatly improve the results in CNN
x_test_test <- lapply(1:10, function(x){
       list((x_test[x,,]),
       rot90(x_test[x,,], 1),
       rot90(x_test[x,,], 2),
       rot90(x_test[x,,], 3))
  }) %>% 
  do.call(c,.) %>% 
  abind(., along = 0)

x_test_test <- array_reshape(x_test_test, c(nrow(x_test_test), 28, 28, 1))

#thank's Michał Maj for this code
xy_coord <- data.frame(x = expand.grid(1:28, 28:1)[ ,1],
                      y = expand.grid(1:28, 28:1)[ ,2])

sample_plots <- 1:4 %>% map(~ {
  plot_data <- cbind(xy_coord, z = as.vector(t(x_test_test[.x,,,1])))
  ggplot(plot_data, aes(x, y)) +
    geom_raster(aes(fill = z)) +
    guides(fill = FALSE) +
    theme_void() +
    ggtitle(paste(((.x - 1) * 90) %% 360, "degree rotation"))
})

#jpeg('weekday//img//mnist.jpg', width = 480, height = 480)
do.call("grid.arrange", c(sample_plots, ncol = 2, nrow = 2))
#dev.off()
