# install.packages("alphahull")
library(alphahull)
library(animation)


################# Simulation: Circle center(0,0), r = 1 #################
points<- matrix(NA, nrow = 100, ncol = 2)
for (i in 1:100){
  new <- runif(2, -1, 1)
  while(((new[1])^2 + (new[2])^2) > 1){
    new <- runif(2, -1, 1)
  }
  points[i,] <- new
}

edge <- ashape(points[,1], points[,2], alpha = 0.5)
ee <- edge$edges
plot(points, pch = 20, cex = 0.2)
segments(points[ee[,1],1], points[ee[,1],2], points[ee[,2],1], points[ee[,2],2], col= 'pink')

plot(edge)

set.seed(123)
animation:::saveMovie({
  par(mfrow = c(1, 2), mar = rep(0, 4), xaxt = "n", yaxt = "n", bg = "black", col = "white")
  for (alpha in seq(1.2, 0, -0.1)) {
    plot(ahull(points[,1], points[,2], alpha = alpha), pch = 2, col = "white", 
         panel.last = c(text(-0.6, 0.95, sprintf("alpha = %.2f", alpha), cex = 1), 
                        text(-0.6, 0.9, "alpha-convex hull", cex = 1)))
    plot(ashape(points[,1], points[,2], alpha = alpha), pch = 2, col = "white", 
         panel.last = c(text(-0.6, 0.95, sprintf("alpha = %.2f", alpha), cex = 1), 
                        text(-0.6, 0.9, "alpha-shape", cex = 1)))
  }
}, moviename = "alpha-shape-plot")




################# Simulation: Ring center(0,0), r = 1 , r = 0.5 #################
points<- matrix(NA, nrow = 100, ncol = 2)
for (i in 1:100){
  new <- runif(2, -1, 1)
  while(((new[1])^2 + (new[2])^2) > 1 | ((new[1])^2 + (new[2])^2) < 0.25){
    new <- runif(2, -1, 1)
  }
  points[i,] <- new
}
plot(points)

set.seed(123)
animation:::saveMovie({
  # layout(mat = matrix(c(1,2), nrow = 1, ncol = 2),
  #        heights = 1, widths = c(1,1))
  par(mfrow = c(1, 2), mar = rep(0, 4), xaxt = "n", yaxt = "n", bg = "black", col = "white")
  for (alpha in seq(1.2, 0, -0.1)) {
    plot(ahull(points[,1], points[,2], alpha = alpha), pch = 2, col = "white", 
         panel.last = c(text(-0.6, 0.95, sprintf("alpha = %.2f", alpha), cex = 1), 
                        text(-0.6, 0.9, "alpha-convex hull", cex = 1)))
    plot(ashape(points[,1], points[,2], alpha = alpha), pch = 2, col = "white", 
         panel.last = c(text(-0.6, 0.95, sprintf("alpha = %.2f", alpha), cex = 1), 
                        text(-0.6, 0.9, "alpha-shape", cex = 1)))
  }
}, moviename = "alpha-shape-plot")




################# Simulation: Ellipse with a = 2, b = 1, equation x^2/4 + y^2 = 1 #################
points<- matrix(NA, nrow = 100, ncol = 2)
for (i in 1:100){
  x <- runif(1, -2, 2)
  y <- runif(1, -1, 1)
  while((x^2/4 + y^2) > 1){
    x <- runif(1, -2, 2)
    y <- runif(1, -1, 1)
  }
  points[i,] <- c(x,y)
}
plot(points)

alpha <- 1.5
set.seed(123)
animation:::saveMovie({
  par(mfrow = c(1, 2), mar = rep(0, 4), xaxt = "n", yaxt = "n", bg = "black", col = "white")
  for (alpha in seq(1.5, 0, -0.1)) {
    plot(ahull(points[,1], points[,2], alpha = alpha), pch = 2, col = "white", 
         panel.last = c(text(-1.5, 0.9, sprintf("alpha = %.2f", alpha), cex = 1), 
                        text(-1.4, 0.8, "alpha-convex hull", cex = 0.8)))
    plot(ashape(points[,1], points[,2], alpha = alpha), pch = 2, col = "white", 
         panel.last = c(text(-1.5, 0.9, sprintf("alpha = %.2f", alpha), cex = 1), 
                        text(-1.5, 0.8, "alpha-shape", cex = 1)))
  }
}, moviename = "alpha-shape-plot")








################# Breast Cancer Data #################

# preprocessing
data <- read.csv("~/Desktop/Research/Stratification/data/human_breast_cancer_connected_regions.csv")[,-1]

data_tumor <- data[data$nucleus_class == "tumor",]

edge <- ashape(data_tumor[,1], data_tumor[,2], alpha = alpha)
ee <- edge$edges
plot(data_tumor[,1:2], pch = 20, cex = 0.2, panel.last = c(text(6000, 21000, sprintf("alpha = %f", alpha), cex = 0.8)))
segments(data_sub[ee[,1],1], data_sub[ee[,1],2], data_sub[ee[,2],1], data_sub[ee[,2],2], col= 'red', lwd= 2)






library(readr)
data_tumor$region <- parse_number(data_tumor$region)
a <- as.data.frame(table(data_tumor$region))
min_points <- 50
region_keep <- a[a$Freq >= min_points, ]
mainDir <- "~/Desktop/Research/Stratification/save_alpha_shape"

for (i in 1:nrow(region_keep)){
  region_n <- region_keep$Var1[i]
  data_sub <- data_tumor[data_tumor$region == region_n, 1:2]
  range_x <- max(data_sub$x) - min(data_sub$x)
  range_y <- max(data_sub$y) - min(data_sub$y)
  
  setwd(mainDir)
  
  subDir <- paste0("tumor_region", region_n)
  
  if (file.exists(subDir)){
    # unlink(paste0(subDir, "/*"), force = TRUE)
    setwd(file.path(mainDir, subDir))
  } else {
    dir.create(file.path(mainDir, subDir))
    setwd(file.path(mainDir, subDir))
  }
  
  for (alpha in seq(max(range_x, range_y)/5, max(range_x, range_y)/50, -max(range_x, range_y)/50)){
    
    # save images
    png(filename = paste0("alpha_", alpha,  ".png"))
    
    
    edge <- ashape(data_sub[,1], data_sub[,2], alpha = alpha)
    ee <- edge$edges
    plot(data_sub[,1:2], pch = 20, cex = 0.2, panel.last = c(text(6000, 21000, sprintf("alpha = %f", alpha), cex = 0.8)))
    segments(data_sub[ee[,1],1], data_sub[ee[,1],2], data_sub[ee[,2],1], data_sub[ee[,2],2], col= 'red', lwd= 2)
    
    dev.off()
  }
  
}

datanew <- unique(data[,1:2])

edge <- ashape(datanew[,1], datanew[,2], alpha = alpha)
ee <- edge$edges
plot(data[,1:2], pch = 20, cex = 0.2, panel.last = c(text(6000, 21000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(datanew[ee[,1],1], datanew[ee[,1],2], datanew[ee[,2],1], datanew[ee[,2],2], col= 'red', lwd= 2)


animation:::saveMovie({
  par(mar = rep(2, 4))
  for (alpha in seq(200, 100, -10)) {
    edge <- ashape(datanew[,1], datanew[,2], alpha = alpha)
    ee <- edge$edges
    plot(data[,1:2], pch = 20, cex = 0.2, panel.last = c(text(6000, 21000, sprintf("alpha = %.2f", alpha), cex = 0.8)))
    segments(datanew[ee[,1],1], datanew[ee[,1],2], datanew[ee[,2],1], datanew[ee[,2],2], col= 'red', lwd= 2.5)
    
  }
}, moviename = "alpha-shape-plot")


