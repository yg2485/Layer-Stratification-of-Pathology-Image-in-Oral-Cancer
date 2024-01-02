library(alphahull)
library(animation)
library(readr)



#######################################################################################################
#========================================= use provided data =========================================#
#######################################################################################################

#################### Pic21600 ####################
Pic21600 <- read.csv("/Users/yanghongguo/Desktop/cell_summary_EPOC premalignant trial-imaging AI study-017-1_21600x8416_40X.csv",
                     header = TRUE)

location_1 <- Pic21600[,1:3]
plot(location_1[,1:2], pch = as.character(location_1$cell_type), 
     col = location_1$cell_type)

tumor_1 <- location_1[which(location_1$cell_type == 1),]


# alpha shape
alpha <- -10
edge <- ashape(tumor_1[,1], tumor_1[,2], alpha = alpha)
ee <- edge$edges
plot(tumor_1[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(tumor_1[ee[,1],1], tumor_1[ee[,1],2], tumor_1[ee[,2],1], tumor_1[ee[,2],2], col= 'red', lwd= 2)


# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer"
setwd(mainDir)
subDir <- "tumor_1"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(130, 250, 10)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(tumor_1[,1], tumor_1[,2], alpha = alpha)
  ee <- edge$edges
  plot(tumor_1[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(tumor_1[ee[,1],1], tumor_1[ee[,1],2], tumor_1[ee[,2],1], tumor_1[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
  
}

# 细分
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer/tumor_1"
setwd(mainDir)
subDir <- "small_range_alpha"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(150, 180, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(tumor_1[,1], tumor_1[,2], alpha = alpha)
  ee <- edge$edges
  plot(tumor_1[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(tumor_1[ee[,1],1], tumor_1[ee[,1],2], tumor_1[ee[,2],1], tumor_1[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}


#################### Pic67072 ####################
Pic67072 <- read.csv("/Users/yanghongguo/Desktop/cell_summary_EPOC premalignant trial-imaging AI study-019_67072x54880_40X.csv",
                     header = TRUE)

location_2 <- Pic67072[,1:3]
plot(location_2[,1:2], pch = as.character(location_2$cell_type), 
     col = location_2$cell_type)

tumor_2 <- location_2[which(location_2$cell_type == 1),]

# alpha shape
alpha <- 110
edge <- ashape(tumor_2[,1], tumor_2[,2], alpha = alpha)
ee <- edge$edges
plot(tumor_2[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 500, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(tumor_2[ee[,1],1], tumor_2[ee[,1],2], tumor_2[ee[,2],1], tumor_2[ee[,2],2], col= 'red', lwd= 2)


# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer"
setwd(mainDir)
subDir <- "tumor_2"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(90, 200, 10)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(tumor_2[,1], tumor_2[,2], alpha = alpha)
  ee <- edge$edges
  plot(tumor_2[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 500, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(tumor_2[ee[,1],1], tumor_2[ee[,1],2], tumor_2[ee[,2],1], tumor_2[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
  
}

# 细分
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer/tumor_2"
setwd(mainDir)
subDir <- "small_range_alpha"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(110, 150, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(tumor_2[,1], tumor_2[,2], alpha = alpha)
  ee <- edge$edges
  plot(tumor_2[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 500, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(tumor_2[ee[,1],1], tumor_2[ee[,1],2], tumor_2[ee[,2],1], tumor_2[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}


#################### Pic6688 ####################
Pic6688 <- read.csv("/Users/yanghongguo/Desktop/cell_summary_EPOC premalignant trial-imaging AI study-015_6688x14560_40X.csv",
                     header = TRUE)
location_3 <- Pic6688[,1:3]
plot(location_3[,1:2], pch = as.character(location_3$cell_type), 
     col = location_3$cell_type)

tumor_3 <- location_3[which(location_3$cell_type == 1),]

# alpha shape
alpha <- 120
edge <- ashape(tumor_3[,1], tumor_3[,2], alpha = alpha)
ee <- edge$edges
plot(tumor_3[,1:2], pch = 20, cex = 0.2, panel.last = c(text(3500, 1800, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(tumor_3[ee[,1],1], tumor_3[ee[,1],2], tumor_3[ee[,2],1], tumor_3[ee[,2],2], col= 'red', lwd= 2)


# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer"
setwd(mainDir)
subDir <- "tumor_3"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(110, 230, 10)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(tumor_3[,1], tumor_3[,2], alpha = alpha)
  ee <- edge$edges
  plot(tumor_3[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(tumor_3[ee[,1],1], tumor_3[ee[,1],2], tumor_3[ee[,2],1], tumor_3[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
  
}

# 细分
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer/tumor_3"
setwd(mainDir)
subDir <- "small_range_alpha"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(120, 160, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(tumor_3[,1], tumor_3[,2], alpha = alpha)
  ee <- edge$edges
  plot(tumor_3[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(tumor_3[ee[,1],1], tumor_3[ee[,1],2], tumor_3[ee[,2],1], tumor_3[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}




#######################################################################################################
#========================================= use manual data =========================================#
#######################################################################################################

#################### Pic21600 ####################
location_11 <- read.csv("/Users/yanghongguo/Desktop/Research/Stratification/Oral Cancer 2/location1.csv")
plot(location_11, pch = 20, cex = 0.2, col = "red")

# alpha shape
alpha <- 10
edge <- ashape(location_11[,1], location_11[,2], alpha = alpha)
ee <- edge$edges
plot(location_11[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(location_11[ee[,1],1], location_11[ee[,1],2], location_11[ee[,2],1], location_11[ee[,2],2], col= 'red', lwd= 2)
boundary1 <- as.data.frame(ee[,3:4])
write_csv(boundary1, "boundary1.csv")

# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer 2"
setwd(mainDir)
subDir <- "tumor_1"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(4, 20, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(location_11[,1], location_11[,2], alpha = alpha)
  ee <- edge$edges
  plot(location_11[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(location_11[ee[,1],1], location_11[ee[,1],2], location_11[ee[,2],1], location_11[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}



#################### Pic67072 ####################
location_22 <- read.csv("/Users/yanghongguo/Desktop/Research/Stratification/Oral Cancer 2/location2.csv")
plot(location_22, pch = 20, cex = 0.2, col = "red")

# alpha shape
alpha <- 6
edge <- ashape(location_22[,1], location_22[,2], alpha = alpha)
ee <- edge$edges
plot(location_22[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(location_22[ee[,1],1], location_22[ee[,1],2], location_22[ee[,2],1], location_22[ee[,2],2], col= 'red', lwd= 2)
segments(ee[,3], ee[,4], ee[,5], ee[,6])
boundary2 <- as.data.frame(ee[,3:4])
write_csv(boundary2, "boundary2.csv")

# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer 2"
setwd(mainDir)
subDir <- "tumor_2"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(4, 24, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(location_22[,1], location_22[,2], alpha = alpha)
  ee <- edge$edges
  plot(location_22[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(location_22[ee[,1],1], location_22[ee[,1],2], location_22[ee[,2],1], location_22[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}



#################### Pic6688 ####################
location_33 <- read.csv("/Users/yanghongguo/Desktop/Research/Stratification/Oral Cancer 2/location3.csv")
plot(location_33, pch = 20, cex = 0.2, col = "red")

# alpha shape
alpha <- 10
edge <- ashape(location_33[,1], location_33[,2], alpha = alpha)
ee <- edge$edges
plot(location_33[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(location_33[ee[,1],1], location_33[ee[,1],2], location_33[ee[,2],1], location_33[ee[,2],2], col= 'red', lwd= 2)
boundary3 <- as.data.frame(ee[,3:4])
write_csv(boundary3, "boundary3.csv")

# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer 2"
setwd(mainDir)
subDir <- "tumor_3"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(4, 20, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(location_33[,1], location_33[,2], alpha = alpha)
  ee <- edge$edges
  plot(location_33[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(location_33[ee[,1],1], location_33[ee[,1],2], location_33[ee[,2],1], location_33[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}








#######################################################################################################
#================================= use manual data with assigned region=================================#
#######################################################################################################
setwd("/Users/yanghongguo/Desktop/Research/Stratification")
#################### Pic21600 ####################
location_11 <- read.csv("/Users/yanghongguo/Desktop/Research/Stratification/location1_region.csv")
plot(location_11[,1:2], pch = 20, cex = 0.2, col = "red")
location_11 <- location_11[!duplicated(location_11[,1:2]),]

# alpha shape
alpha <- 10
edge <- ashape(location_11[,1], location_11[,2], alpha = alpha)
ee <- edge$edges
plot(location_11[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(location_11[ee[,1],1], location_11[ee[,1],2], location_11[ee[,2],1], location_11[ee[,2],2], col= 'red', lwd= 2)
boundary1 <- as.data.frame(ee[,3:4])
write_csv(boundary1, "boundary1.csv")

# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer 2"
setwd(mainDir)
subDir <- "tumor_1"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(4, 20, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(location_11[,1], location_11[,2], alpha = alpha)
  ee <- edge$edges
  plot(location_11[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(location_11[ee[,1],1], location_11[ee[,1],2], location_11[ee[,2],1], location_11[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}



#################### Pic67072 ####################
location_22 <- read.csv("/Users/yanghongguo/Desktop/Research/Stratification/location2_region.csv")
plot(location_22[,1:2], pch = 20, cex = 0.2, col = "red")
location_22 <- location_22[!duplicated(location_22[,1:2]),]

# alpha shape
alpha <- 6
edge <- ashape(location_22[,1], location_22[,2], alpha = alpha)
ee <- edge$edges
plot(location_22[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(location_22[ee[,1],1], location_22[ee[,1],2], location_22[ee[,2],1], location_22[ee[,2],2], col= 'red', lwd= 2)
# segments(ee[,3], ee[,4], ee[,5], ee[,6])
boundary2 <- as.data.frame(ee[,3:4])
write_csv(boundary2, "boundary2.csv")

# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer 2"
setwd(mainDir)
subDir <- "tumor_2"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(4, 24, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(location_22[,1], location_22[,2], alpha = alpha)
  ee <- edge$edges
  plot(location_22[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(location_22[ee[,1],1], location_22[ee[,1],2], location_22[ee[,2],1], location_22[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}



#################### Pic6688 ####################
location_33 <- read.csv("/Users/yanghongguo/Desktop/Research/Stratification/location3_region.csv")
plot(location_33[,1:2], pch = 20, cex = 0.2, col = "red")
location_33 <- location_33[!duplicated(location_33[,1:2]),]

# alpha shape
alpha <- 8
edge <- ashape(location_33[,1], location_33[,2], alpha = alpha)
ee <- edge$edges
plot(location_33[,1:2], pch = 20, cex = 0.2, 
     panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(location_33[ee[,1],1], location_33[ee[,1],2], location_33[ee[,2],1], location_33[ee[,2],2], col= 'red', lwd= 2)
boundary3 <- as.data.frame(ee[,3:4])
write_csv(boundary3, "boundary3.csv")

s# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer 2"
setwd(mainDir)
subDir <- "tumor_3"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(4, 20, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(location_33[,1], location_33[,2], alpha = alpha)
  ee <- edge$edges
  plot(location_33[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(location_33[ee[,1],1], location_33[ee[,1],2], location_33[ee[,2],1], location_33[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}






#######################################################################################################
#================================= use connected region from Xi =================================#
#######################################################################################################

setwd("/Users/yanghongguo/Desktop/Research/Stratification")

#################### Pic21600 ####################
location_11 <- read.csv("data/location_1_full.csv")
plot(location_11[,1:2], pch = 20, cex = 0.2, col = "red")
location_11 <- location_11[!duplicated(location_11[,1:2]),]

# alpha shape
alpha <- 100
edge <- ashape(location_11[,1], location_11[,2], alpha = alpha)
ee <- edge$edges
plot(location_11[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(location_11[ee[,1],1], location_11[ee[,1],2], location_11[ee[,2],1], location_11[ee[,2],2], col= 'red', lwd= 2)
boundary1 <- as.data.frame(ee[,3:4])
write_csv(boundary1, "boundary1.csv")

# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer 2"
setwd(mainDir)
subDir <- "tumor_1"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(4, 20, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(location_11[,1], location_11[,2], alpha = alpha)
  ee <- edge$edges
  plot(location_11[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(location_11[ee[,1],1], location_11[ee[,1],2], location_11[ee[,2],1], location_11[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}



#################### Pic67072 ####################
location_22 <- read.csv("data/location_2_full.csv")
plot(location_22[,1:2], pch = 20, cex = 0.2, col = "red")
location_22 <- location_22[!duplicated(location_22[,1:2]),]

# alpha shape
alpha <- 80
edge <- ashape(location_22[,1], location_22[,2], alpha = alpha)
ee <- edge$edges
plot(location_22[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(location_22[ee[,1],1], location_22[ee[,1],2], location_22[ee[,2],1], location_22[ee[,2],2], col= 'red', lwd= 2)
# segments(ee[,3], ee[,4], ee[,5], ee[,6])
boundary2 <- as.data.frame(ee[,3:4])
write_csv(boundary2, "boundary2.csv")

# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer 2"
setwd(mainDir)
subDir <- "tumor_2"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(4, 24, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(location_22[,1], location_22[,2], alpha = alpha)
  ee <- edge$edges
  plot(location_22[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(location_22[ee[,1],1], location_22[ee[,1],2], location_22[ee[,2],1], location_22[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}



#################### Pic6688 ####################
location_33 <- read.csv("location_3_full.csv")
plot(location_33[,1:2], pch = 20, cex = 0.2, col = "red")
location_33 <- location_33[!duplicated(location_33[,1:2]),]

# alpha shape
alpha <- 8
edge <- ashape(location_33[,1], location_33[,2], alpha = alpha)
ee <- edge$edges
plot(location_33[,1:2], pch = 20, cex = 0.2, 
     panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(location_33[ee[,1],1], location_33[ee[,1],2], location_33[ee[,2],1], location_33[ee[,2],2], col= 'red', lwd= 2)
boundary3 <- as.data.frame(ee[,3:4])
write_csv(boundary3, "boundary3.csv")

s# generate plots
mainDir <- "~/Desktop/Research/Stratification/Oral Cancer 2"
setwd(mainDir)
subDir <- "tumor_3"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (alpha in seq(4, 20, 2)){
  
  # save images
  png(filename = paste0("alpha_", alpha,  ".png"))
  
  edge <- ashape(location_33[,1], location_33[,2], alpha = alpha)
  ee <- edge$edges
  plot(location_33[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
  segments(location_33[ee[,1],1], location_33[ee[,1],2], location_33[ee[,2],1], location_33[ee[,2],2], col= 'red', lwd= 2)
  
  dev.off()
}