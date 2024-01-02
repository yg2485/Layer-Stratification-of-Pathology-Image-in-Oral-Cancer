
## install.packages("devtools")
devtools::install_github("estfernandez/SAFARI")

install.packages("BiocManager")
BiocManager::install("EBImage", force = TRUE)

library(SAFARI)
?read.image           # read and preprocess reconstructed image
?binary.segmentation  # segmentation procedure
?compute.features     # feature extraction for an individual ROI
?rc.plot              # visualize binary or segmented images



img_1 <- read.image("/Users/yanghongguo/Downloads/example4.png")
seg <- binary.segmentation(img_1, c("NLST", "AA00474", "11030"))
unique_values <- unique(c(seg$regions))



# load sample
data("rBPS")

# segmentation procedure
rBPS <- binary.segmentation(
  rBPS,
  id = c("NLST", "AA00474", "11030"),
  filter = 150,
  categories = c("geometric", "boundary", "topological")
)








pic_3 <- read.csv("/Users/yanghongguo/Desktop/Research/Stratification/location2_region.csv")
plot(pic_3[,1:2], pch = 20, cex = 0.2)
pic_3 <- pic_3[!duplicated(pic_3[,1:2]),] # remove all duplicate points


mainDir <- "~/Desktop/Oral Cancer"
setwd(mainDir)
subDir <- "tumor_2"

if (file.exists(subDir)){
  # unlink(paste0(subDir, "/*"), force = TRUE)
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

for (i in 1: length(unique(pic_3[,3]))){
  # save images
  png(filename = paste0("region_", i,  ".png"))
  
  plot(pic_3[,1:2], pch = 20, cex = 0.2, col = ifelse(pic_3[,3] == i, "red", "blue"))
  dev.off()
}




sum(duplicated(pic_3[,1:2]))

pic_3[pic_3[,1] == 55 & pic_3[,2] == 348,]
