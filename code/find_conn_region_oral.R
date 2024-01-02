
library(tiff)
library(png)
library(magick)
# Download and read sample image (readJPEG doesn't work with urls)
# img <- readTIFF("/Users/yanghongguo/Desktop/EPOC premalignant trial-imaging AI study-015_6688x14560_40X.png")

# If want to reduce the resolution, write the image to a file with 300 ppi resolution
# Resize the image to 1000 x 1000 pixels

# Image 1
img <- png::readPNG("/Users/yanghongguo/Desktop/EPOC premalignant trial-imaging AI study-017-1_21600x8416_40X.png")

# Image 2
img <- png::readPNG("/Users/yanghongguo/Desktop/EPOC premalignant trial-imaging AI study-019_67072x54880_40X.png")

# Image 3
img <- png::readPNG("/Users/yanghongguo/Desktop/EPOC premalignant trial-imaging AI study-015_6688x14560_40X.png")

img <- round(img, 3)
unique(matrix(img, ncol=3, byrow=TRUE))
nrow(unique(matrix(img, ncol=3, byrow=TRUE))) # how many different colors

# store results
re <- matrix(-1, nrow = dim(img)[1], ncol = dim(img)[2])

n_row = dim(img)[1]
n_col = dim(img)[2]

# # color of cell types
# cell_type <- c('normal', 'tumor', 'stroma', 'lymphocyte', 'macrophage', 'blood', 'necrosis')
color_tumor <- c(1, 0, 0)
# 
# color_list <- NULL
# color_list[[1]] <- round(c(144,   0, 255)/255, 3)
# color_list[[2]] <- round(c(0, 255,   0)/255, 3)
# color_list[[3]] <- round(c(255,   0,   0)/255, 3)
# color_list[[4]] <- round(c(0,   0, 255)/255, 3)
# color_list[[5]] <- round(c(255, 255,   0)/255, 3)
# color_list[[6]] <- round(c(255,   0, 255)/255, 3)
# color_list[[7]] <- round(c(0, 148, 225)/255, 3)

# compare pixel color with color of cell types
color_compare <- function(c1, c2){
  if (c1[1] == c2[1]){
    if (c1[2] == c2[2]){
      if (c1[3] == c2[3]){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

dd <- list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))

# bfs function
bfs <- function(k, now_x, now_y){
  re[now_x, now_y] <<- k
  iii <- (now_x - 1) * n_col + now_y - 1
  temp <- c(iii)
  re_list <- c(iii)
  
  while (length(temp) > 0){
    temp_idx <- temp[1]
    temp_x <- floor(temp_idx / n_col) + 1
    temp_y <- temp_idx %% n_col + 1
    temp <- temp[-1]
    for (d_idx in 1:4){
      new_x <- temp_x + dd[[d_idx]][1]
      new_y <- temp_y + dd[[d_idx]][2]
      if (new_x <= n_row & new_x >= 1 & new_y <= n_col & new_y >= 1){
        if (re[new_x, new_y] == -1){
          if (color_compare(img[new_x, new_y, ], color_tumor)){
            re[new_x, new_y] <<- k
            new_idx <- (new_x - 1) * n_col + new_y - 1
            temp <- c(temp, new_idx)
            re_list <- c(re_list, new_idx)}
        }
      }
    }
  }
  return(re_list)
}


# search whole image   
k <- 1
# cell_list <- NULL
cell_locations <- list()

for (i in 1:n_row){
  for (j in 1:n_col){
    if (re[i, j] == -1){
      if (color_compare(img[i, j, ], color_tumor)){
        re_list <- bfs(k, i, j)
        # cell_list <- c(cell_list,  cell_type[idx])
        cell_locations[[k]] <- re_list
        k <- k + 1
      }
    }
    if (re[i, j] == -1){
      re[i, j] <- 0
    }
  }
  if (i %% 100 == 0){
    print(i)
  }
}


# store results
n_cell <- max(re)
# cell_list <- data.frame(cell_idx = 1:n_cell)
cell_res <- matrix(NA, nrow = 0, ncol = 3)

print(n_cell)
for (i in 1:n_cell){
  cell_idxs_list <- cell_locations[[i]]
  cell_idxs <- matrix(0, nrow = length(cell_idxs_list), ncol = 3)
  cell_idxs[ ,1] <- floor(cell_idxs_list / n_col) + 1
  cell_idxs[ ,2] <- cell_idxs_list %% n_col + 1
  cell_idxs[ ,3] <- rep(i, length(cell_idxs_list))
  cell_res <- rbind(cell_res, cell_idxs)
}


plot(cell_res[,1:2], pch = 20, cex = 0.2)
cell_res <- data.frame(cell_res)
colnames(cell_res) <- c("x","y","region")
write_csv(cell_res, "location_3_full.csv")

# alpha shape
alpha <- 8
edge <- ashape(cell_res[,1], cell_res[,2], alpha = alpha)
ee <- edge$edges
plot(cell_res[,1:2], pch = 20, cex = 0.2, panel.last = c(text(500, 3000, sprintf("alpha = %d", alpha), cex = 0.8)))
segments(cell_res[ee[,1],1], cell_res[ee[,1],2], cell_res[ee[,2],1], cell_res[ee[,2],2], col= 'red', lwd= 2)



# # find cell center
# cell_list$x <- 0
# cell_list$y <- 0
# for (i in 1:n_cell){
#   cell_idxs_list <- cell_locations[[i]]
#   cell_idxs <- matrix(0, nrow = length(cell_idxs_list), ncol = 2)
#   cell_idxs[ ,1] <- floor(cell_idxs_list / n_col) + 1
#   cell_idxs[ ,2] <- cell_idxs_list %% n_col + 1
#   cell_list$x[i] <- round(mean(cell_idxs[, 1]))
#   cell_list$y[i] <- round(mean(cell_idxs[, 2]))
#   
#   if (i %% 100 == 0){
#     print(i)
#   }
# }
# 
# library(ggplot2)
# library(ggpubr)
# 
# x_range <- 4000
# y_range <- 4000
# 
# ggplot(cell_list, aes(y, 2001 - x, color = type)) +
#   background_image(img) + 
#   coord_fixed(ratio = 1, xlim = c(0, x_range), ylim = c(0, y_range), expand = FALSE) +
#   geom_point(size = 0.5) + scale_color_manual(values=c('stroma' = "#87CEFA", 'blood' = "#720000",'lymphocyte' = '#EE82EE', 'tumor' = "#40826d",  'necrosis' = "#FF8000",  'ductal epithelium' = '#645452', 'macrophage' = '#6C71C4')) + 
#   theme(axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none")
# 
# # plot re matrix
# re_new <- re
# re_new[re_new != 0] <- 1
# 
# library(lattice)
# levelplot(re_new)

save(re, cell_list, file = 'breast_cancer_cell_information_from_image_example.RData')

