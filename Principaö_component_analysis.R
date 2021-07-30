########## PCA #################
library(tidyverse)
library(ggplot2)
library(gridExtra)

#### standardizing variable by scaling it
scaled_data_select <- apply(data_select[, c(1, 3:8)], 2, scale)
scaled_data_select

data_select_PCA <- prcomp(data_select[, c(1, 3:8)], scale = TRUE)
names(data_select_PCA)
data_select_PCA$scale  ## standard deviations computed by prcomp()
data_select_PCA$rotation ## PC loadings
data_select_PCA$rotation <- -data_select_PCA$rotation
data_select_PCA$x  ## PC scores
data_select_PCA$x <- -data_select_PCA$x
biplot(data_select_PCA, scale = 0)
(VE <- data_select_PCA$sdev^2) ## variance explain by PC
PVE <- VE/sum(VE)
PVE <- round(PVE, 2)

PVEplot <- qplot(c(1:7), PVE) + geom_line() + xlab("Princiapl component") + ylab("PVE") + ylim(0,1) + ggtitle("Scree plot")

#cumulative PVE plot
cumPVE <- qplot(c(1:7), cumsum(PVE)) + geom_line() + xlab("Principal component") +ylim(0,1)
screeplot <- grid.arrange(PVEplot, cumPVE, ncol = 2)

fviz_screeplot <- fviz_eig(data_select_PCA)

(viz_pca <- fviz_pca_ind(data_select_PCA,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#1b9e77", "#d95f02", "#7570b3"), #c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
))

viz_pca_date <- fviz_pca_ind(PC,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#1b9e77", "#d95f02", "#7570b3"), #c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)


## set Date as rownames
row.names(data_select_) = Complete_Dataset$Date


#PC1 <- as.matrix(data_select_PCA$x)
# Create data frame with Principal Components scores
# PC <- data.frame(Date = row.names(data_select), PC1)
# head(PC)
# # Plot Principal Components for each State
# ggplot(PC, aes(PC1, PC2)) + 
#   modelr::geom_ref_line(h = 0) +
#   modelr::geom_ref_line(v = 0) +
#   geom_text(aes(label = Date), size = 3) +
#   xlab("First Principal Component") + 
#   ylab("Second Principal Component") + 
#   ggtitle("First Two Principal Components of USArrests Data")
