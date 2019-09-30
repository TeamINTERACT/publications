## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----load libraries and functions, echo=FALSE----------------------------
library("ggplot2")
minMaxNormalization <- function(x){
  return ((x - min(x))/(max(x)-min(x)))
}
normalizeDataFrame <- function(data_frame_no_normalized, features_list){
  data_frame_normalized <- data.frame(data_frame_no_normalized)
  data_frame_colnames <- colnames(data_frame_normalized)
  for (feature_name in features_list){
    if (feature_name %in% data_frame_colnames){
      data_frame_normalized[,feature_name] <- minMaxNormalization(data_frame_normalized[,feature_name])
    }
  }
  return (data_frame_normalized)
}
drawFeatureBoxplot <- function(data, x_col_name, y_col_name, xlab_str, ylab_str){
  p_boxplot <- ggplot(data, aes_string(x=x_col_name, y=y_col_name)) + 
    geom_boxplot(width=0.65) +
    xlab(xlab_str) +
    ylab(ylab_str)+
    theme_bw() +
    theme(axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=12))
  p_boxplot
  return (p_boxplot)
}

## ----load feature csv file, echo=FALSE-----------------------------------
# data_file <- "/Users/ruizhang/Dropbox/dimensionality_activity_space_0326/Features-1.0/results/all_datasets/normalized_features_all_datasets.csv"
no_normalized_data_file <- "/Users/ruizhang/Dropbox/dimensionality_activity_space_0326/Features-1.0/results/all_datasets/no_normalized_features_all_datasets_valid_participants_only.csv"
feature_column_names <- c("dataset","user_id","convex_hull","buffer_area","N5","N10","N15","N20","dim","C1","C2","C3","C4","C5","R.squared")
features_need_to_normalized <- c("convex_hull","buffer_area","N5","N10","N15","N20","dim","C1","C2","C3","C4","C5","R.squared")
data <- read.csv(no_normalized_data_file)
metrics.no_normalized_df <- data[,feature_column_names]
metrics.normalized_df <- normalizeDataFrame(metrics.no_normalized_df, features_need_to_normalized)


## ----summarize of datasets-----------------------------------------------
head(data)

## ----participants distribution in each dataset, echo=FALSE---------------
dataset_ids <- data$dataset
factor_dataset_id <- factor(dataset_ids)
summary(factor_dataset_id)

## ----show metrics.df-----------------------------------------------------
colnames(metrics.normalized_df)

## ----plot r_squared boxplot, echo=FALSE----------------------------------
p_r_squared <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "R.squared","Datasets","R-squared")
p_r_squared

## ------------------------------------------------------------------------
metrics.a_r_squared <- aov(R.squared ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_r_squared)
TukeyHSD(metrics.a_r_squared)

## ----distribution of fitted entropy rate parameters, echo=FALSE----------
p_C1 <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "C1","Datasets","C1")
p_C1

## ------------------------------------------------------------------------
metrics.a_C1 <- aov(C1 ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_C1)
TukeyHSD(metrics.a_C1)

## ----plot C2 boxplot, echo=FALSE-----------------------------------------
p_C2 <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "C2","Datasets","C2")
p_C2

## ------------------------------------------------------------------------
metrics.a_C2 <- aov(C2 ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_C2)
TukeyHSD(metrics.a_C2)

## ----plot C3 boxplot, echo=FALSE-----------------------------------------
p_C3 <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "C3","Datasets","C3")
p_C3

## ------------------------------------------------------------------------
metrics.a_C3 <- aov(C3 ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_C3)
TukeyHSD(metrics.a_C3)

## ----plot C4 boxplot, echo=FALSE-----------------------------------------
p_C4 <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "C4","Datasets","C4")
p_C4

## ------------------------------------------------------------------------
metrics.a_C4 <- aov(C4 ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_C4)
TukeyHSD(metrics.a_C4)

## ----plot C5 boxplot, echo=FALSE-----------------------------------------
p_C5 <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "C5","Datasets","C5")
p_C5

## ------------------------------------------------------------------------
metrics.a_C5 <- aov(C5 ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_C5)
TukeyHSD(metrics.a_C5)

## ------------------------------------------------------------------------
p_convex_hull <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "convex_hull","Datasets","Convex Hull")
p_convex_hull

## ------------------------------------------------------------------------
metrics.a_convex_hull <- aov(convex_hull ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_convex_hull)
TukeyHSD(metrics.a_convex_hull)

## ------------------------------------------------------------------------
p_N5 <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "N5","Datasets","Top 5 visited places Convex Hull")
p_N5

## ------------------------------------------------------------------------
metrics.a_N5 <- aov(N5 ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_N5)
TukeyHSD(metrics.a_N5)

## ------------------------------------------------------------------------
p_N10 <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "N10","Datasets","Top 10 visited places Convex Hull")
p_N10

## ------------------------------------------------------------------------
metrics.a_N10 <- aov(N10 ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_N10)
TukeyHSD(metrics.a_N10)

## ------------------------------------------------------------------------
p_N15 <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "N15","Datasets","Top 15 visited places Convex Hull")
p_N15

## ------------------------------------------------------------------------
metrics.a_N15 <- aov(N15 ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_N15)
TukeyHSD(metrics.a_N15)

## ------------------------------------------------------------------------
p_N20 <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "N20","Datasets","Top 20 visited places Convex Hull")
p_N20

## ------------------------------------------------------------------------
metrics.a_N20 <- aov(N20 ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_N20)
TukeyHSD(metrics.a_N20)

## ------------------------------------------------------------------------
p_buffer_area <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "buffer_area","Datasets","Buffer Area")
p_buffer_area

## ------------------------------------------------------------------------
metrics.a_buffer_area <- aov(buffer_area ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_buffer_area)
TukeyHSD(metrics.a_buffer_area)

## ------------------------------------------------------------------------
p_dim <- drawFeatureBoxplot(metrics.normalized_df, "dataset", "dim","Datasets","Fractal Dimension")
p_dim

## ------------------------------------------------------------------------
metrics.a_dim <- aov(dim ~ dataset, data = metrics.normalized_df) 
summary(metrics.a_dim)
TukeyHSD(metrics.a_dim)

