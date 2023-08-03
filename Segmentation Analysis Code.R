############################
# Segmentation Analysis in R #
##############################

## Install Packages (if needed)
install.packages("plotly") 
library(plotly)
install.packages("ggplot2") 
library(ggplot2) 
install.packages("ggdendro") 
library(ggdendro)

## Load Packages and Set Seed
set.seed(40387286)

# Import Data
data <- read.csv(file.choose()) ## Choose retail_segmentation.csv file



# Run hierarchical clustering with bases variables
data_hclust <- hclust(dist(scale(cbind(data$Food_Quality, data$Beverages, data$Location,data$innovation, data$Quality_of_Service,data$Menu_Design,
                                       data$Prioritize_Hygiene, data$Interior_design, data$Reasonable_Pricing, data$Restaurant_Technology,
                                       data$Brands, data$Staff_behavior, data$avg_order_size, data$avg_order_freq))), method="complete")




plot(data_hclust, labels = NULL, hang= -1,
            main= "Dendogram", sub= NULL,
            xlab = "Distance", ylab= "Height")





# Elbow plot for first 10 segments
x <- c(1:10)
sort_height <- sort(data_hclust$height,decreasing=TRUE)
y <- sort_height[1:10]
plot(x,y); lines(x,y,col="red")


# Run hierarchical clustering with bases variables single linkage
data_hclust <- hclust(dist(scale(cbind(data$Food_Quality, data$Beverages, data$Location,data$innovation, data$Quality_of_Service,data$Menu_Design,
                                       data$Prioritize_Hygiene, data$Interior_design, data$Reasonable_Pricing, data$Restaurant_Technology, data$Brands, data$Staff_behavior, data$avg_order_size, data$avg_order_freq))), method="single")

# Elbow plot for first 10 segments
a <- c(1:10)
sort_height <- sort(data_hclust$height,decreasing=TRUE)
b <- sort_height[1:10]
plot(a,b); lines(a,b,col="blue")


# Run k-means with 3 segments
data_kmeans <- kmeans(x = data.frame(data$Food_Quality, data$Beverages, data$Location,data$innovation, data$Quality_of_Service,data$Menu_Design,
                                     data$Prioritize_Hygiene, data$Interior_design, data$Reasonable_Pricing, data$Restaurant_Technology,
                                     data$Brands, data$Staff_behavior, data$avg_order_size, data$avg_order_freq), 3)





# Add segment number back to original data
segment = data_kmeans$cluster
segmentation_result <- cbind(data, segment)

data_kmeans
data_kmeans$withinss #shows within cluster variation
data_kmeans$tot.withinss #shows total within cluster variation
# Export data to a CSV file
write.csv(segmentation_result, "/Users/user/seg_result.csv",row.names = FALSE)

