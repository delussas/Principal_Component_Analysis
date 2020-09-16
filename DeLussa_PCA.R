# Scott DeLussa
# PCA on FIFA dataset
# 7/6/2020

setwd("~/Documents/Stockton Graduate/Machine_Learning/PCA")

library(tidyverse)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

# Read in the Soccer dataset
df <- read.csv("data.csv", header = TRUE, sep = (","))
# Get an idea of what the structure of the data is
glimpse(df)
# I noticed the many positions, so I wanted to see the individual positions
unique(df$Position)
# I used the names function to fiqure out the index of each column
names(df)

# After looking at the data, I am curious what skills attribute make for the best players

# This creates a new column, pos, which condenses the positions for each player into general categories
pos <- as.factor(df$Position)
levels(pos) <- list(Goalie  = c("GK"), 
                    Defender = c("CB","RB","LB","RWB","LWB"), 
                    Midfielder = c("CDM","CM","RM","LM","CAM"), 
                    Forward = c("CF","RW","LW","ST"))
df <- mutate(df, pos)
names(df)

# I removed the data that I didnt find necessary for the PCA
sub_data <- subset(df, select = c(4,6,8,10,12,13,15,17:20,24,27:28,55:88,90))
glimpse(sub_data)
sum(is.na(sub_data))
drop_data <- drop_na(sub_data)
sum(is.na(drop_data))

# This converts the factors into integers, since PCA requires the data to be numeric
must_convert <- sapply(drop_data,is.factor)
mc2 <- sapply(drop_data[,must_convert], unclass)
soccer <- cbind(drop_data[,!must_convert],mc2)
glimpse(soccer)
names(soccer)

# Creates a correlation matrix
correlation_matrix <- cor(soccer, use = "complete.obs")
correlation_matrix
# Illustrates the correlation matrix
ggcorrplot(correlation_matrix)
# Removes the redundancy of the above matrix
corrplot_clustered <- ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower")
corrplot_clustered

# My first PCA using R's built in prcomp function. The data is centered to 0 based on the variable means, in other words, the mean of each column, excluding any missing data, is subtracted from the column. Then, scale is used where each column (after centering) is divided by the square root of sum-of-squares (after centering) over n - 1, where n is the number of non-missing values. Scaling the data ensures different units of measure do not distort the data. All in all, the data retains its geographic shape prior to centering and scaleing, it it just condenced for ease of calculations
soccer_pca <- prcomp(soccer, center = TRUE, scale. = TRUE)

# Quickly view the details of the PCA
summary(soccer_pca)

# The Screeplot is used to understand the percent of variance per principal component. This screeplot shows the first few PC's alone account for a considerable amount of the variance
fviz_screeplot(soccer_pca)
# The below two funcitons explain the eigenvalues per dimension. This tells what percent of the variance each dimension makes up
get_eigenvalue(soccer_pca)
# Since the above graph levels off at the 6th dimension, the below graph is a more detailed visual, which adds the percentages to each dimension, as well as limits it the number of dimensions to 5 using ncp
fviz_eig(soccer_pca, addlabels = T, ncp = 5)

# Creates a factor map using fviz_pca_var() for the variables with cos2 higher than 0.75. The squared cosign or cos2, shows how accurate the representation of your variables or individuals on the PC plane is
fviz_pca_var(soccer_pca, col.var = "cos2", select.var = list(cos2 = .75), gradient.cols = c("blue", "green", "red"), repel = TRUE)
# Creates a factor map for the top 5 variables with the highest contributions. Given this factor map, player performance seems to be determined based on Positioning, Dribbling, Ball Control, Standing Tackle, and Interceptions
fviz_pca_var(soccer_pca, select.var = list(contrib = 5), repel = T)

# Creates a barplot for the 10 variables with the highest cos2 on the 1st PC then again on the 2nd PC
fviz_cos2(soccer_pca, choice = "var", axes = 1, top = 10)
fviz_cos2(soccer_pca, choice = "var", axes = 2, top = 10)

# Creates a barplot of the 5 variables with the highest contributions on the 1st and 2nd PCs respectively. PC1's contributions are pretty insignificant, with the first two being just over 4%. PC2 on the other hand has 4 contributions over 10%
fviz_contrib(soccer_pca, choice = "var", axes = 1, top = 5)
fviz_contrib(soccer_pca, choice = "var", axes = 2, top = 5)

# Biplots are graphs that provide a compact way of summarizing the relationships between individuals, variables, and also between variables and individuals within the same plot
fviz_pca_biplot(soccer_pca)
# Added Ellipses to the biplot to which are added within the individuals factors' map to group individuals that resemble each other in the coordinate system of the principal components. The habillage argument, determines how the ellipses will be colored.
fviz_pca_biplot(soccer_pca, geom.ind = "point", habillage = soccer$pos, addEllipses = T)
# Here, the general positions created above, seem to be decent identifiers of groups. The Golie is clearly seperated, and so is the Forwards from the Defenders. The Midfielders are blended between the Forwards and Defenders as one would expect.

# I used the names function for numerical indexing
names(soccer)
# Created a subset of the data that disreguards goalies, in an attempt to make a more accurate PCA
field_players <- subset(soccer, select = -c(34:38))
names(field_players)
# A quick analysis proves to be less accurate
field_pca <- prcomp(field_players, center = T, scale. = T)
summary(field_pca)
fviz_screeplot(field_pca)
fviz_eig(field_pca, addlabels = T, ncp = 5)
fviz_pca_biplot(field_pca)
fviz_contrib(field_pca, choice = "var", axes = 1, top = 5)
fviz_contrib(field_pca, choice = "var", axes = 2, top = 5)
# These results are similar to that of soccer_pca. The percent of explained variance for the field players is slightly less per dimension compared to the soccer_pca. The contributions for dimension one and two are very similar for both the soccer_pca and the field_pca.  

# Again used for indexing
names(soccer)
# In another attempt to be more accurate, now I just focused on the goalie
goalie_data <- subset(soccer, select = c(1:3,34:49))
names(goalie_data)
# This is even less accurate than the field players
goalie_pca <- prcomp(goalie_data, center = T, scale. = T)
summary(goalie_pca)
fviz_screeplot(goalie_pca)

# It seems that the less data, the more poorly the PCA performs, so I now did a quick PCA on the largest possible subset of the orginal dataset. Here, the only columns excluded consisted of qualitative data like the players name, ID, face, etc.
broad_data <- subset(df, select = -c(1:3,5,7,10))
broad_data <- drop_na(broad_data)
sum(is.na(broad_data))
must_convert2 <- sapply(broad_data,is.factor)
mc3 <- sapply(broad_data[,must_convert2], unclass)
broad_soccer <- cbind(broad_data[,!must_convert2],mc3)
# This PCA has proved to be more accurate based on the proportion of variance per dimension
broad_pca <- prcomp(broad_soccer, center = T, scale. = T)
summary(broad_pca)
fviz_screeplot(broad_pca, type = "line")
fviz_pca_biplot(broad_pca)
fviz_pca_ind(broad_pca)

# Because it is evident that this PCA is better than both the field players and the goalie data, I performed a more in depth PCA
get_eigenvalue(broad_pca)
fviz_eig(broad_pca, addlabels = T, ncp = 5)
fviz_pca_var(broad_pca, col.var = "cos2", select.var = list(cos2 = .75), gradient.cols = c("blue", "green", "red"), repel = T)
# Again, PC 1 has small dominate contributions compared to PC 2
fviz_contrib(broad_pca, choice = "var", axes = 1, top = 5)
fviz_contrib(broad_pca, choice = "var", axes = 2, top = 5)
fviz_pca_var(broad_pca, select.var = list(contrib = 5), repel = T)


# After I completed the assignment, I was watching a PCA video where the person transposed their data, then ran the PCA. I was curious what effect this would have on my data so I ran it as well. Interestingly, after running it on the soccer data subset I created, PC1 accounted for an incredible 89.9% of the explained variance. When I looked into what was the dominate contributions for PC 1, I was unsure of what the dimensions represented. Plus, the greatest dimensions represented less than 3%. PC2 accounted for 7.2% of the explained variance, and its dominate dimension being the Club the person played for, accounted for over 80% of that dimension. I'm not sure if this model is truly accurate, or if there is some inaccuracies to this.

transposed <- prcomp(t(soccer))
summary(transposed)
fviz_eig(transposed, addlabels = T, ncp = 5)
fviz_pca_biplot(transposed)
fviz_pca_biplot(transposed, geom.ind = "point", addEllipses = T)
fviz_pca_var(transposed)
fviz_pca_var(transposed, select.var = list(contrib = 5), repel = T)
fviz_contrib(transposed, choice = "var", axes = 1, top = 5)
fviz_contrib(transposed, choice = "ind", axes = 2, top = 5)

# One thing I noticed was the difference in results between screeplot() and fviz_screeplot(). I am not sure why they return different results ie:
screeplot(soccer_pca)
fviz_screeplot(soccer_pca)
# After researching it, they both seem to plot the eigenvalues (variances) against the number of dimensions

#References:

# prcomp explained including center / scale https://docs.tibco.com/pub/enterprise-runtime-for-R/5.0.0/doc/html/Language_Reference/stats/prcomp.html  

# DataCamp Multivariate Probability Distributions in R https://campus.datacamp.com/courses/multivariate-probability-distributions-in-r/principal-component-analysis-and-multidimensional-scaling?ex=2

# DataCamp Dimensionality Reduction in R https://learn.datacamp.com/courses/dimensionality-reduction-in-r

# Definitions http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining

# Position conversion https://github.com/aditya14/data-science-portfolio/blob/master/PCA/PCA.R 

# Referenced to convert categorical data to numerical data, https://stackoverflow.com/questions/47922184/convert-categorical-variables-to-numeric-in-r
