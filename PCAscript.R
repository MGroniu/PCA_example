# When we got our data set there is high probability that we will
# have to reduce dimensions. I will use Principal Component Analysis
# to show example how to reduce dimensions of the data but still 
# contain the most information of the previous data. The whole data set has
# 249 observations and 13 variables. For purpose in this paper I will 
# use 4 variables in terms of psychical strength of the creature.



#Load libraries
library(factoextra)
library(ClusterR)
library(factoextra)
library(corrplot)

#Selecting columns in terms mentioned above
digimons<-read.csv('DigiDB_digimonlist.csv',header=TRUE,sep=",")
digimons<-digimons[,c(8,10,11,13)]

#Quick summary of the data
summary(digimons)
head(digimons)


# Before using principal component analysis we can 
# check correlation between variables


#Plot for better visualization
corrplot(cor(digimons), method="shade")



# As we can see correlation is pretty strong between variables.
# In the next step we will use principal component analysis algorithm but
# before that we have to use internalstandarization. If we put argument 
# center=TRUE and scale.=TRUE in prcomp() function our data will be standarized.




digimons.pca<-prcomp(digimons,center=TRUE, scale.=TRUE) 
digimons.pca
digimons.pca$rotation
#If we consider rotations of PC1 we can say that:
#Lv.50.HP, Lv50.Atk, Lv50.Def and Lv50.Spd are directly related
#Summary of digimons principal compononents
summary(digimons.pca)

# If we look at Cumulative Proportion we can say that
# PC1 and PC2 explain almost 80% of total variance of the data




#PCA individual results
# Below there are shown the results of Principal Component Analysis for each observation


ind<- get_pca_ind(digimons.pca)
#We see three names
print(ind)

#"$coord" Coordinates for the individuals
head(ind$coord) 

#"$cos2" it tell us how important is principal component for a given observation
head(ind$cos2)

#It show us contribution of a given observation to the principal component
head(ind$contrib) 



#PCA visualization
# Visualizations can help us to choose the number of principal components and verify our decision of taking two principal components.

fviz_eig(digimons.pca)


# Using elbow method and by looking at first two bars we can we see that choosing two principal components was a right idea because they graphically show that almost 80% of total variance of the data is exaplained by them.


#Other Plots
#This plot show us observations on plot with two variables#
fviz_pca_ind(digimons.pca, col.ind = "#00AFBB", repel = TRUE)


# The plot has principal axes which represent the first and the second PC scores.
# The observations are plotted on that basis. We can also see the number of the observation.
#Plot shows importance of component to the variable
fviz_pca_ind(digimons.pca, col.ind="cos2", geom = "point", gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))


# Components which has big value of cos2 are contributing a relatively big portion to the total distance
# and because of that we know the importance of a principal component for a given observation.

#Correlation circle
#This plot shows us variables correlation
fviz_pca_var(digimons.pca, col.var = "black")


# This plot shows how variables are correlated.
# We see that all four variables are positively correlated because they are grouped together.

#Conclusions
# Principal Component Analysis help us with dimension reduction in many cases.
# On this example I wanted to show that even on small data we can provide dimension reduction. In our case we replaced our 4 variables with two principal components which explain almost 80% of total variance of the data
# We also have to remember that PCA method is very sensitive to the outliers.


