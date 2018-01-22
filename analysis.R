#exploratory analysis
data=read.csv("weather_diag.csv", header=TRUE, ",")
diagnosis <- data[,2]
weather <- data[,3:16]
dates <- data[,1]
names <- variable.names(data)[3:16]
names.n <- c("CBL pressure", "Wind direction","Highest Gust", "Max Air Temp", "Highest mean wind speed", "Min Air Temp", "PET", "Precipitation","SMD","SMD mod","SMD poor","Soil Temp","Sun duration","Mean Wind Speed")

diagnosis <- data.matrix(diagnosis)
weather <- data.matrix(weather)
dates <- as.matrix(dates)
variables <- cbind(diagnosis,weather)

postiveDiagnosis = variables[which(diagnosis[,1]==1)]
negativeDiagnosis = variables[which(diagnosis[,1]==0)]

plot(diagnosis)

#Analysis of Observations 55 and 69
means= apply(X = weather,MARGIN = 2, FUN=mean)
sd=apply(X = weather,MARGIN = 2, FUN=sd)
weather1 = weather[which(diagnosis==1),]
colnames(weather1)=3:16
weather1[c(55,69),]

barplot(height = weather1[c(55,69),2:14],beside = TRUE, ylim=c(-20,100), xlab="variables", ylab="values")
points(x=c(2,5,8,11,14,17,20,23,26,29,32,35,38),y=means[2:14], pch="-", col=2)
arrows(x0=c(2,5,8,11,14,17,20,23,26,29,32,35,38), x1=c(2,5,8,11,14,17,20,23,26,29,32,35,38), y0=means[2:14]-2*sd[2:14], y1=means[2:14]+2*sd[2:14], angle=90, code = 3, col=2, length = 0.1)

#PCA just on weather
pca <- prcomp(weather, scale. = TRUE, center=TRUE)
summary(pca)
PC1To3 = pca$rotation[,1:3]
plot(pca, type="l", main="") #take 0.72 so 3PC

newpca = predict(pca)
plot(newpca[which(diagnosis[,1]==0),1], newpca[which(diagnosis[,1]==0),2], type="p", xlab="PC1", ylab="PC2", col="grey", cex=0.75, pch=16)
points(newpca[which(diagnosis[,1]==1),1], newpca[which(diagnosis[,1]==1),2], col="black", pch=16, cex=0.75)
legend(x="topright", legend=c("Diagnosis 0", "Diagnosis 1"), col=c("grey","black"), pch=16)

plot(newpca)
pairs(newpca, col=as.factor(diagnosis[,1])) 
new_weather=data.matrix(newpca[which(diagnosis==1),1:3])

#hierarchical clustering
clh = hclust(dist(new_weather, method="euclidean"), method="complete") #hierarchical cluster for the filtered data
plot(clh, cex=0.75, xlab="Number of diagnosis", main="")
abline(h=8.26015, col=2)

clhlabel = cutree(clh, h=mean(clh$height)+3*sd(clh$height))
plot(newpca[,1], newpca[,2], col=clhlabel, pch=clhlabel+15)

#k means clustering
n = nrow(new_weather)
WSS = rep(0,10)
WSS[1]=(n-1)*sum(apply(new_weather,2,var))
for(k in 2:10) {
  WSS[k]=sum(kmeans(new_weather,centers=k)$withinss)
}
plot(WSS,type="b", main="Total of the within sum of squares values versus k")
abline(v=3, lty=2, col=3)

#k=3
k=3
clk3 = kmeans(new_weather,centers=k)
table(clk3$cluster)

plot(new_weather,col=clk3$cluster, main="K-means clustering for k=3 on positive diagnosis data", pch=16)
points(clk3$centers,col=1:k,pch=8,cex=10)

#distance between cluster centroids
distanceBetweenClusterCentroids = dist(clk3$centers, method="euclidean")

#average distance from cluster centroids
averageDistanceFromClusterCentroids = rep(0,3)
for(k in 1:3) {
  g = new_weather[which(clk3$cluster==k),]
  ng = clk3$size[k]
  total = sum(as.matrix(dist(rbind(g,clk3$centers[k,])))[ng+1,])
  averageDistanceFromClusterCentroids[k] = total/ng
}
distanceBetweenClusterCentroids
averageDistanceFromClusterCentroids

#cluster centroids
clk3$centers

#compare two clustering methods
new_new_weather=new_weather[-69,]
new_new_weather=new_new_weather[-55,]
hcl = cutree(hclust(dist(new_new_weather)),3)
icl = kmeans(new_new_weather,centers=3)
tab = table(hcl, icl$cluster)
tab

require(flexclust)
rI = randIndex(hcl,icl$cluster,correct=FALSE)
aRI = randIndex(hcl,icl$cluster)

#logistic regression
lreg=glm(diagnosis ~ newpca[,1:3], family=binomial(logit))
summary(lreg)

colnames(weather)=3:16
lreg1=glm(diagnosis ~ weather, family=binomial(logit))
summary(lreg1)

lreg2=glm(diagnosis ~.*., data=data.frame(newpca[,1:3]), family=binomial(logit))
summary(lreg2)

lreg3=glm(diagnosis ~.*., data=data.frame(weather), family=binomial(logit))
summary(lreg3)
