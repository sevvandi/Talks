library(ggplot2)
library(outlierensembles)
library(DDoutlier)
library(pROC)
library(ROCR)

set.seed(1)
X <- data.frame(x1 = rnorm(100), x2 = rnorm(100), label = "Normal")
oo <- data.frame(x1 = rnorm(5, mean = 2), x2 = rnorm(5, mean = 2), label = "Anomaly" )

X <- rbind.data.frame(X, oo)

ggplot(X, aes(x1, x2)) + geom_point(aes(color=label))


mems <- HDoutliers::getHDmembers(X)
hdout <- HDoutliers::getHDoutliers(X, mems)
Method1 <- rep("Normal", 105)
Method1[hdout] <- "Anomaly"


ggplot(X, aes(x1, x2)) + geom_point(aes(color=Method1))

knndist <- DDoutlier::KNN_AGG(X[ ,1:2])
ggplot(X, aes(x1, x2)) + geom_point(aes(color=knndist))


lobj <- lookout::lookout(X[ ,1:2], alpha = 0.1)
lobj$outlier_scores

ggplot(X, aes(x1, x2)) + geom_point(aes(color=lobj$outlier_scores))


# rocobj <- pROC::roc(X$label, knndist)
# plot(rocobj, xlab = "False Positive Rate", ylab = "True Positive Rate")

library(ROCR)
pred <- prediction(knndist, c(rep(0, 100), rep(1,5)))
perf <- performance(pred,"tpr","fpr")
plot(perf)

 # compare anomaly detection methods
set.seed(1)
r1 <-runif(805)
r2 <-rnorm(805, mean=5)
theta = 2*pi*r1;
R1 <- 2
R2 <- 2
dist = r2+R2;
x =  dist * cos(theta)
y =  dist * sin(theta)

X <- data.frame(
  x1 = x,
  x2 = y
)
labs <- c(rep(0,800), rep(1,5))
nn <- dim(X)[1]
knn_auc <- lof_auc <- cof_auc <- rep(0, 10)
for(i in 1:10){
  mu <-  5 - (i-1)*0.5
  z <- cbind(rnorm(5,mu, sd=0.2), rnorm(5,0, sd=0.2))
  X[801:805, 1:2] <- z

  # Outlier Detection Methods
  knn_scores <- DDoutlier::KNN_AGG(X)
  lof_scores <- DDoutlier::LOF(X)
  cof_scores <- DDoutlier::COF(X)

  # Area Under ROC = AUC values
  # KNN
  roc_obj <- pROC::roc(labs, knn_scores, direction ="<")
  knn_auc[i] <- roc_obj$auc

  # LOF
  roc_obj <- pROC::roc(labs, lof_scores, direction ="<")
  lof_auc[i] <- roc_obj$auc

  # COF
  roc_obj <- pROC::roc(labs, cof_scores, direction ="<")
  cof_auc[i] <- roc_obj$auc
}
X <- cbind.data.frame(X, labs)
# Plot of points in the last iteration
ggplot(X, aes(x1, x2, col= label)) + geom_point()

set.seed(1)
r1 <-runif(805)
r2 <-rnorm(805, mean=5)
theta = 2*pi*r1;
R1 <- 2
R2 <- 2
dist = r2+R2;
x =  dist * cos(theta)
y =  dist * sin(theta)
X <- data.frame(
  x1 = x,
  x2 = y
)
mu <-  5 - (5-1)*0.5
z <- cbind(rnorm(5,mu, sd=0.2), rnorm(5,0, sd=0.2))
X[801:805, 1:2] <- z
label <- c(rep("Normal", 800), rep("Anomaly", 5)  )
ggplot(X, aes(x1, x2, col=label)) + geom_point()

df <- data.frame(Iteration=1:10, KNN=knn_auc, LOF=lof_auc, COF=cof_auc)
dfl <- tidyr::pivot_longer(df, 2:4)
colnames(dfl)[2:3] <- c("Method", "AUC")
ggplot(dfl, aes(x=Iteration, y=AUC, color=Method)) + geom_point() + geom_line() + scale_x_continuous(breaks=1:10) + theme_bw()



### -Downsampling Process
set.seed(1)
df1 <- data.frame(x=rnorm(100), y=rnorm(100, mean=1))
df2 <- data.frame(x=rnorm(50, mean=1), y=rnorm(50, mean=2))
labs <- c(rep("A", 100), rep("B", 50))
df3 <- rbind.data.frame(df1, df2)
df <- cbind.data.frame(df3, labs)

g1 <- ggplot(df, aes(x,y)) + geom_point(aes(color=labs, shape = labs )) + theme_bw() + theme(legend.position = "none")

# NOT SO GREAT DOWNSAMPLING
s1 <- which((df2$x <1) &(df2$y <2.5))
s1 <- sample(s1, 5)
df4 <- rbind.data.frame(df1, df2[s1, ])
labs <- c(rep("A", 100), rep("B", 50)[s1])
df <- cbind.data.frame(df4, labs)
g2 <- ggplot(df, aes(x,y)) + geom_point(aes(color=labs, shape = labs)) + theme_bw() + theme(legend.position = "none")


# GOOD DOWNSAMPLING
s1 <- which((df2$x >1.5) &(df2$y >2.5))
df4 <- rbind.data.frame(df1, df2[s1, ])
labs <- c(rep("A", 100), rep("B", 50)[s1])
df <- cbind.data.frame(df4, labs)
g3 <-ggplot(df, aes(x,y)) + geom_point(aes(color=labs, shape = labs)) + theme_bw()

gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(1,1,1.3))
