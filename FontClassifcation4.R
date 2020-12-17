library(dplyr)
MISTRAL <- read.csv("C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/MISTRAL.csv")
BOOK <- read.csv("C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/BOOK.csv")
COMIC <- read.csv("C:/Users/Kevin/Desktop/fall 2020/6350/HW/HW3/COMIC.csv")

nrow(MISTRAL)
nrow(BOOK)
nrow(COMIC)

MISTRAL <- select(MISTRAL, -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
BOOK <- select(BOOK, -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
COMIC <- select(COMIC, -c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))

MISTRAL <- na.omit(MISTRAL)
BOOK <- na.omit(BOOK)
COMIC <- na.omit(COMIC)

MISTRAL <- filter(MISTRAL, strength == 0.4, italic == 0)
BOOK <- filter(BOOK, strength == 0.4, italic == 0)
COMIC <- filter(COMIC, strength == 0.4, italic == 0)

n1 <- nrow(MISTRAL)
n2 <- nrow(BOOK)
n3 <- nrow(COMIC)
Ntotal = n1 + n2 + n3
n1;n2;n3;Ntotal

DATA <- bind_rows(MISTRAL, BOOK, COMIC)
DataCol <- ncol(DATA)

SDATA <- scale(DATA[,4:ncol(DATA)])


km <- kmeans(SDATA, centers = 5, nstart = 50)
km
km$size

library(factoextra)
library(gridExtra)

set.seed(1)
km1 <- kmeans(SDATA, centers = 1, nstart = 50)
kmeans1 <- fviz_cluster(km1, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km2 <- kmeans(SDATA, centers = 2, nstart = 50)
kmeans2 <- fviz_cluster(km2, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km3 <- kmeans(SDATA, centers = 3, nstart = 50)
kmeans3 <- fviz_cluster(km3, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km4 <- kmeans(SDATA, centers = 4, nstart = 50)
kmeans4 <- fviz_cluster(km4, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km5 <- kmeans(SDATA, centers = 5, nstart = 50)
kmeans5 <- fviz_cluster(km5, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())

grid.arrange(kmeans1,kmeans2,kmeans3,kmeans4,kmeans5)

km6 <- kmeans(SDATA, centers = 6, nstart = 50)
kmeans6 <- fviz_cluster(km6, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km7 <- kmeans(SDATA, centers = 7, nstart = 50)
kmeans7 <- fviz_cluster(km7, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km8 <- kmeans(SDATA, centers = 8, nstart = 50)
kmeans8 <- fviz_cluster(km8, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
km9 <- kmeans(SDATA, centers = 9, nstart = 50)
kmeans9 <- fviz_cluster(km9, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
kmx <- kmeans(SDATA, centers = 10, nstart = 50)
kmeansx <- fviz_cluster(kmx, data= SDATA, geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
grid.arrange(kmeans6,kmeans7,kmeans8,kmeans9,kmeansx)


RV = data.frame(matrix(ncol = 1, nrow = 10))
colnames(RV) = c("Perf")
RV$Perf = c(1 - km1$tot.withinss/km1$totss, 1 - km2$tot.withinss/km2$totss,1 - km3$tot.withinss/km3$totss,1 - km4$tot.withinss/km4$totss,1 - km5$tot.withinss/km5$totss,
            1 - km6$tot.withinss/km6$totss,1 - km7$tot.withinss/km7$totss,1 - km8$tot.withinss/km8$totss,1 - km9$tot.withinss/km9$totss,1 - kmx$tot.withinss/kmx$totss
            )

dev.off()
plot(RV$Perf, xlab = "k", ylab = "Reduction of Variance (Perf(k))", main = "K-means perfomance per K")
lines(RV$Perf)

fviz_nbclust(SDATA, kmeans, method = "silhouette")

centers <- km4$centers

pr.out <- prcomp(centers, scale = TRUE, center = TRUE)
centpca <- pr.out$x[,1:3]


centpca <- data.frame(centpca)
p <- plot_ly(TDATA, x =~centpca$PC1, y =~ centpca$PC2, z = centpca$PC3, colors = c('blue','green','red'))
print(p)

SDATA <- cbind(DATA[,1:3],SDATA)

SDATA$cluster = as.factor(km4$cluster)

colfont = as.numeric(as.factor(SDATA$font))
plot3d(centpca, col =SDATA$cluster, mean = "k-means cluster")
plot3d(centpca, col = rainbow(4)[colfont], main = "Actual Fonts")

ClusterTable = with(SDATA, table(cluster,font))
ClusterTable

ClusterSize = apply(ClusterTable,1,sum)
ClusterSize

ClassSize = apply(ClusterTable,2,sum)
ClassSize

CPD = ClusterTable/ClusterSize
CPD

with(SDATA, plot3d(centpca, col =SDATA$cluster, main = "k-means cluster", pos = 1))
bigCLU = ClusterTable[4,]

bigcluster = SDATA$cluster == 4
CluT = SDATA[bigcluster,]
pcacluster = prcomp(CluT[,4:403], scale = TRUE)
colors = c("green","blue","red")
colors = as.numeric(as.factor(CluT$font))
plot3d(pcacluster$x[,1:3], col = rainbow(3)[colors], main = "Cluster 4")

cluster1= SDATA$cluster == 1
cluster2 = SDATA$cluster == 2
cluster3 = SDATA$cluster == 3
cluster4 = SDATA$cluster == 4
Clu1 = SDATA[cluster1,]
Clu2 = SDATA[cluster2,]
Clu3 = SDATA[cluster3,]
Clu4 = SDATA[cluster4,]

install.packages('reldist')
library(reldist)
gini(ClusterTable)

sum(table(Clu1$font))
sum(table(Clu2$font))
sum(table(Clu3$font))
sum(table(Clu4$font))

F <- function(cluster, i) {
  f <- 0
  
  f <- table(cluster$font)[i]/sum(table(cluster$font))
  return(f)
}

gini <- function(F1,F2,F3) {
  gini = F1 * (1 - F1) + F2 * (1 - F2) + F3 * (1 - F3) + F4 * (1 - F4)
  
  return(gini)
}

F1C1 <- F(Clu1,1)
F2C1 <- F(Clu1,2)
F3C1 <- F(Clu1,3)
giniC1 <- as.vector(gini(F1C1, F2C1, F3C1))


F1C2 <- F(Clu2,1)
F2C2 <- F(Clu2,2)
F3C2 <- F(Clu2,3)
giniC2 <- as.vector(gini(F1C2, F2C2, F3C2))

F1C3 <- F(Clu3,1)
F2C3 <- F(Clu3,2)
F3C3 <- F(Clu3,3)
giniC3 <- as.vector(gini(F1C3, F2C3, F3C3))

F1C4 <- F(Clu4,1)
F2C4 <- F(Clu4,2)
F3C4 <- F(Clu4,3)
giniC4 <- as.vector(gini(F1C4, F2C4, F3C4))

Impurity = giniC1 + giniC2 + giniC3 + giniC4

CPD

# A(TOP(1),1) = .44 = COMIC
# A(TOP(2),2) = .41 = BOOK
# A(TOP(3),3) = .35 = COMIC
# A(TOP(4),4) = .48 = MISTRAL


TDATA <- as.data.frame(SDATA)

TDATA$predclass = NA

TDATA$cluster = km4$cluster
TDATA$predclass <- ifelse(TDATA$cluster == 1, "COMIC", TDATA$predclass)
TDATA$predclass <- ifelse(TDATA$cluster == 2, "BOOK", TDATA$predclass)
TDATA$predclass <- ifelse(TDATA$cluster == 3, "COMIC", TDATA$predclass)
TDATA$predclass <- ifelse(TDATA$cluster == 4, "MISTRAL", TDATA$predclass)


TDATA$font <- SDATA$font
CONF <- prop.table(table(TDATA$predclass, TDATA$font))
CONF/rowSums(CONF)



