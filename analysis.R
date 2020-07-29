setwd("/Users/mayalewinsohn/Documents/PhD/Bedford_lab/practice/lab_traits")
library(tidyverse)
library(phangorn)
library(ggrepel)
library(ggtree)
library(treeio)
library(rethinking)
data <- read.csv("lab_traits_responses_cleaned_binary.csv")
pca_analysis <- prcomp(t(data[,-1]), scale = TRUE)
pca_analysis <- as.data.frame(pca_analysis$rotation) %>% 
  bind_cols(., data)
ggplot(pca_analysis, aes(x = PC1, y = PC2, label = Name)) + geom_label_repel() + theme_bw() + geom_point()

df <- data[,-1]
rownames(df) <- data$Name
dm <- dist(data[,-1])
# library(dendextend)
# library(ComplexHeatmap)
row_dend = hclust(dist(df)) # row clustering
col_dend = hclust(dist(t(df))) # column clustering
# Heatmap(df, 
#         cluster_rows = color_branches(row_dend, k = 4),
#         cluster_columns = color_branches(col_dend, k = 2)) 
library("pheatmap")
pheatmap(df, legend = FALSE)


data2 <- read.csv("lab_traits_responses_cleaned_encode.csv") 

for (col in 2:ncol(data2)) {

  if(min(data2[,col]) == -1) {
    print(col)
    data2[data2[,col]==0,col] <- 0.5
    data2[data2[,col]==-1,col] <- 0
  }
}

df2 <- data2[,-1]
rownames(df2) <- data2$Name

pheatmap(df2, legend = FALSE)
write.csv(data2,"lab_traits_responses_cleaned_scaled.csv")

pca_analysis <- prcomp(t(data2[,-1]), scale = TRUE)
pca_analysis <- as.data.frame(pca_analysis$rotation) %>% 
  bind_cols(., data)
ggplot(pca_analysis, aes(x = PC1, y = PC2, label = Name)) + geom_label_repel() + geom_point() + theme_bw()


tree_string <- "(((Maya,Trevor),(Marlin, (Thomas, Nicola))),((Jover,Barney),
(Cassia,(((Gytis, Alli, Misja), (Louise, Sydney)),((Kairsten, John), Katie)))));"


tree <- as.treedata(read.tree(text = tree_string ))

ggtree(tree) + geom_tiplab(geom="label") + xlim(NA, 8)


tree_string2 <- "(((Trevor,(John,(Alli, Louise))),((Nicola, Maya),(Gytis, Barney))),((Tom, (Cassia, Misja)),(Sydney,(Marlin,(Jover, Kairsten, Katie)))));"
tree2 <- as.treedata(read.tree(text = tree_string2 ))

ggtree(tree2) + geom_tiplab(geom="label") + xlim(NA, 8)

pheatmap(cor(df2))



data(Kline)
d <- Kline
d$logpop <- log(d$population)
d$society <- 1:10
m23.6 <- map2stan(alist(total_tools <- dpois(mu), 
                         log(mu) <- a + a_society[society] + bp*logpop,
                   a ~ dnorm(0,10),
                   bp ~ dnorm(0,1),
                   a_society[society] ~ dnorm(0, sigma_society), 
                   sigma_society ~ dcauchy(0,1)
  ),
data =d, 
iter = 400, 
chains = 3)

post <- extract.samples(m23.6)

d.pred <- list(
  logpop = seq(from=6, to=14, length.out=30),
  society = rep(1,30)
)
a_society_sims <- rnorm(20000,0,post$sigma_society)
a_society_sims <- matrix(a_society_sims, 2000,10)
link.m23.6 <- link(m23.6, n = 200, data=d.pred, replace= list(a_society=a_society_sims))
plot(d$logpop, d$total_tools, col= rangi2, pch= 16, xlab="log population",  ylab="total tools")

mu.median <- apply(link.m23.6, 2, median)

lines(d.pred$logpop, mu.median)

mu.PI <- apply(link.m23.6, 2, PI, prob=0.97)
shade(mu.PI, d.pred$logpop)

mu.PI <- apply(link.m23.6, 2, PI, prob=0.89)
shade(mu.PI, d.pred$logpop)

mu.PI <- apply(link.m23.6, 2, PI, prob=0.67)
shade(mu.PI, d.pred$logpop)

plot(precis(m23.6 ))
