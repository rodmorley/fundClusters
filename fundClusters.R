##
##
##  fund clusters
##
##
##

library(PerformanceAnalytics)
library(tidyverse)
library(timetk)
library(tidyquant)
library(cluster)
library(factoextra)
load('tmfmgrs.RData')

# load investments / hedge fund data
hfr = read.csv("C:/Users/rmorley/Dropbox/R/rmdata/hfr_hfs.csv", stringsAsFactors = F)
hfr$Date <- as.Date(hfr$Date, format = "%d/%m/%Y")
hfr.xts = xts(hfr[,-1], order.by = hfr[,1])
hfr.xts %>% head()

periodicity(hfr.xts)
periodicity(tmfmgrs)

# combine with own funds
hfr.xts = xts::merge.xts(hfr.xts, tmfmgrs, join = 'inner')

# compute investment statistics (features)
dat <- t(table.Arbitrary(R = hfr.xts,
                         metrics = c(
                           # drawdowns 
                           "AverageDrawdown",
                           "BurkeRatio",
                           "DownsideDeviation",
                           "DrawdownDeviation",
                           "maxDrawdown",
                           "PainRatio",
                           "UlcerIndex",
                           
                           # risk adj performance
                           "CalmarRatio",
                           "HurstIndex",
                           "MartinRatio",
                           "SharpeRatio.annualized",
                           "SortinoRatio",
                           
                           # risk
                           "ETL",
                           "kurtosis",
                           "Omega",
                           "skewness",
                           "SkewnessKurtosisRatio",
                           "StdDev",
                           "VaR",
                           "VolatilitySkewness",
                           
                           # win loss ratio
                           "BernardoLedoitRatio"
                         ),
                         metricsNames = c(
                           # drawdowns
                           "AvgDD",
                           "Burke",
                           "DownsideDev",
                           "DrawdownDev",
                           "maxDD",
                           "PainRatio",
                           "Ulcer",
                           
                           # risk adj performance
                           "Calmar",
                           "Hurst",
                           "Martin",
                           "Sharpe",
                           "Sortino",
                           
                           # risk
                           "ETL",
                           "Kurt",
                           "Omega",
                           "Skew",
                           "SkewKurt",
                           "StdDev",
                           "VaR",
                           "VolSkew",
                           
                           # win loss ratios
                           "BernardoLedoit")
)
)

# compute beta versus hedge fund index and equity index.
hf_beta <-  t(CAPM.beta(hfr.xts, Rb = hfr.xts$HFRI.Fund.of.Funds.Composite.Index))
eq_beta <-  t(CAPM.beta(hfr.xts, Rb = hfr.xts$MSCI.World))
hf_beta

dat <- cbind(dat, eq_beta, hf_beta)

# scale the inputs

dat_scaled <- scale(dat)

# check for NAs in the dataset

sum(!complete.cases(dat_scaled))
dat_scaled %>% as.data.frame %>% vis_dat()

dat_scaled <- dat_scaled[complete.cases(dat_scaled),]
dim(dat_scaled)


# assess the best number of clusters

elbow_results <- vector(mode = 'numeric', length = 10)

for(k in 1:10){
  model = kmeans(dat_scaled, centers = k, nstart = 20)
  elbow_results[k] <- model$tot.withinss
}

plot(1:10, elbow_results, type = 'b',main = "Elbow method\n Total within-cluster sum of square (WSS)")


# kmeans using 6 clusters

kmeans_6_clusters <- kmeans(dat_scaled, centers = 6, nstart = 20)

dat_with_cluster  <- data.frame(dat_scaled, cluster = as.factor(kmeans_6_clusters$cluster))


dat_with_cluster %>% 
  filter(str_detect(tolower(rownames(.)), pattern = "ksc|mlp|kensington|des|mweureka"))


dat_with_cluster %>% 
  group_by(cluster) %>% 
  tally()

dat_with_cluster %>% 
  filter(cluster==4) %>% 
  select(cluster)

dat_with_cluster %>% 
  filter(cluster==6) %>% 
  select(cluster)


# nice
fviz_cluster(kmeans_6_clusters, dat_scaled)
fviz_cluster(kmeans_6_clusters, dat_scaled, ellipse.type = 'norm')

pca = prcomp(t(dat_scaled))
pca
xy = pca$rotation[, c(1,2)]

clusplot(xy,kmeans_6_clusters$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, plotchar=F, main="Investment clusters")

dat_6_scaled %>% 
  filter(cluster==4) %>% 
  select(cluster)
