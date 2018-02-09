########## STATS 218 Final Project
##### Set Environment
library(rvest)
library(XML)
library(readxl)
library(readr)
library(xlsx)
setwd("~/Desktop/218/Final Project")


heros_attr <- read_excel("heros_attr.xlsx")
heros_attr$Hero <- gsub(" ", "-", tolower(heros_attr$Hero))
heros_attr$Hero <- gsub("'", "", heros_attr$Hero)
hero_names <- heros_attr$Hero
hero_names

##### Scrape hero matchups data from Dotabuff
data_dotabuff <- NULL

for (i in 1:length(hero_names)) {
  hero_name <- hero_names[i]
  print(i)
  url <- paste("https://www.dotabuff.com/heroes/", hero_name, "/matchups", sep="")
  page <- htmlTreeParse(readLines(url), useInternalNodes=T)
  table <- data.frame(readHTMLTable(page)[2])
  table$NULL.V1 <- hero_name
  table$NULL.V3 <- gsub("%", "", table$NULL.V3)
  table$NULL.V3 <- as.numeric(table$NULL.V3)/100
  table$NULL.V4 <- gsub("%", "", table$NULL.V4)
  table$NULL.V4 <- as.numeric(table$NULL.V4)/100
  table$NULL.V5 <- gsub(",", "", table$NULL.V5)
  table$NULL.V5 <- as.numeric(table$NULL.V5)
  
  table_sort <- table[order(-table$NULL.V4),]
  # new_table <- head(table_sort, 10)
  data_dotabuff <- rbind(data_dotabuff, table_sort)
}

colnames(data_dotabuff) <- c("Hero1", "Hero2", "Advantage", "Win_Rate", "Matches_Played")
data_dotabuff$Hero2 <- gsub(" ", "-", tolower(data_dotabuff$Hero2))
data_dotabuff$Hero2 <- gsub("'", "", data_dotabuff$Hero2)
dim(data_dotabuff)



##### Scrape hero lane data from Dotabuff
url1 <- 'https://www.dotabuff.com/heroes/lanes?lane=mid'
page1 <- htmlTreeParse(readLines(url1), useInternalNodes=T)
mid <- data.frame(readHTMLTable(page1)[1])[,2:3]
colnames(mid) <- c("Hero", "Presence")
dim(mid)
mid$Hero <- gsub(" ", "-", tolower(mid$Hero))
mid$Hero <- gsub("'", "", mid$Hero)
mid$Presence <- as.numeric(gsub("%", "", (mid$Presence)))/100

url2 <- 'https://www.dotabuff.com/heroes/lanes?lane=off'
page2 <- htmlTreeParse(readLines(url2), useInternalNodes=T)
off <- data.frame(readHTMLTable(page2)[1])[,2:3]
colnames(off) <- c("Hero", "Presence")
dim(off)
off$Hero <- gsub(" ", "-", tolower(off$Hero))
off$Hero <- gsub("'", "", off$Hero)
off$Presence <- as.numeric(gsub("%", "", (off$Presence)))/100

url3 <- 'https://www.dotabuff.com/heroes/lanes?lane=safe'
page3 <- htmlTreeParse(readLines(url3), useInternalNodes=T)
safe <- data.frame(readHTMLTable(page3)[1])[,2:3]
colnames(safe) <- c("Hero", "Presence")
dim(safe)
safe$Hero <- gsub(" ", "-", tolower(safe$Hero))
safe$Hero <- gsub("'", "", safe$Hero)
safe$Presence <- as.numeric(gsub("%", "", (safe$Presence)))/100


url4 <- 'https://www.dotabuff.com/heroes/lanes?lane=jungle'
page4 <- htmlTreeParse(readLines(url4), useInternalNodes=T)
jung <- data.frame(readHTMLTable(page4)[1])[,2:3]
colnames(jung) <- c("Hero", "Presence")
dim(jung)
jung$Hero <- gsub(" ", "-", tolower(jung$Hero))
jung$Hero <- gsub("'", "", jung$Hero)
jung$Presence <- as.numeric(gsub("%", "", (jung$Presence)))/100


url5 <- 'https://www.dotabuff.com/heroes/lanes?lane=roaming'
page5 <- htmlTreeParse(readLines(url5), useInternalNodes=T)
roam <- data.frame(readHTMLTable(page5)[1])[,2:3]
colnames(roam) <- c("Hero", "Presence")
dim(roam)
roam$Hero <- gsub(" ", "-", tolower(roam$Hero))
roam$Hero <- gsub("'", "", roam$Hero)
roam$Presence <- as.numeric(gsub("%", "", (roam$Presence)))/100




##### Data preprocessing
heros_attr1 <- merge(heros_attr, safe, by.x="Hero", by.y="Hero", all.x=T)
colnames(heros_attr1)[24] <- "Safe"
heros_attr2 <- merge(heros_attr1, mid, by.x="Hero", by.y="Hero", all.x=T)
dim(heros_attr2)[2]
colnames(heros_attr2)[25] <- "Mid"
heros_attr3 <- merge(heros_attr2, off, by.x="Hero", by.y="Hero", all.x=T)
dim(heros_attr3)[2]
colnames(heros_attr3)[26] <- "Off"
heros_attr4 <- merge(heros_attr3, jung, by.x="Hero", by.y="Hero", all.x=T)
dim(heros_attr4)[2]
colnames(heros_attr4)[27] <- "Jung"
heros_attr5 <- merge(heros_attr4, roam, by.x="Hero", by.y="Hero", all.x=T)
dim(heros_attr5)[2]
colnames(heros_attr5)[28] <- "Roam"

heros_attr5[is.na(heros_attr5)] <- 0
all_attr <- heros_attr5
lane_index <- max.col(all_attr[,24:26],ties.method="first")+23
all_attr$Lane <- colnames(all_attr)[lane_index]
# lane45_index <- max.col(all_attr[,27:28],ties.method="first")+26
# all_attr$Lane45 <- colnames(all_attr)[lane45_index]
View(all_attr)

# all_attr$Position <- NA
# for (i in 1:115){
#   if (all_attr[i, "Jung"] != 0 | all_attr[i, "Roam"] != 0){
#     all_attr[i, "Position"] <- "supporter"
#   }
#   else if(all_attr[i, "Lane"] == "Safe"){
#     all_attr[i, "Position"] <- "carry"
#   }
#   else if(all_attr[i, "Lane"] == "Mid"){
#     all_attr[i, "Position"] <- "mid laner"
#   }
#   else if(all_attr[i, "Lane"] == "Off"){
#     all_attr[i, "Position"] <- "off laner"
#   }
#   
# }
# 
# write.xlsx(all_attr, "all_attr.xlsx")
attr <- read_excel("all_attr_new.xlsx")
View(attr)
quantile(attr$KDA_Ratio, probs = seq(0, 1, 1/3)) # 1.64      2.35      2.64      3.73 

attr$KDA_group <- NA
for (i in 1:115){
  if (attr[i,]$KDA_Ratio < 2.35){
    attr[i,]$KDA_group <- "low KDA"
  }
  else if (attr[i,]$KDA_Ratio <= 2.64){
    attr[i,]$KDA_group <- "mid KDA"
  }
  else if (attr[i,]$KDA_Ratio > 2.64){
    attr[i,]$KDA_group <- "high KDA"
  }
}
View(attr)


quantile(data_dotabuff$Win_Rate, probs = seq(0, 1, 0.05))
mod_data <- data_dotabuff
# which(new_data$Win_Rate == 0.50000)
new_data2 <- mod_data[mod_data$Win_Rate > 0.50000, ]
dim(new_data2)
quantile(new_data2$Win_Rate, probs = seq(0.5, 1, 0.05))
cut_off <- 0.6

win_rate_CDF <- ecdf(quantile(new_data2$Win_Rate))
win_rate_CDF[0.6857]

new_data <- new_data2
new_data[new_data$Win_Rate < cut_off, 4] <- 0
dim(new_data)
# View(new_data)

data <- new_data[, c(1,2,4,5)]
el2 <- data[, 1:3]
dim(el2)
myel <- data.frame(lapply(el2, as.character), stringsAsFactors=FALSE)
myel <- as.matrix(el2)
sm <- el2sm(myel, directed=TRUE, nadiag=TRUE)
class(sm) <- "numeric"
dim(sm) # 115 115
class(sm[10,10])
el <- el2[el2$Win_Rate > 0, ]
dim(el) # 420 3
# View(el)

### Normalize n_Matches
a <- attr$n_Matches
attr$n_Matches_nml = (a-min(a))/(max(a)-min(a))
attr$n_Matches_nml

dotahero <- list(Y = sm, X = attr)
View(dotahero$X)



##### Create network object
library(network)
library(sna)
library(degreenet)
library(rda)

hero_attr <- as.list(data.frame(dotahero$X))

View(el)
View(attr)
new_el <- el[,1:3]
new_el$diff_KDA <- NA
View(new_el)

for (i in 1:dim(new_el)[1]) {
  hero_name <- hero_names[i]
  print(i)
  hero1 <- new_el[i,]$Hero1
  hero2 <- new_el[i,]$Hero2
  new_el[i,4] <- attr$KDA_Ratio[attr$Hero==hero1] - attr$KDA_Ratio[attr$Hero==hero2]
}
View(new_el)  


n1 <- network(dotahero$Y, vertex.attr=hero_attr, 
              vertex.attrnames=colnames(dotahero$X), directed=TRUE)
set.edge.attribute(n1, "diff_KDA", new_el$diff_KDA)
set.edge.attribute(n1, "win_rate", new_el$Win_Rate)
n1
summary(n1)

n_match <- n1 %v% "n_Matches_nml" # %v% references vertex attributes
n_match

png(filename="~/Desktop/218/Final Project/Figures/network.png",
    units="in", width=5, height=5, res=300)
plot(n1, displaylabels=T,
     label.cex=0.5, vertex.cex=0.75,
     main="Hero Win Rate Network")
dev.off()

### Plot the subgraph comprised of the largest component
el.pair <- el[,1:2]
n2 <- network(el, matrix.type="edgelist", vertex.attr=hero_attr, 
               vertex.attrnames=colnames(dotahero$X), directed=TRUE)

gplot(n2, vertex.col = "yellow", vertex.cex = 1)

png(filename="~/Desktop/218/Final Project/Figures/largest-component.png",
    units="in", width=5, height=5, res=300)
plot(n2, displaylabels=T,
     label.cex=0.5, vertex.cex=1, vertex.col = "yellow",
     main="Largest component - Hero Win Rate Network")
dev.off()

##### Degree distribution

# Start by presenting visual summaries of the networks, followed by numerical summary
# measures.
# Describe the impact of the covariates on the patterns of ties.
# Describe any sub-group cohesion you see in the networks.

network.vertex.names(n1)
ideg=degree(n1, gmode="digraph", cmode="indegree")
ideg2=degree(n2, gmode="digraph", cmode="indegree")
ideg2
summary(ideg)

odeg=degree(n1, gmode="outdegree", cmode="outdegree")
odeg2=degree(n2, gmode="outdegree", cmode="outdegree")
odeg2
summary(odeg)

### 3 isolates: “Death Prophet”, “Keeper of the Light” and “Queen of Pain”
pct_n_match <- ecdf(hero_attr$n_Matches)

as.numeric(heros_attr[hero_attr$Hero=="death-prophet", "n_Matches"]) # 765481
as.numeric(heros_attr[hero_attr$Hero=="death-prophet", "KDA_Ratio"]) # 2.29
as.numeric(heros_attr[hero_attr$Hero=="death-prophet", "Hero_DamagePM"]) # 532.83
as.numeric(heros_attr[hero_attr$Hero=="death-prophet", "Tower_DamagePM"]) # 80.7
as.numeric(heros_attr[hero_attr$Hero=="death-prophet", "Hero_HealingPM"]) # 0.71

as.numeric(heros_attr[hero_attr$Hero=="keeper-of-the-light", "n_Matches"]) # 1130593
as.numeric(heros_attr[hero_attr$Hero=="keeper-of-the-light", "KDA_Ratio"]) # 2.33
as.numeric(heros_attr[hero_attr$Hero=="keeper-of-the-light", "Hero_DamagePM"]) # 305.98
as.numeric(heros_attr[hero_attr$Hero=="keeper-of-the-light", "Tower_DamagePM"]) # 9.51
as.numeric(heros_attr[hero_attr$Hero=="keeper-of-the-light", "Hero_HealingPM"]) # 61.67

as.numeric(heros_attr[hero_attr$Hero=="queen-of-pain", "n_Matches"]) # 2039550
as.numeric(heros_attr[hero_attr$Hero=="queen-of-pain", "KDA_Ratio"]) # 3.05
as.numeric(heros_attr[hero_attr$Hero=="queen-of-pain", "Hero_DamagePM"]) # 595.65
as.numeric(heros_attr[hero_attr$Hero=="queen-of-pain", "Tower_DamagePM"]) # 51.08
as.numeric(heros_attr[hero_attr$Hero=="queen-of-pain", "Hero_HealingPM"]) # 0.25

### Highest out-degree and in-degree heroes
network.vertex.names(n1)[which.max(odeg)] # zeus id=26
heros_attr[hero_attr$Hero=="zeus", "KDA_Ratio"] # 3.73
heros_attr[hero_attr$Hero=="zeus", "Hero_DamagePM"] # 931.08
heros_attr[hero_attr$Hero=="zeus", "Tower_DamagePM"] # 11

network.vertex.names(n1)[which(odeg==21)] # vengeful-spirit id=21
heros_attr[hero_attr$Hero=="vengeful-spirit", "KDA_Ratio"] # 3.73
heros_attr[hero_attr$Hero=="vengeful-spirit", "Hero_DamagePM"] # 931.08
heros_attr[hero_attr$Hero=="vengeful-spirit", "Tower_DamagePM"] # 37.24

network.vertex.names(n1)[which(odeg==20)] # "chaos-knight" "omniknight"
heros_attr[hero_attr$Hero=="chaos-knight", "KDA_Ratio"] # 2.72
heros_attr[hero_attr$Hero=="chaos-knight", "Hero_DamagePM"] # 554.76
heros_attr[hero_attr$Hero=="omniknight", "KDA_Ratio"] # 2.54
heros_attr[hero_attr$Hero=="omniknight", "Hero_HealingPM"] # 144.38

network.vertex.names(n1)[which.max(ideg)] # io
heros_attr[hero_attr$Hero=="io", "KDA_Ratio"] # 1.64
heros_attr[hero_attr$Hero=="io", "Hero_DamagePM"] # 180.39
heros_attr[hero_attr$Hero=="io", "Hero_HealingPM"] # 145
heros_attr[hero_attr$Hero=="io", "Tower_DamagePM"] # 13.59

network.vertex.names(n1)[which(ideg==77)] # "lone-druid"
heros_attr[hero_attr$Hero=="lone-druid", "KDA_Ratio"] # 2.14
heros_attr[hero_attr$Hero=="lone-druid", "Hero_DamagePM"] # 364.57
heros_attr[hero_attr$Hero=="lone-druid", "Hero_HealingPM"] # 2.66
heros_attr[hero_attr$Hero=="lone-druid", "Tower_DamagePM"] # 94.83

png(filename="~/Desktop/218/Final Project/Figures/degreedist.png",
    units="in", width=5, height=5, res=300)
par(mfrow=c(2,1))
barplot(odeg, ylim = c(0,max(odeg)+5),
        main="Out Degree Distribution")
barplot(ideg, ylim = c(0,max(ideg)+5),
        main="In Degree Distribution")
dev.off()



##### Centrality Analysis

# Describe the pattern of centralization of the actors.
g1 <- network(el, matrix.type="edgelist", directed = F)
g1
## Degree centrality
dc1 <- centralization(g1, degree, mode="graph")
dc1 # 0.7017199

## Eigenvalue centrality
e1 <- evcent(g1, gmode="graph")
e1
ec1 <- centralization(g1, evcent, mode="graph")
ec1 # 0.4224704
barplot(e1)
summary(e1)

## Closeness centrality
c1 <- closeness(g1, gmode="graph")
c1
cc1 <- centralization(g1, closeness, mode="graph")   
cc1 # 0.704382
barplot(c1)
summary(c1)

## Betweenness centrality
b1 <- betweenness(g1, gmode="graph")
bc1 <- centralization(g1, betweenness, mode="graph")   
bc1 # 0.4191147
barplot(b1)
summary(b1)

png(filename="~/Desktop/218/Final Project/Figures/EC.png",
    units="in", width=5, height=5, res=300)
barplot(e1, main='Eigenvalue Centrality')
dev.off()

png(filename="~/Desktop/218/Final Project/Figures/CC.png",
    units="in", width=5, height=5, res=300)
barplot(c1, main='Closeness Centrality')
dev.off()

png(filename="~/Desktop/218/Final Project/Figures/BC.png",
    units="in", width=5, height=5, res=300)
barplot(b1, main='Betweenness Centrality')
dev.off()



##### Fit social network models

# Fit latent position and latent position cluster models to the network. Is there evidence
# of clustering?
# If you have directed data, fit a triad census model to the networks. Is there evidence
# of balance in the patterns of ties?
# You can fit any ERGM you like to represent the structure.
library(latentnet)
library(ergm)
library(mclust)
library(stargazer)
library(texreg)


##### Fit latent position model
fit.lp <- ergmm(n1 ~ euclidean(d=2), verbose = T)
summary(fit.lp) # 3856.015
mcmc.diagnostics(fit.lp)
plot(n1, displaylabels=T, vertex.col="Position",
     label.cex=0.5, vertex.cex=0.75, pad=0.1, label.bg = "white",
     coord=fit.lp$mkl$Z,
     main="Fitted Latent Positions")

fit.lp2 <- ergmm(n1 ~ euclidean(d=2) + latentcov("diff_KDA"), verbose = T)
summary(fit.lp2) # 2150.813


# fit.lp2 <- ergmm(n1 ~ euclidean(d=2), response="win_rate", verbose = T)
# summary(fit.lp2)
# mcmc.diagnostics(fit.lp2)
# fit.lp2$mkl$Z
# plot(n1, displaylabels=T, vertex.col="Position",
#      label.cex=0.5, vertex.cex=0.75, pad=0.1, label.bg = "white",
#      coord=fit.lp2$mkl$Z,
#      main="Fitted Latent Positions")


fit.lp3 <- ergmm(n1 ~ euclidean(d=3))
summary(fit.lp3) # 4060.505


pdf(file="~/Desktop/218/Final Project/Figures/mcmc.diagnostics.latentposition.pdf")
mcmc.diagnostics(fit.lp)
dev.off()

### Plot
n_match <- n1 %v% "n_Matches_nml" # %v% references vertex attributes
hero_names[which.max(n_match)]

png(filename="~/Desktop/218/Final Project/Figures/latentposition.png",
    units="in", width=5, height=5, res=300)
plot(n1, displaylabels=T, vertex.col="Position",
     label.cex=0.5, vertex.cex=n_match*3, pad=0.1, label.bg = "white",
     coord=fit.lp$mkl$Z,
     main="Fitted Latent Positions")
legend("topleft",bty="n", cex=0.75,
       legend=c("carry","jungler","mid laner", "mix", "off laner", "supporter"),text.col=1:6)
dev.off()


png(filename="~/Desktop/218/Final Project/Figures/latentposition2.png",
    units="in", width=5, height=5, res=300)
plot(n1, displaylabels=T, vertex.col="Position",
     label.cex=0.5, vertex.cex=n_match*3, pad=0.1, label.bg = "white",
     coord=fit.lp2$mkl$Z,
     main="Fitted Latent Positions with Edge Covariate: diff KDA")
legend("topright",bty="n", cex=0.75,
       legend=c("carry","jungler","mid laner", "mix", "off laner", "supporter"),text.col=1:6)
dev.off()

anova(fit.lp, fit.lp2)



##### Fit latent position cluster model
n3
fit.clust2 <- ergmm(n1 ~ euclidean(d=2, G=2))
summary(fit.clust2)
bic2 <- bic.ergmm(fit.clust2) # 3603.005
bic2
plot(fit.clust2)


fit.clust22 <- ergmm(n1 ~ euclidean(d=2, G=2) + latentcov("diff_KDA"))
summary(fit.clust22)
bic22 <- bic.ergmm(fit.clust22) # 2118.735
bic22
plot(fit.clust22)



fit.clust3 <- ergmm(n1 ~ euclidean(d=2, G=3))
summary(fit.clust3)
bic3 <- bic.ergmm(fit.clust3) # 3812.057
bic3
plot(fit.clust3)


fit.clust32 <- ergmm(n1 ~ euclidean(d=2, G=3) + latentcov("diff_KDA"))
summary(fit.clust32)
bic32 <- bic.ergmm(fit.clust32) # 2118.985
bic32
plot(fit.clust32)



fit.clust4 <- ergmm(n1 ~ euclidean(d=2, G=4))
summary(fit.clust4)
bic4 <- bic.ergmm(fit.clust4) # 3221.567
bic4
plot(fit.clust4)
print(round(fit.clust4$mkl$mbc$Z.pZK,3))
mcmc.diagnostics(fit.clust4)


fit.clust42 <- ergmm(n1 ~ euclidean(d=2, G=4) + latentcov("diff_KDA"))
summary(fit.clust42)
bic42 <- bic.ergmm(fit.clust42) # 2158.416
bic42
plot(fit.clust42)


fit.clust5 <- ergmm(n1 ~ euclidean(d=2, G=5))
summary(fit.clust5)
bic5 <- bic.ergmm(fit.clust5) # 4620.277
bic5
plot(fit.clust5)

# fit.clust52 <- ergmm(n1 ~ euclidean(d=2, G=5) + latentcov("diff_KDA"))
# summary(fit.clust52)
# bic52 <- bic.ergmm(fit.clust52)
# bic52
# plot(fit.clust52)



bics <- cbind(bic2$overall, bic3$overall, bic4$overall, bic5$overall)

png(filename="~/Desktop/218/Final Project/Figures/BICs.png",
    units="in", width=5, height=5, res=300)
barplot(bics, names.arg=c("2 Groups", "3 Gourps", "4 Groups", "5 Groups"),
        ylim=c(0,5000))
dev.off()


png(filename="~/Desktop/218/Final Project/Figures/2cluster.png",
    units="in", width=5, height=5, res=300)
plot(fit.clust2)
dev.off()

png(filename="~/Desktop/218/Final Project/Figures/3cluster.png",
    units="in", width=5, height=5, res=300)
plot(fit.clust3)
dev.off()

png(filename="~/Desktop/218/Final Project/Figures/4cluster.png",
    units="in", width=5, height=5, res=300)
plot(fit.clust4)
dev.off()


png(filename="~/Desktop/218/Final Project/Figures/diag.4cluster.png",
    units="in", width=5, height=5, res=300)
mcmc.diagnostics(fit.clust4)
dev.off()


save.image("~/Desktop/218/Final Project/final_project_12-14.RData")




##### Fit a triad census model
census <- triad.census(n1, mode = c("digraph"))
census
stargazer(census)

fit.triad <- ergm(n1 ~ triadcensus,
            estimate = "MPLE",
            eval.loglik=FALSE)
summary(fit.triad)
texreg(fit.triad)

fit.triad2 <- ergm(n1 ~ triadcensus(c(1,3,4,8)))
summary(fit.triad2)
texreg(fit.triad2)



##### Fit ERGM models with covariate terms
n3 <- network(new_el, matrix.type="edgelist",
              vertex.attr=hero_attr, 
              vertex.attrnames=colnames(dotahero$X), directed=TRUE)
plot(n3)
set.edge.attribute(n3, "diff_KDA", new_el$diff_KDA)
set.edge.attribute(n3, "win_rate", new_el$Win_Rate)
n3

n1
fit.ergm01 <- ergm(n1 ~ edges + nodemix("KDA_group"), eval.loglik=T, estimate="MLE")
summary(fit.ergm01) # 3308
stargazer(fit.ergm01)
texreg(fit.ergm01)
mcmc.diagnostics(fit.ergm01)
fit.ergm01

fit.ergm02 <- ergm(n1 ~ edges + nodemix("KDA_group"), eval.loglik=T,
                          control=control.ergm(MCMC.burnin=15000, MCMC.samplesize=1000))


png(filename="~/Desktop/218/Final Project/Figures/diag.png",
    units="in", width=5, height=5, res=300)
mcmc.diagnostics(fit.ergm02)
dev.off()



fit.ergm1 <- ergm(n1 ~ edges + nodemix("Position"))
summary(fit.ergm1) # 4083

anova(fit.ergm01, fit.ergm1)

fit.ergm2 <- ergm(n1 ~ edges + nodemix("Position") + nodemix("KDA_group"))
summary(fit.ergm2) # 3679
anova(fit.ergm01, fit.ergm2)

fit.ergm3 <- ergm(n1 ~ edges + nodemix("Position") + nodemix("KDA_group")
                  + absdiff("KDA_Ratio"))
summary(fit.ergm3)

anova_table2 <- anova(fit.ergm0, fit.ergm2, fit.ergm3)
texreg()

##### Fit SRM models  
library(amen)

dyad_attr <- array(0, dim=c(115,115,1))
for (n in 1:dim(new_el)[1]){
  hero_name <- hero_names[n]
  print(n)
  hero1 <- new_el[i,]$Hero1
  hero2 <- new_el[i,]$Hero2
  i <- which(hero_names==hero1)
  j <- which(hero_names==hero2)
#  dyad_attr[i,j,1] <- new_el[i,3]
  dyad_attr[i,j,1] <- new_el[i,4]
} 
rownames(dyad_attr) <- hero_names
colnames(dyad_attr) <- hero_names

colnames(dotahero$X)

Y <- dotahero$Y
Xn <- as.matrix(dotahero$X[,c(24:30)])
dim(Xn)
colnames(Xn)
Xd <- dyad_attr
dim(Xd)
fit.ame <- ame(Y, Xdyad=Xd, Xrow=Xn, Xcol=Xn, plot=F)
fit.ame2 <-ame(Y, Xdyad=Xd, Xrow=Xn, Xcol=Xn, R=2, nscan=10000, plot=F)
summary(fit.ame)
summary(fit.ame2)
save.image("~/Desktop/218/Final Project/final_project_12-14.RData")


