#######create pcoa_plot##################### 

pcs<-read.table ("./unweighted_unifrac_pc.txt", header=TRUE, sep="\t", row.names=1, stringsAsFactor=FALSE)
map<-read.table ("./mapping.txt", header=TRUE, sep="\t", row.names=1, stringsAsFactor=FALSE)
pc3<-pcs[,1:3]
merged <- merge (map,pc3, by= "row.names")
ibd_healthy<-subset(merged, select=c(Row.names,IBDorHealthy,X1,X2,X3))
merged_clean<-na.omit(ibd_healthy)
merged_clean[  merged_clean$IBDorHealthy=="healthy",]$color <- "blue"
merged_clean[  merged_clean$IBDorHealthy=="IBD",]$color <- "red"

ggplot (merged_clean, aes(x=merged_clean$X1, y=merged_clean$X2, geom="blank", colour=merged_clean$color)) + geom_point (data=merged_clean, aes(x=merged_clean$X1, y=merged_clean$X2)) + scale_color_identity ("Datasets", breaks=merged_clean$color, labels=merged_clean$IBDorHealthy, guide="legend") + theme_classic() + labs(x="PC1", y="PC2")


ibd_healthy<-subset(merged, select=c(Row.names,ConclusionVisitDiagnosisShortCDUConly,X1,X2,X3))
merged_clean<-na.omit(ibd_healthy)
merged_clean$color <- "grey"
merged_clean$trans <- "1"
merged_clean[  merged_clean$ConclusionVisitDiagnosisShortCDUConly=="healthy",]$trans <- "0.5"
merged_clean[  merged_clean$ConclusionVisitDiagnosisShortCDUConly=="UC",]$color <- "chartreuse4"
merged_clean[  merged_clean$ConclusionVisitDiagnosisShortCDUConly=="CD",]$color <- "orangered3"

ggplot (merged_clean, aes(x=merged_clean$X1, y=merged_clean$X2, geom="blank", colour=merged_clean$color, alpha=merged_clean$trans)) + geom_point (data=merged_clean, aes(x=merged_clean$X1, y=merged_clean$X2)) + scale_color_identity ("Datasets", breaks=merged_clean$color, labels=merged_clean$ConclusionVisitDiagnosisShortCDUConly, guide="legend") + theme_classic() + labs(x="PC1", y="PC2")

ibd_healthy<-subset(merged, select=c(Row.names,IleumColonBoth,X1,X2,X3))
merged_clean<-na.omit(ibd_healthy)
merged_clean$color <- "grey"
merged_clean$trans <- "1"
merged_clean[  merged_clean$IleumColonBoth=="healthy",]$trans <- "0.9"
merged_clean[  merged_clean$IleumColonBoth=="healthy",]$color <- "blue"
merged_clean[  merged_clean$IleumColonBoth=="Ileum",]$color <- "red"
merged_clean[  merged_clean$IleumColonBoth=="Colon",]$color <- "orange"
merged_clean[  merged_clean$IleumColonBoth=="Both",]$color <- "purple"

ggplot (merged_clean, aes(x=merged_clean$X1, y=merged_clean$X2, geom="blank", colour=merged_clean$color, alpha=merged_clean$trans)) + geom_point (data=merged_clean, aes(x=merged_clean$X1, y=merged_clean$X2)) + scale_color_identity ("Inflammation", breaks=merged_clean$color, labels=merged_clean$IleumColonBoth, guide="legend") + theme_classic() + labs(x="PC1", y="PC2")

#######Calculate statistics IBD vs Healthy ###############

ibd_healthy<-subset(merged, select=c(Row.names,IBDorHealthy,X1,X2,X3))
merged_clean<-na.omit(ibd_healthy)
##Correlation PC's disease 
merged_clean$dummy<-"3"
merged_clean[ merged_clean$IBDorHealthy =="IBD",]$dummy<- 1 
merged_clean[ merged_clean$IBDorHealthy =="healthy",]$dummy<- 0
cor.test (merged_clean$dummy, merged_clean$X1, method="spearman")
cor.test (merged_clean$dummy, merged_clean$X2, method="spearman")
cor.test (merged_clean$dummy, merged_clean$X3, method="spearman")

##Diference Healthy vs IBD per PC's

IBD <- subset (merged_clean, merged_clean$IBDorHealthy=="IBD")
healthy <- subset (merged_clean, merged_clean$IBDorHealthy=="healthy")
wilcox<-wilcox.test(IBD$X1,healthy$X1)
wilcox<-wilcox.test(IBD$X2,healthy$X2)
wilcox<-wilcox.test(IBD$X3,healthy$X3)

#######Calculate statistics Healthy vs UC vs CD ###############

ibd_healthy<-subset(merged, select=c(Row.names,ConclusionVisitDiagnosisShortCDUConly,X1,X2,X3))
merged_clean<-na.omit(ibd_healthy)

##Correlation PC's disease 
merged_clean$dummy<-4
merged_clean[  merged_clean$ConclusionVisitDiagnosisShortCDUConly=="healthy",]$dummy <- 0
merged_clean[  merged_clean$ConclusionVisitDiagnosisShortCDUConly=="UC",]$dummy <- 1
merged_clean[  merged_clean$ConclusionVisitDiagnosisShortCDUConly=="CD",]$dummy <- 2
cor.test (merged_clean$dummy, merged_clean$X1, method="spearman")
cor.test (merged_clean$dummy, merged_clean$X2, method="spearman")
cor.test (merged_clean$dummy, merged_clean$X3, method="spearman")


##Diference Healthy vs UC vs CD per PC's
UC <- subset (merged_clean, merged_clean$ConclusionVisitDiagnosisShortCDUConly=="UC")
healthy <- subset (merged_clean, merged_clean$ConclusionVisitDiagnosisShortCDUConly=="healthy")
CD <- subset (merged_clean, merged_clean$ConclusionVisitDiagnosisShortCDUConly=="CD")
dati = list(g1=healthy$X1, g2=UC$X1, g3=CD$X1)
kruskal.test(dati)
dati = list(g1=healthy$X2, g2=UC$X2, g3=CD$X2)
kruskal.test(dati)
dati = list(g1=healthy$X3, g2=UC$X3, g3=CD$X3)
kruskal.test(dati)

wilcox<-wilcox.test(UC$X1,healthy$X1)
wilcox<-wilcox.test(UC$X2,healthy$X2)
wilcox<-wilcox.test(UC$X3,healthy$X3)

##Diference Healthy vs Colon vs Ileum vs Both per PC's
ibd_healthy<-subset(merged, select=c(Row.names,IleumColonBoth,X1,X2,X3))
merged_clean<-na.omit(ibd_healthy)
merged_clean$dummy<-3
merged_clean[  merged_clean$IleumColonBoth=="healthy",]$dummy <- 0
merged_clean[  merged_clean$IleumColonBoth=="Ileum",]$dummy <- 3
merged_clean[  merged_clean$IleumColonBoth=="Colon",]$dummy <- 1
merged_clean[  merged_clean$IleumColonBoth=="Both",]$dummy <- 2
cor.test (merged_clean$dummy, merged_clean$X1, method="spearman")
cor.test (merged_clean$dummy, merged_clean$X2, method="spearman")
cor.test (merged_clean$dummy, merged_clean$X3, method="spearman")



Ileum <- subset (merged_clean, merged_clean$IleumColonBoth=="Ileum")
healthy <- subset (merged_clean, merged_clean$IleumColonBoth=="healthy")
Colon <- subset (merged_clean, merged_clean$IleumColonBoth=="Colon")
Both <- subset (merged_clean, merged_clean$IleumColonBoth=="Both")
dati = list(g1=healthy$X1, g2=Colon$X1, g3=Ileum$X1, g4=Both$X1)
kruskal.test(dati)
ati = list(g1=healthy$X2, g2=Colon$X2, g3=Ileum$X2, g4=Both$X2)
kruskal.test(dati)
ati = list(g1=healthy$X3, g2=Colon$X3, g3=Ileum$X3, g4=Both$X3)
kruskal.test(dati)

wilcox<-wilcox.test(Colon$X1,Ileum$X1)
wilcox<-wilcox.test(Colon$X2,Ileum$X2)
wilcox<-wilcox.test(Colon$X3,Ileum$X3)

########Analysis alpha diversity ########################

#####IBD vs Healthy

obs_sp<- read.table("./observed_species.txt", header=TRUE, sep ="\t", row.names=1, stringsAsFactors = FALSE)
col_means<-colMeans (obs_sp)
map_and_sp <- merge (map, col_means, by="row.names")
ibd_healthy<-subset(map_and_sp, select=c(Row.names,IBDorHealthy,y))
IBD <- subset (ibd_healthy, ibd_healthy$IBDorHealthy=="IBD")
healthy <- subset (ibd_healthy, ibd_healthy$IBDorHealthy=="healthy")
wilcox<-wilcox.test(IBD$y,healthy$y)
wilcox$p.value
boxplot(healthy$y,IBD$y,ylim=c(0,800), ylab="Richness (observed species index)", names=c("Healthy", "IBD patients"))
text(x=2, y=750, "*", pos=3, cex=1.2)
ibd_healthy$violin<-"3"
ibd_healthy2<-na.omit(ibd_healthy)
ibd_healthy2[  ibd_healthy2$IBDorHealthy=="healthy",]$violin <- "0"
ibd_healthy2[  ibd_healthy2$IBDorHealthy=="IBD",]$violin <- "1"
ggplot(ibd_healthy2, aes(x=ibd_healthy2$violin, y=ibd_healthy2$y, fill=ibd_healthy2$violin)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1) + labs (y="Observed Species", x="Dataset") + theme_classic() + scale_fill_manual(values=c("lightblue2", "indianred1")) + scale_x_discrete(labels=c("Healthy","IBD")) + guides(fill=FALSE)


obs_sp<- read.table("./shannon.txt", header=TRUE, sep ="\t", row.names=1, stringsAsFactors = FALSE)
col_means<-colMeans (obs_sp)
map_and_sp <- merge (map, col_means, by="row.names")
ibd_healthy<-subset(map_and_sp, select=c(Row.names,IBDorHealthy,y))
IBD <- subset (ibd_healthy, ibd_healthy$IBDorHealthy=="IBD")
healthy <- subset (ibd_healthy, ibd_healthy$IBDorHealthy=="healthy")
wilcox<-wilcox.test(IBD$y,healthy$y)
wilcox$p.value
boxplot(healthy$y,IBD$y,ylim=c(0,800), ylab="Shannon Index", names=c("Healthy", "IBD patients"))
text(x=2, y=750, "*", pos=3, cex=1.2)
ibd_healthy$violin<-"3"
ibd_healthy2<-na.omit(ibd_healthy)
ibd_healthy2[  ibd_healthy2$IBDorHealthy=="healthy",]$violin <- "0"
ibd_healthy2[  ibd_healthy2$IBDorHealthy=="IBD",]$violin <- "1"
ggplot(ibd_healthy2, aes(x=ibd_healthy2$violin, y=ibd_healthy2$y, fill=ibd_healthy2$violin)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1) + labs (y="Shannon Index", x="Dataset") + theme_classic() + scale_fill_manual(values=c("lightblue2", "indianred1")) + scale_x_discrete(labels=c("Healthy","IBD")) + guides(fill=FALSE)


######Healthy vs Ileum vs Colon vs Both

obs_sp<- read.table("./shannon.txt", header=TRUE, sep ="\t", row.names=1, stringsAsFactors = FALSE)
col_means<-colMeans (obs_sp)
map_and_sp <- merge (map, col_means, by="row.names")
ibd_healthy<-subset(map_and_sp, select=c(Row.names,IleumColonBoth,y))
Ileum <- subset (ibd_healthy, ibd_healthy$IleumColonBoth=="Ileum")
healthy <- subset (ibd_healthy, ibd_healthy$IleumColonBoth=="healthy")
Colon <- subset (ibd_healthy, ibd_healthy$IleumColonBoth=="Colon")
Both <- subset (ibd_healthy, ibd_healthy$IleumColonBoth=="Both")
boxplot(healthy$y,Colon$y,Both$y,Ileum$y,ylim=c(0,9.5), ylab="Shannon", names=c("Healthy", "Colon", "Both", "Ileum"))

ibd_healthy$violin<-"3"
ibd_healthy2<-na.omit(ibd_healthy)
ibd_healthy2[  ibd_healthy2$IleumColonBoth=="healthy",]$violin <- "0"
ibd_healthy2[  ibd_healthy2$IleumColonBoth=="Colon",]$violin <- "1"
ibd_healthy2[  ibd_healthy2$IleumColonBoth=="Both",]$violin <- "2"
ibd_healthy2[  ibd_healthy2$IleumColonBoth=="Ileum",]$violin <- "3"
ggplot(ibd_healthy2, aes(x=ibd_healthy2$violin, y=ibd_healthy2$y, fill=ibd_healthy2$violin)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1) + labs (y="Shannon Index", x="Dataset") + theme_classic() + scale_fill_manual(values=c("lightblue2", "#fee0d2","#fc9272","#de2d26")) + scale_x_discrete(labels=c("Healthy","Colon","Both", "Ileum")) + guides(fill=FALSE) + ylim(0,8.5)

dati = list(g1=healthy$y, g2=Colon$y, g3=Ileum$y, g4=Both$y)
a=kruskal.test(dati)

wilcox.test(healthy$y, Colon$y)
wilcox.test(healthy$y, Both$y)
wilcox.test(healthy$y, Ileum$y)
wilcox.test(Colon$y, Both$y)
wilcox.test(Colon$y, Ileum$y)
wilcox.test(Both$y, Ileum$y)

obs_sp<- read.table("./observed_species.txt", header=TRUE, sep ="\t", row.names=1, stringsAsFactors = FALSE)
col_means<-colMeans (obs_sp)
map_and_sp <- merge (map, col_means, by="row.names")
ibd_healthy<-subset(map_and_sp, select=c(Row.names,IleumColonBoth,y))
Ileum <- subset (ibd_healthy, ibd_healthy$IleumColonBoth=="Ileum")
healthy <- subset (ibd_healthy, ibd_healthy$IleumColonBoth=="healthy")
Colon <- subset (ibd_healthy, ibd_healthy$IleumColonBoth=="Colon")
Both <- subset (ibd_healthy, ibd_healthy$IleumColonBoth=="Both")
boxplot(healthy$y,Colon$y,Both$y,Ileum$y,ylim=c(0,850), ylab="Observed species", names=c("Healthy", "Colon", "Both", "Ileum"))

ibd_healthy$violin<-"3"
ibd_healthy2<-na.omit(ibd_healthy)
ibd_healthy2[  ibd_healthy2$IleumColonBoth=="healthy",]$violin <- "0"
ibd_healthy2[  ibd_healthy2$IleumColonBoth=="Colon",]$violin <- "1"
ibd_healthy2[  ibd_healthy2$IleumColonBoth=="Both",]$violin <- "2"
ibd_healthy2[  ibd_healthy2$IleumColonBoth=="Ileum",]$violin <- "3"
ggplot(ibd_healthy2, aes(x=ibd_healthy2$violin, y=ibd_healthy2$y, fill=ibd_healthy2$violin)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1) + labs (y="Observed species", x="Dataset") + theme_classic() + scale_fill_manual(values=c("lightblue2", "#fee0d2","#fc9272","#de2d26")) + scale_x_discrete(labels=c("Healthy","Colon","Both", "Ileum")) + guides(fill=FALSE) + ylim(0,850)

dati = list(g1=healthy$y, g2=Colon$y, g3=Ileum$y, g4=Both$y)
a=kruskal.test(dati)

wilcox.test(healthy$y, Colon$y)
wilcox.test(healthy$y, Both$y)
wilcox.test(healthy$y, Ileum$y)
wilcox.test(Colon$y, Both$y)
wilcox.test(Colon$y, Ileum$y)
wilcox.test(Both$y, Ileum$y)

