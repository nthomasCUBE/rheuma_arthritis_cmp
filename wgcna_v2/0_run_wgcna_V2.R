#install.packages(c("dynamicTreeCut", "cluster", "flashClust", "Hmisc", "reshape", "foreach", "doParallel") ) 
#source("http://bioconductor.org/biocLite.R") 
#biocLite("impute")
#install.packages("WGCNA")
#http://pklab.med.harvard.edu/scw2014/WGCNA.html

# ----------------------------------------------------------------------------------------

library("WGCNA")
library("flashClust")

options(stringsAsFactors=FALSE)

pdf("0_run_wgcna.pdf")
enableWGCNAThreads()

my_opt="pooled"

# ----------------------------------------------------------------------------------------
preload=TRUE
if(preload==FALSE){
	if(my_opt=="sample"){
		data=read.csv("combinedFPKMs_highestTranscripts.txt",sep="\t",header=T)
	}else if(my_opt=="pooled"){
                data=read.csv("h1_combine_expression_per_group_V2.txt",sep="\t",header=T)
	}
	save.image("0_run_wgcna.RData")
}else{
	load("0_run_wgcna.RData")
}
# ----------------------------------------------------------------------------------------

print("load...")
if(my_opt=="sample"){
	df=as.data.frame(data[,3:(dim(data)[2]-3)])
	rownames(df)=data[,2]
}else{
        df=as.data.frame(data[,3:(dim(data)[2])])
	print(df[1:5,1:5])
	rownames(df)=data[,1]
}

print("filter...")
xx=apply(df[,2:dim(df)[2]],1,sum); df=df[xx>10,]
xx=apply(df[,2:dim(df)[2]],1,sd);  df=df[xx>0.5,]
data=df

gene.names=as.character(data[,1])
trans.oed=t(data)
n=dim(data)[1]
#n=1000
datExpr=trans.oed[,1:n]
SubGeneNames=gene.names[1:n]
powers = c(c(1:10), seq(from = 12, to=20, by=2));
sft=pickSoftThreshold(datExpr,dataIsExpr = TRUE,powerVector = powers,corFnc = cor,corOptions = list(use = 'p'),networkType = "unsigned")
print(sft)

# ----------------------------------------------------------------------------------------

softPower = 7;
adj= adjacency(datExpr,type = "unsigned", power = softPower);
TOM=TOMsimilarityFromExpr(datExpr,networkType = "unsigned", TOMType = "unsigned", power = softPower);
colnames(TOM) =rownames(TOM) =SubGeneNames
dissTOM=1-TOM
geneTree = flashClust(as.dist(dissTOM),method="average");
plot(geneTree, xlab="", sub="",cex=0.3);

minModuleSize = 50;
dynamicMods = cutreeDynamic(dendro = geneTree,  method="tree", minClusterSize = minModuleSize);
table(dynamicMods)
dynamicColors = labels2colors(dynamicMods)
table(dynamicColors)

MEList = moduleEigengenes(datExpr, colors = dynamicColors)
MEs = MEList$eigengenes
plotEigengeneNetworks(MEs, "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2))

MEList = moduleEigengenes(datExpr, colors = dynamicColors)
MEs = MEList$eigengenes
plotEigengeneNetworks(MEs, "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2))
save(datExpr, dynamicColors, MEList,file="0_run_wgcna_V2.RData")

# ----------------------------------------------------------------------------------------

dev.off()
