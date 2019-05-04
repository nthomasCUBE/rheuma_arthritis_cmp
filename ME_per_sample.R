ME_per_sample=function(){

	library(WGCNA)
	library(xlsx)

	options(stringsAsFactors=FALSE)
	
	if(!exists("expr")){
		print("loading expr...")
		expr<<-read.csv("../mirror/data/combinedFPKMs_highestTranscripts.txt",sep="\t",header=T)
		print(dim(expr))
	}

	if(!exists("modul")){
		print("loading modul...")
		modul<<-read.csv("../mirror/2_WGCNA/gene_WGCNA_clustering_pooled_3sep18.txt",sep=" ",header=T)
		print(dim(modul))
	}

	DF=data.frame()
	u_cls=unique(modul[,1])
	my_E=data.frame()
	my_C=c()
	for(x in 1:length(u_cls)){
		print(u_cls[x])
		my_g=subset(modul,modul[,1]==u_cls[x])
		my_e=subset(expr,expr[,1]%in%my_g[,2])
		my_e=my_e[,3:(dim(my_e)[2]-3)]
		my_E=rbind(my_E,my_e)
		my_C=c(my_C,rep(u_cls[x],dim(my_e)[1]))
	}
	MEList = moduleEigengenes(t(my_E),my_C)
	write.xlsx(MEList$eigengene,"DF2_V2_4may19.xlsx")
}
