function(){

	library(WGCNA)
	library(xlsx)

	options(stringsAsFactors=FALSE)
	
	if(!exists("expr")){
		print("loading expr...")
		expr<<-read.csv("C:/Users/thoma/Google Drive/mirror_TN_VM/mirror/data/combinedFPKMs_highestTranscripts.txt",sep="\t",header=T)
		print(dim(expr))
	}

	if(!exists("modul")){
		print("loading modul...")
		modul<<-read.csv("C:/Users/thoma/Google Drive/mirror_TN_VM/mirror/2_WGCNA/gene_WGCNA_clustering_pooled_3sep18.txt",sep=" ",header=T)
		print(dim(modul))
	}


	DF=data.frame()

	u_cls=unique(modul[,1])
	for(x in 1:length(u_cls)){
		print(u_cls[x])
		my_g=subset(modul,modul[,1]==u_cls[x])
		my_e=subset(expr,expr[,1]%in%my_g[,2])
		my_e=my_e[,3:(dim(my_e)[2]-3)]
		print(dim(my_e))
		MEList = moduleEigengenes(t(my_e),rep(u_cls[x],dim(my_e)[1]))
		MEs = MEList$eigengenes
		print(MEs)
		if(x==1){
			DF=data.frame(MEs)
		}else{
			DF=cbind(DF,MEs)
		}
	}

	write.xlsx(DF,"DF_3may19.xlsx")

}
