function(){

	options(stringsAsFactors=FALSE)
	
	if(!exists("expr")){
		print("loading expr...")
		expr<<-read.csv("C:/Users/thoma/Google Drive/mirror_TN_VM/mirror/data/combinedFPKMs_highestTranscripts.txt",sep="\t",header=T)
		print(dim(expr))
	}

	if(!exists("modul")){
		print("loading modul...")
		modul<<-read.csv("C:/Users/thoma/Google Drive/mirror_TN_VM/mirror/2_WGCNA/archive01/gene_WGCNA_clustering_31aug18.txt",sep=" ",header=T)
		print(dim(modul))
	}

	u_cls=unique(modul[,1])
	for(x in 1:length(u_cls)){
		my_g=subset(modul,modul[,1]==u_cls[x])
		my_e=subset(expr,expr[,2]%in%my_g[,2])
		my_e=my_e[,3:(dim(my_e)[2]-3)]
		print(summary(my_e))
		barplot(apply(my_e,2,sum))
		exit
	}
}
