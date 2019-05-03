function(){
	
	if(!exists("expr")){
		expr<<-read.csv("C:/Users/thoma/Google Drive/mirror_TN_VM/mirror/data/combinedFPKMs_highestTranscripts.txt",sep="\t",header=T)
		print(dim(expr))
	}

}
