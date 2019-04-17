parse_content=function(cluster_file,expr_file){
	print("------------------------------------------------")
	print(paste0("parse_content::",cluster_file))
	data=read.csv(cluster_file,sep=" ",header=T)
	u_cols=unique(data[,1])
	for(x in 1:length(u_cols)){
		N=subset(data,data[,1]==u_cols[x])
		N=dim(N)[1]
		print(c(u_cols[x],N))
	}

	print("------------------------------------------------")
	print(paste0("parse_content::",expr_file))
	data=read.csv(expr_file,sep="\t",header=T)
	print(head(data))
	print("------------------------------------------------")
}