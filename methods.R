print_modules=function(d1){
	u_cols=unique(d1[,1])
	for(x in 1:length(u_cols)){
		N=subset(d1,d1[,1]==u_cols[x])
		N=dim(N)[1]
		print(c(u_cols[x],N))
	}
}

parse_content=function(cluster_file,expr_file){
	print("------------------------------------------------")
	print(paste0("parse_content::",cluster_file))
	if(!exists("d1")){
		d1<<-read.csv(cluster_file,sep=" ",header=T)
		print_modules(d1)
	}else{
		print(paste0("d1 already exists"))
		print(dim(d1))
	}

	my_dark_green=subset(d1,d1[,1]=="darkgreen")

	print("------------------------------------------------")
	print(paste0("parse_content::",expr_file))
	if(!exists("d2")){
		d2<<-read.csv(expr_file,sep="\t",header=T)
	}else{
		print(paste0("d2 already exists"))
		print(dim(d2))
	}
	print("------------------------------------------------")

	d2_expr=subset(d2,d2[,1]%in%my_dark_green[,2])
	print(dim(d2_expr))
	print(colnames(d2_expr))

	ix1=grep("longRAF",colnames(d2_expr))
	ix2=grep("longRAM",colnames(d2_expr))
	
	A=c()
	B=c()
	for(x in 1:dim(d2_expr)){
		val1=as.numeric(d2_expr[x,ix1])
		val2=as.numeric(d2_expr[x,ix2])
		A=c(A,mean(val1))
		B=c(B,mean(val2))
	}
	print(t.test(A,B)$p.value)
	print(wilcox.test(A,B)$p.value)
}	