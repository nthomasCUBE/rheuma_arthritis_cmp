options(stringsAsFactors=FALSE)

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


	print("------------------------------------------------")
	print(paste0("parse_content::",expr_file))
	if(!exists("d2")){
		d2<<-read.csv(expr_file,sep="\t",header=T)
	}else{
		print(paste0("d2 already exists"))
		print(dim(d2))
	}
	print("------------------------------------------------")

	all_mods=unique(d1[,1])
	
	my_comp1=c("longRAF","earlyRAF")
	my_comp2=c("longRAM","earlyRAM")

	print("start...")
	df=data.frame()
	for(mo in 1:length(all_mods)){
		cur_mod=subset(d1,d1[,1]==all_mods[mo])
		d2_expr=subset(d2,d2[,1]%in%cur_mod[,2])
		for(mc in 1:length(my_comp1)){
			ix1=grep(my_comp1[mc],colnames(d2_expr))
			ix2=grep(my_comp2[mc],colnames(d2_expr))

			A=c(); B=c()
			for(x in 1:dim(d2_expr)[1]){
				val1=as.numeric(d2_expr[x,ix1])
				val2=as.numeric(d2_expr[x,ix2])
				A=c(A,mean(val1))
				B=c(B,mean(val2))
			}
			my_wx=(wilcox.test(A,B,paired=TRUE)$p.value)
			my_t=(t.test(A,B,paired=TRUE)$p.value)
			if(my_wx<0.05 && my_t<0.05){
				df=rbind(df,c(all_mods[mo],my_comp1[mc],my_t,my_wx))
			}
		}
	}

	df=cbind(df,p.adjust(df[,3]))
	df=cbind(df,p.adjust(df[,4]))

	library(xlsx)
	colnames(df)=c("module","comparison","t.test","wilcox.test_paired","t.test_adj","wilcox.test_paired_adj")
	write.xlsx(df,"alex_sign_comp.xlsx")
	print("ended...")
}


