options(stringsAsFactors=FALSE)

#
#
#
density_plot_analysis=function(expr_file){
	print("density plot analysis")
	if(!exists("d3")){
		d3<<-read.csv(expr_file,sep="\t",header=T)
	}
	d3_sub=d3[,3:dim(d3)[2]]
	for(x in 1:dim(d3_sub)[2]){
		d3_sub[,x]=as.numeric(d3_sub[,x])
	}

	earlyRAF=grep("earlyRAF",colnames(d3))
	earlyRAF=apply(d3[,earlyRAF],1,median)

	earlyRAM=grep("earlyRAM",colnames(d3))
	earlyRAM=apply(d3[,earlyRAM],1,median)

	lateRAF=grep("longRAF",colnames(d3))
	lateRAF=apply(d3[,lateRAF],1,median)

	lateRAM=grep("longRAM",colnames(d3))
	lateRAM=apply(d3[,lateRAM],1,median)
	
	df=data.frame(earlyRAF,earlyRAM,lateRAF,lateRAM)
	print(head(df))
	print(summary(df))
	
	library(kdensity)
	plot(kdensity(df[,1],kernel="normal"),cex=0,type="o")
	for(x in 2:dim(df)[2]){
		points(kdensity(df[,x],kernel="normal"),cex=0,type="o")
	}
}


#
#	Module assignment
#
print_modules=function(d1){
	u_cols=unique(d1[,1])
	for(x in 1:length(u_cols)){
		N=subset(d1,d1[,1]==u_cols[x])
		N=dim(N)[1]
		print(c(u_cols[x],N))
	}
}

#
#	Extraction of the WGCNA content and of the expression data
#
parse_content=function(cluster_file,expr_file){
	print("------------------------------------------------")
	print(paste0("parse_content::",cluster_file))
	if(!exists("d1")){
		d1<<-read.csv(cluster_file,sep=" ",header=T)
	}else{
		print(paste0("d1 already exists"))
		print(dim(d1))
	}
	print_modules(d1)


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

	print(colnames(d2))

	df=data.frame()
	pdf("hist.pdf")
	par(mfrow=c(3,3))
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
			df=rbind(df,c(all_mods[mo],my_comp1[mc],my_t,my_wx))
			plot(log(1+A),log(1+B),xlim=c(0,max(log(1+A),log(1+B))),ylim=c(0,max(log(1+A),log(1+B))),cex=0.2,main=paste0(all_mods[mo],"_",my_comp1[mc]))
			legend("top",legend=c(my_comp1[mc],my_comp2[mc]))
		}
	}
	dev.off()
	
	df=cbind(df,p.adjust(df[,3]))
	df=cbind(df,p.adjust(df[,4]))
	df=subset(df,df[,5]<0.05 | df[,6]<0.05)

	library(xlsx)
	colnames(df)=c("module","comparison","t.test","wilcox.test_paired","t.test_adj","wilcox.test_paired_adj")
	write.xlsx(df,"alex_sign_comp.xlsx")
	
	print("start...")
	print("ended...")

	print(df)

}