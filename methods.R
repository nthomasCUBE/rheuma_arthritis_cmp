parse_content=function(my_file){
	print(paste0("parse_content::",my_file))
	
	data=read.csv(my_file,sep=" ",header=T)
	u_cols=unique(data[,1])
	print(length(u_cols))

	for(x in 1:length(u_cols)){
		N=subset(data,data[,1]==u_cols[x])
		N=dim(N)[1]
		print(c(u_cols[x],N))
	}


}