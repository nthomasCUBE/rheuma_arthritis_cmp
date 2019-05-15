import string
import sys
import statistics

fh=open("../data/combinedFPKMs_highestTranscripts.txt")
MAP={}
i=0
all_h={}
REMAP={}
for line in fh.readlines():
	line=line.strip()
	vals=line.split("\t")
	i=i+1
	if(i==1):
		my_header=vals
		for x in range(3,len(my_header)-3):
			for y in range(0,10):
				my_header[x]=my_header[x].replace(str(y),"")
	else:
		REMAP[vals[0]]=vals[1]
		for x in range(3,len(my_header)-3):
			if(MAP.get(vals[0])==None):
				MAP[vals[0]]={}
			if(MAP[vals[0]].get(my_header[x])==None):
				MAP[vals[0]][my_header[x]]=[]
			MAP[vals[0]][my_header[x]].append(float(vals[x]))
			all_h[my_header[x]]=1

ak=all_h.keys()
ak=sorted(ak)

fw=open("h1_combine_expression_per_group_V2.txt","w")
arr=["gene","gene_name"]
for ak_ in ak:
	arr.append(ak_)
fw.write("\t".join(arr)+"\n")
for MAP_ in MAP:
	arr=[]
	arr.append(MAP_)
	arr.append(REMAP[MAP_])
	for ak_ in ak:
		my_v=MAP[MAP_][ak_]
		arr.append(str(statistics.median(my_v)))
	fw.write("\t".join(arr)+"\n")
fw.close()
