savecolsxml <- function(table,cols,likelihood,filename){
	fc <- file(filename,"w");
	cat("<ClusterData>","\n",file=fc );
	cat("<MetaData>","\n",file=fc, append = TRUE  );
	cat("<Likelihood>",likelihood,"</Likelihood>", "\n", file=fc, append = TRUE);
	cat("</MetaData>","\n",file=fc, append = TRUE );
	cat("<Instances>","\n", file=fc, append = TRUE);
	for (l in 1:nrow(table)){
		cat("<Inst id =\"",l, "\"> " , file=fc, append = TRUE);
		for (i in cols){
			cat("<Clus name=\"", colnames(table)[i], "\"  val =\"" , as.character(datatable[l,i]), "\"", file=fc, append = TRUE);
		cat("/> ", file=fc);
		}
		cat("</Inst>", "\n", file=fc, append = TRUE);
	}
	cat("</Instances>", "\n", file=fc , append = TRUE );
	cat("</ClusterData>", file=fc );
	close(fc);
}