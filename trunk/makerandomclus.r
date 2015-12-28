randlabel <- function(num){
	randnum <- floor(num*runif(1));
	label <- paste("c",randnum, sep="");
}

randclusvec <- function(num, length){
	result <- c(randlabel(num));
	for (i in 1:(length-1)){
		print(i);
		result <- c(result, randlabel(num))
	}
	result;	
}

attachrandclus <- function(data, num){
	newcol <- randclusvec(num, dim(data)[1]);
	data <- cbind(data,newcol);
}

renamecol <- function(data,oldname,newname){
	namesvec <- colnames(data);
	namesvec[which(namesvec==oldname)] <- newname;
	colnames(data) <- namesvec;
	data;
}