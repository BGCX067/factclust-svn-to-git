library(nnet);

source("/home/jaeger/B/SWF/makerandomclus.r");
source("/home/jaeger/B/SWF/savexml.r");

# TODO: currently implementation assumes nominal predictors.
# should also allow ordinal predictor variables, i.e. 
# general layerstruc vectors of the form c(o3,o2,n2)
# where o3 represents and ordinal predictor variable with states 1,2,3,
# and n2 represents a nominal predictor variable with states c1,c2.

# layerstruc <- c(2,2); 

datatable <- read.table(inputdata);

numinstances <- dim(datatable)[1];
numattributes <- dim(datatable)[2];
numlayers <- length(layerstruc);

#
# Initialize table for cluster labels and attach 
# to datatable
#
clusters <- array(0, dim = c(numinstances,numlayers));
 
for (i in 1:(numlayers)){
		for (j in 1:(numinstances)){
			clusters[j,i] <- randlabel(layerstruc[i]);
		}
	}

colnames(clusters) <- colnames(clusters,do.NULL=FALSE,prefix="layer_");

datatable <- cbind(datatable,clusters);

#
# Initialize table for model parameters
# (this table not needed in main operation of algorithm
#  -- but may be of later use for analysis)
#
numparams <- 2; # mystery 0.000 and the intercept!
for (i in 1:(numlayers)){
		numparams <- numparams + layerstruc[i] - 1;
	}
modelparams <- array(0, dim=c(numparams,numattributes));


#
# Create a matrix of all combinations of clusters
#
#
# TODO Needs to be replaced by recursive construction that takes as 
# input the layerstruc vector
# DONE! PAOLO 20-06-2012
numclustercombs <- prod(layerstruc);
clustercombs <- array(0,dim=c(numclustercombs,numlayers));
colnames(clustercombs) <- colnames(clustercombs,do.NULL=FALSE,prefix="layer_");
for (i in 1:numclustercombs) {
	current <- i-1;
	thisVec <- array(0,length(layerstruc));
 	for (j in length(layerstruc):1) {
		thisVec[j] <- current %% layerstruc[j]; # %% is the modulus reimander
		current <- current %/% layerstruc[j]; # %/% integer division
 	}
	for (j in 1:length(layerstruc)) {
		clustercombs[i,j] <- paste("c", thisVec[j], sep="")
	}
}

#
# Initialize table for plant occurrence probabilities
#
plantprobs <- array(0.0, dim=c(numclustercombs,numattributes));

#
# Initialize table for probabilities of clusterings
#

clusterprobs <- array(0,dim=c(numclustercombs,numinstances));
# mapclusters <- array(0,dim=c(numinstances));


###
### Begin the iterations
###
terminate <- FALSE;
maxit <- 0;
itcount <- 0;

outputfile <- paste(outputbase,"_iter_",itcount,sep="");

savecolsxml(datatable,(numattributes+1):(numattributes+numlayers), "unknown", outputfile);

while (! terminate){

cat("Iteration", itcount,"\n");

#
# Fit the models:
#

# TODO (?): general ~ layer_1 + layer_2 + ... + layer_n in multinom() command
# PAOLO NOTES:
# layer_i is a column name
# eval(string) function?
# eval(parse(text="command here"))

for (i in 1:(numattributes)){	model <- multinom(formula = datatable[,i] ~ layer_1 + layer_2  ,data=datatable, trace = FALSE);
	modelparams[,i] <- model$wts;
	plantprobs[,i] <- predict(model, clustercombs , type = c("probs"));
}

cat("finished learning","\n");

#
# Compute the log-probabilities of clusterings
#

# oneminusplantprobs <- 1-plantprobs;

for (i in 1:(numclustercombs)){
cat(i," ");
	for (j in 1:(numinstances)){
		clusterprobs[i,j]<-0;
		for (h in 1:(numattributes)){
			clusterprobs[i,j]<-clusterprobs[i,j]+log(datatable[j,h]*plantprobs[i,h]+(1-datatable[j,h])*(1-plantprobs[i,h]));
		}
# TODO: can we find vector-operations that speed things up, not slow things down ...
#	clusterprobs[i,j]<-sum(log( datatable[j,1:numattributes]*plantprobs[i,]+(1-datatable[j,1:numattributes])*(oneminusplantprobs[i,])));
#	clusterprobs[i,j]<-sum(log( (2*datatable[j,1:numattributes]-1)*plantprobs[i,]+1-datatable[j,1:numattributes]))
		}
	}

cat("\n");

#
# Find the most probable cluster values and
# Update cluster assignment
#

numberchanged <- 0;

likelihood <-0;

for (j in 1:numinstances){
	newclusterassignment <- clustercombs[which.max(clusterprobs[,j]),];
# TODO Eliminate hard-coding of 2 clusterings/layers
	if (any(newclusterassignment != datatable[j,(numattributes+1):(numattributes+2)])){
		numberchanged <- numberchanged +1;
	}
	datatable[j,(numattributes+1):(numattributes+2)] <- clustercombs[which.max(clusterprobs[,j]),]
# TODO Deal with overflow of likelihood
	likelihood <- likelihood+max(clusterprobs[,j]);
}

itcount <- itcount +1;

cat("Number changed instances: ",numberchanged,"\n");

if ((itcount > maxit) | (numberchanged==0)){
	terminate <- TRUE;
	}

if (terminate){
outputfile <- paste(outputbase,"_FINAL",sep="");
}
else {
outputfile <- paste(outputbase,"_iter_",itcount,sep="");
}

#
# Save the clustering
#

savecolsxml(datatable,(numattributes+1):(numattributes+numlayers), likelihood, outputfile);

} # end while loop

