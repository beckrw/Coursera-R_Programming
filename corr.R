corr <- function(directory, threshold = 0) {

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0
## Return a numeric vector of correlations

Correlations	<- c()

for(id_curr in 1:332) {
	if(id_curr <= 9) {
		ID_As_Str 		<- paste('00', id_curr, sep='')	
	} else if(id_curr <= 99) {
		ID_As_Str 		<- paste('0', id_curr, sep='')
	} else {
		ID_As_Str 		<- paste(id_curr)
	}

	File_Path 		<- paste(directory, '/', ID_As_Str, '.csv', sep='')
	Data 			<- read.csv(File_Path, header=T)
	ind 			<- complete.cases(Data)

	if(sum(ind) > threshold) {
		Correlations 	<- c(Correlations, cor(Data$sulfate[ind], Data$nitrate[ind]))
	}
}

Correlations

}
