complete <- function(directory, id = 1:332) {

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
## Return a data frame of the form:
## id nobs
## 1 117
## 2 1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

nobs			<- c()

for(id_curr in id) {
	if(id_curr <= 9) {
		ID_As_Str 		<- paste('00', id_curr, sep='')	
	} else if(id_curr <= 99) {
		ID_As_Str 		<- paste('0', id_curr, sep='')
	} else {
		ID_As_Str 		<- paste(id_curr)
	}

	File_Path 		<- paste(directory, '/', ID_As_Str, '.csv', sep='')
	Data 			<- read.csv(File_Path, header=T)
	nobs 			<- c(nobs, sum(complete.cases(Data)))
}

Result		<- data.frame(id, nobs)

}
