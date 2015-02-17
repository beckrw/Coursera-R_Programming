pollutantmean <- function(directory, pollutant, id = 1:332) {

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

Aggr_Sample 	<- c()

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
	Variable 		<- Data[pollutant] 
	Aggr_Sample 	<- c(Aggr_Sample, Variable[!is.na(Variable)])
}

Mean_Pollutant 	<- mean(Aggr_Sample)

cat(paste('\nThe mean value of ', pollutant, ' is ', Mean_Pollutant, '.\n\n', sep=''))

Mean_Pollutant


}
