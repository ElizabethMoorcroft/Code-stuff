#--------------------------------
# Project title: Snow leopards
#
# Elizabeth Moorcroft: Created: 19 August 2014
# 
# Script title: Snow Leopard Selecting combinations
# Purpose: Selects the different combinations and 
#			runs though small programs based on sub sample
#--------------------------------

# Load Data
length.of.list<-length(row.numbers)
vector.of.numbers<-1:length.of.list

# All possible combinations from the list of data
possible.combinations<-combn(1:length.of.list,3)
length.of.training.data<-c()
length.of.validation.data<-c()

# Calculates the amount of data in the training & validation data
for(i in 1:dim(possible.combinations)[2]){
	validation.combinations<-possible.combinations[,i]
	training.combinations<-vector.of.numbers[!(vector.of.numbers %in% possible.combinations[,i])]
	
	training.info<-sum(unlist(lapply(row.numbers[training.combinations],length)))
	validation.info<-sum(unlist(lapply(row.numbers[validation.combinations],length)))
	
	length.of.training.data<-c(training.info,length.of.training.data)
	length.of.validation.data<-c(validation.info,length.of.validation.data)
}



