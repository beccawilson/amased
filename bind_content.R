#TODO: functionalise
# script to bind content column 


# test data file path
data<-"/home/rw13742/Documents/datashield/amased/data/bl_csv_1_book/data.csv"
data.txt<-"/home/rw13742/Documents/datashield/amased/data/bl_csv_1_book/data.txt"
#1) bind all data in content column into one variable name called text
book1<-read.csv(data)

#order the book according to entity id
book1 <- book1[order(book1$entity_id),] 

test<-book1$content

text<- test[1]

for (i in 2:length(test)){
text = paste(text, test[i], sep = " ")
text
}

#text<- c("GAD", "far", ",", "ten")
#tmp = text [1]
#for (i in 2:length(text)){
#tmp = paste(tmp, text[i], sep = " ")
#tmp
#}

#check data
#head(text)

#write to file
write.table(text, data.txt, col.names = FALSE, row.names = FALSE)


