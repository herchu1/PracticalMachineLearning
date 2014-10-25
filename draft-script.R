pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pmldata <- read.csv("pml-training.csv", stringsAsFactors=T, na.strings=c("NA","","#DIV/0!"))
pmlanswers <- read.csv("pml-testing.csv", stringsAsFactors=T, na.strings=c("NA","","#DIV/0!"))


colSums(is.na(pmltrain[,colSums(is.na(pmltrain)) > 0]))
min(colSums(is.na(pmltrain[,colSums(is.na(pmltrain)) > 0])) / nrow(pmltrain))

pmltrainnona <- pmltrain[,colSums(is.na(pmltrain)) == 0]
ignorecols <- c('X','user_name','raw_timestamp_part_1','raw_timestamp_part_2','cvtd_timestamp','new_window','num_window')
pmltrainnona <- pmltrainnona[,!(names(pmltrainnona) %in% ignorecols)]

set.seed(101)
inTrain = createDataPartition(pmldatanona$classe, p = .8)[[1]]
training = pmldatanona[ inTrain,]
testing = pmldatanona[-inTrain,]


set.seed(107)
modfit <- train(classe ~. , data=training, method="rf",
                tuneGrid=data.frame(mtry=1),
                trControl=trainControl(method="none"))
modfit2 <- train(classe ~. , data=training, method="rf",
                 trControl=trainControl(method="none"))

modfit$finalModel

pr <- predict(modfit, newdata=testing)
confusionMatrix(pr, testing$classe)

result <- predict(modfit, newdata=pmltest)


