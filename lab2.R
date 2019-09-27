require('randomForest')
require('MASS')
path = "~/TMDA/tmdaLab2_randomForest/breast-cancer-wisconsin.data"
#path = "~/Escritorio/USACH/Topicos/Taller de mineria de datos avanzada/tmdaLab1/breast-cancer-wisconsin.data"
data =  read.table(path,sep=",", na.strings = c("?"))

names = c('ID','clump','size',
          'shape','adhesion','epithelial',
          'nuclei','chromatin','nucleoli','mitoses','Class')
colnames(data) = names

data$Class[data$Class == "2"] = "Benigno"
data$Class[data$Class == "4"] = "Maligno"
summary(data)

#Se eliminan los datos con campos vacíos
data.without.na = na.omit(data)

#Se elimina la columna ID de cada sujeto
data.2 = data.without.na[,2:11]

smp_size = floor(0.632*nrow(data.2))
set.seed(12345)

train_ind = sample(seq_len(nrow(data.2)),size = smp_size)

data.train = data.2[train_ind,]
data.test = data.2[-train_ind,]
data.2$Class = as.factor(data.2$Class)
#600 arboles es mejor que 700
data.rf = randomForest(Class ~ ., data = data.2, ntree= 600,importance=TRUE, proximity = TRUE)
print(data.rf)

round(importance(data.rf),2)
varImpPlot(data.rf)

data.mds = cmdscale(1-data.rf$proximity,eig=TRUE)
op = par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, 
      gap=0,
      col=c("red", "green")[as.numeric(data.2$Class)],
      main="BCW Data: Predictors and MDS of Proximity Based on RandomForest")


plot(data.rf)
legend("bottomright", colnames(data.rf$err.rate),col=1:4,cex=0.8,fill=1:4)

parcoord(data.2[,1:9],var.label = TRUE,col=c("red", "green")[as.numeric(data.2$Class)])
legend("bottomright",legend = c("Benigno", "Maligno"),fill=2:3)



#data.3 = subset(data.2 , select = -mitoses)
#data.rf.2 = randomForest(Class ~ ., data = data.3, ntree= 600,importance=TRUE, proximity = TRUE)
#print(data.rf.2)
#round(importance(data.rf.2),2)
#varImpPlot(data.rf.2)


#data.4 = subset(data.3,select=-adhesion)
#data.rf.3 = randomForest(Class ~ ., data = data.4, ntree= 600,importance=TRUE, proximity = TRUE)
#print(data.rf.3)
#round(importance(data.rf.3),2)
#varImpPlot(data.rf.3)
#regularizacion:  parsimonia (BIC)
#generalizacion (datos nuevos)



#si ACP no es suficiente, usamos proximidad

#jugar con el nro de arboles y variables
#