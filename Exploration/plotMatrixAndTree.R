setwd('C:\\Dropbox\\Wang-hy\\!XGBOOST')

#install.packages('xgboost')
library('xgboost')
mymodel=readRDS('model_0413_v2.RDS')

dat=as.matrix(read.csv('my_train_data.csv') )
testdat=as.matrix(read.csv('my_test_data.csv'))


#########################################
# fit glm with top 16 features in the plot
top16features=as.character(read.csv('numberOfTimesInTop10Nodes.csv')[1:16,1])
colnames(X_train)
xpart=X_train[,top16features]
#xpart[1:20,]

glm1=glm(y_train~., family='binomial', data=data.frame(y_train, xpart))
temp=coef(glm1)
ord1=rev(order(abs(temp)))
coefglm=temp[ord1]
coefglm16=data.frame(coeff=coefglm, oddsRatio=exp(abs(c(coefglm[1:2]*0.01, coefglm[-(1:2)]) ) ))#
write.csv(coefglm16, file='coef_glm_16only.csv')


###############################
# plot the importance of the top features
importance_matrix = xgb.importance(colnames(dat[,-1]), model = mymodel)
keep= importance_matrix$Frequency >0.01
#par(mar=c(4,14, 3,1))
#xgb.plot.importance(importance_matrix[keep,], rel_to_first = TRUE, xlab = "Relative importance")

install.packages('Ckmeans.1d.dp')
library('Ckmeans.1d.dp')
library('ggplot2')
(gg <- xgb.ggplot.importance(importance_matrix[keep,], 
                             measure = "Frequency", rel_to_first = T))
gg + ggplot2::ylab("Frequency")
ggsave('importancePlot.wmf', height=5.5, width=11)


####################### ################# ################# 
## Plot precision, recall, and accuracy

oldpred=predict(mymodel, testdat[,-1])
testY=as.factor(testdat[,1] )

# install.packages('caret')
library(caret)
metrics=function(oldpred, testY ){
  res=NULL
  for (cutoff in seq(0.2, 0.6, by=0.02) ){
    oldpredY =as.factor(ifelse(oldpred >= cutoff, 1, 0))
    resultsO = confusionMatrix(oldpredY, testY, positive = '1')
    accold <- round(resultsO$overall['Accuracy'], 3)
    recold <- round(resultsO$byClass['Recall'], 3)
    precold <- round(resultsO$byClass['Precision'], 3)
    res=rbind(res, c(cutoff, accold, recold, precold))
  }
 res
}

oldpredTrain=predict(mymodel, dat[,-1])
trainY=as.factor(dat[,1] )
resTrain=metrics(oldpredTrain, trainY)

plotMetric=function(resTrain, filen='perfTrain.pdf', type='training'){
  pdf(filen)
  colos=c('blue','black',  'red'); ltys=c(2,1,3)
  par(mar=c(4, 4, 3, 1))
  matplot(x=resTrain[,1], y=resTrain[,-1], type='l', xlab='Cutoff',
        ylab='Precision, Recall, Accuracy', lty=ltys, font.lab=2, font=2,
        col=colos, main=paste('Performance for', type, 'data'), lwd=2)
  legend(x=0.38, y=0.88, legend=c( 'Accuracy','Recall','Precision'),
       lty=ltys, col=colos, text.col=colos )
  dev.off()
}

plotMetric(resTrain, 'perfTrain.pdf', 'training')

oldpredTest=predict(mymodel, testdat[,-1])
resTest=metrics(oldpredTest, testY)
plotMetric(resTest, 'perfTest.pdf', 'test')


#######################################################
## plot the first tree

install.packages('Rcpp')
install.packages('rlang')
install.packages('glue')
install.packages('tibble')
install.packages('processx')
install.packages('backports')
install.packages('fs')
devtools::install_github('rich-iannone/DiagrammeRsvg')

library(DiagrammeR)

gr=xgb.plot.tree(model = mymodel,trees=0, render=FALSE)  
export_graph(gr, 'tree1.pdf', width=1500, height=1900)
#gr2 = myxgb.plot.multi.trees(model=mymodel, features_keep = 2, render=FALSE)
#export_graph(gr2, 'treeM.pdf', width=1500, height=1900)
#
#The "Yes" branches are marked by the "< split_value" label.

#Cover: The sum of second order gradient of training data classified to the leaf.
#      If it is square loss, this simply corresponds to the number of instances seen by a split
#      or collected by a leaf during training.
#       The deeper in the tree a node is, the lower this metric will be.
#Gain (for split nodes): the information gain metric of a split
#      (corresponds to the importance of the node in the model).
#Value (for leafs): the margin value that the leaf may contribute to prediction.

###########################################################
## Find the number of times a feature is used in top 10 nodes of all 64 trees.
## Get features used most often in early split of trees. 
## Note: The most important feature to differentiate the response variable 
##       generally appears at the root node (node 0). As we go down a tree, 
##       the importance reduces. 

dt_tree= xgb.model.dt.tree(model = mymodel)

# top 10 internal nodes from all 64 trees
top10nodes=dt_tree[(dt_tree$Node <15)&(dt_tree$Feature !='Leaf'), ] 
write.csv(top10nodes, file='top15nodes.csv')

tempp=table(top10nodes$Feature)
#names(tempp)[rev(order(tempp)) ]
feature=factor(top10nodes$Feature, levels= names(tempp)[rev(order(tempp)) ], ordered =T)
tabu=tapply(rep(1, nrow(top10nodes)), list(top10nodes$Node, feature),sum)
write.csv(t(tabu), file='numberOfTimesInTop15Nodes.csv')

reportImportance=t(tabu[,1:which(colnames(tabu)=='APR')])
reportImportance

#                                                            0  1  2  3  4  5  6  7  8  9
#days_ratio_notice_intent_cancel_to_end                      7 10 19  9 12 13  5  6  8 12
#payments_left_percent                                      36  8  8  4  1 21  8  2  2  1
#Default_Charge                                             12 15  9  1  1  2 10  1  4  1
#Late_Charge                                                NA  3 11  3 13  3  7 NA  3  4
#Borrower_Reg_Cancel_Warning_Date_number_days_intent_notice  1  3  5 NA  3  1  3  6  3  6
#Mature_Date_number                                         NA  7 NA  2  1  7 NA  3  3  2
#Borrower_Registered_Web_Date_days_to_accept                 2  2  5  1  4  3 NA  2  4  1
#FinChg                                                     NA NA NA  3  2 NA  4  2  6  5
#AmtFin                                                     NA NA NA  2  2  1  3  1  1 11
#Borrower_CreditScore                                       NA  1  3  2  2  4  2  1  2  3
#Term                                                       NA NA NA  1  8 NA  1  6 NA NA
#Borrower_EForm_Date_days_to_accept                          6  1  2  2  3  1 NA NA  1 NA
#Down                                                       NA NA  1 NA  1  2  5  3  1  2
#Exposure                                                   NA NA NA  5 NA NA  2  2  2  1
#NSF_Charge                                                 NA  2 NA  4 NA  1  2  2 NA NA
#APR                                                        NA NA  1 NA  3 NA  4 NA  1  2



########################
