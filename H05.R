# STA 135 H05
# 20130312 HCrockford

library(MASS)
library(rpart)
library(xtable)
library(randomForest)
load("../H04/digitsKnownTest.rda")
load("../H04/digitsTrain.rda")

######
## Random Forest
######

fullrf = randomForest(as.factor(label) ~ ., data = sampleTrain, ntree=30)	# choose 30 - from previous hw gains drop off after this.
conf = rf$confusion[,11]*100
oob = 6.44
names(oob) = "OOB err"

xtab = xtable(round(prop.table(rf$confusion[,-11],margin=2)*100,2), caption = "confusion matrix for Random Forest, ntree = 30")
print(xtab,file = 'confRF.tex')

jpeg('err.jpg')
barplot(c(conf,oob),main="comparison of OOB error rate with class error",ylab = "error (%)",col = c(rep("grey",10),"red"))
dev.off()

imp = rf$importance
impp = transform(vnom = row.names(imp),imp)
impp.order = impp[order(imp,decreasing = TRUE),]
imp.order = imp[order(imp,decreasing = TRUE),]
rel.imp.order = imp.order/sum(imp.order)

jpeg('relplot.jpg')
plot(rel.imp.order,type = "b",main = "relative importance of variables, measured by change in gini")
dev.off()

img = matrix(imp,nrow=28)
image(img,nrow=2)
rotmat = diag(rep(1, times=28))[28:1,]
newimg = img%*%rotmat

jpeg('imp.jpg')
image(newimg,main = "Relative importance of each pixel, as measured by change in gini")
dev.off()

small50 = sampleTrain[,impp.order[1:50,2]] # get 50 most important
rf5030 = randomForest(as.factor(sampleTrain$label) ~ ., data = small50, ntree=30)	# choose 30 - from previous hw gains drop off after this.
rf50 = randomForest(as.factor(sampleTrain$label) ~ ., data = small50)	# choose 30 - from previous hw gains drop off after this.

small100 = sampleTrain[,impp.order[1:100,2]] # get 50 most important
rf10030 = randomForest(as.factor(sampleTrain$label) ~ ., data = small100, ntree=30)	# choose 30 - from previous hw gains drop off after this.
rf100 = randomForest(as.factor(sampleTrain$label) ~ ., data = small100)	# choose 30 - from previous hw gains drop off after this.

system.time(randomForest(as.factor(sampleTrain$label) ~ . data = small50, ntree=30)

res.fullrf = predict(fullrf,test[,-1],class = TRUE)
tab = table(true = test[,1],pred = res.fullrf)

xtab = xtable(round(prop.table(tab,margin=2)*100,2), caption = "confusion matrix for Random Forest, test data, full model")
print(xtab,file = 'conftestRF.tex')




















#######################
## BOOST try
#######################

train = sampleTrain
train.e = eigen(cov(train[,-1]))
train.pca = as.matrix(train[,-1]) %*% train.e$vectors[,1:20]	# transform data to top 20 eigens
dat = cbind(label = as.factor(train$label), data.frame(train.pca))

train.lda = lda(as.factor(label) ~ ., data = dat, CV = FALSE) # LDA doesnent supoport weights!!
train.pred = predict(train.lda, newdata = data.frame(dat[,-1]))


tree4 = rpart(as.factor(label) ~ . , data = train, maxdepth = 4)
test.pred = predict(tree3, newdata = test[,-1],type = "class")
wrong = test$label != test.pred 
sum(wrong)/length(wrong) 	# getting 36%  - dooable.

tree = list()
train.pred = list()
alpha = list()
epsilon = list()
wt =  rep(1/length(train$label),length(train$label)) # init wt.
i=1

wt =  rep(1/length(train$label),length(train$label)) # init wt.
tree = list()
got  = for(i in 1:10){
	tree[[i]] = rpart(as.factor(label) ~ . , data = train, weights = wt,maxdepth = 4)
	train.pred = predict(tree[[i]], newdata = train[,-1],type = "class")
	wrong = train$label != train.pred 
	err = sum(wrong)/length(wrong)
	epsilon = sum(wt[wrong])/sum(wt)       # calc error
	alpha = log((1-epsilon)/epsilon,base = exp(1)) # calc alpha
	wt[wrong] = wt[wrong] * exp(alpha)     # update weights
	print(c(i,alpha,epsilon,err))
 }

table(wt)

tree5 = rpart(as.factor(label) ~ . , data = train, weights = wt,maxdepth = 4)
train.pred = predict(tree3, newdata = train[,-1],type = "class")
wrong = train$label != train.pred 
epsilon = sum(wt[wrong])/sum(wt)       # calc error
alpha = log((1-epsilon)/epsilon,base = exp(1)) # calc alpha
wt[wrong] = wt[wrong] * exp(alpha)     # update weights
return(c(i,alpha,epsilon))

###############################
## RF try
#################

# need to determine best split out of all variables
## split that maximises difference in groups.

# play data

t = data.frame(lab = rep(c(1,0),each = 10),x = c(sample(6:10,10,replace = TRUE),sample(1:8,10,replace = TRUE))) #  best cut should be 7.
mean(t$x)                              # 
pred = 1* (t$x > mean(t$x))            # starting point - work from mean
isBetter = TRUE
while(isBetter == TRUE){

	right2 = sum(t$lab == pred)/length(t$lab)
	if(right2>right){isBetter == TRUE}
	print right2
}

bestcut = function(dat,lab = minitrain$label) {	# return best cut for binary - how do multiple groups?
      scor = sapply(unique(dat), function(i){ 
	     sum((1*dat>i) == unique(lab)*c(TRUE,FALSE))/length(lab)
})
	c(unique(dat)[which.max(scor)],max(scor))
}


hick = subset(train,label %in% c(1,8))
minitrain = hick[1:100,]

bestcut(minitrain[,689],minitrain$label)     # returning cutoff and purity.

res = apply(minitrain[,-1],2,bestcut)






