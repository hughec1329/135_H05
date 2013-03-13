# STA 135 H05
# 20130312 HCrockford

library(MASS)
library(rpart)
load("../H04/digitsKnownTest.rda")
load("../H04/digitsTrain.rda")

train = sampleTrain
train.e = eigen(cov(train[,-1]))
train.pca = as.matrix(train[,-1]) %*% train.e$vectors[,1:20]	# transform data to top 20 eigens
dat = cbind(label = as.factor(train$label), data.frame(train.pca))

train.lda = lda(as.factor(label) ~ ., data = dat, CV = FALSE) # LDA doesnent supoport weights!!
train.pred = predict(train.lda, newdata = data.frame(dat[,-1]))


rpart

table(actual = train$label, pred = train.pred$class)

alphas =  rep(1/length(train$label),length(train$label))
alphas[train$label != train.pred$class] = 


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

bestcut = function(dat,lab) {	# return best cut for binary - how do multiple groups?
	unique(dat)[
		    which.max(
			      sapply(unique(dat), function(i) 
				     sum((1*dat>i) == lab)/length(lab) 						)
			      ) 
		    ]
}


minitrain = train[100,]

bestcut(train[,3],train$label)





