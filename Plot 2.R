

set.seed(1)

##Year 13
#sample w/o replacement
train13 = sample(1:nrow(newyear13),nrow(newyear13)*0.7)

tree.13=tree(DRate.1~.,newyear13,subset=train13)

summary(tree.13)

plot(tree.13)
text(tree.13,pretty=0)

cv.13=cv.tree(tree.13)

plot(cv.13$size,cv.13$dev,type='b')

prune.13=prune.tree(tree.13,best=9)

plot(prune.13)
text(prune.13,pretty = 0)

##Year12

#sample w/o replacement
train12 = sample(1:nrow(newyear12),nrow(newyear12)*0.7)

tree.12=tree(DRate.2~.,newyear12,subset=train12)

summary(tree.12)

plot(tree.12)
text(tree.12,pretty=0)

cv.12=cv.tree(tree.12)

plot(cv.12$size,cv.12$dev,type='b')

prune.12=prune.tree(tree.12,best=12)

plot(prune.12)
text(prune.12,pretty = 0)


##Year 11
set.seed(1)

#sample w/o replacement
train11 = sample(1:nrow(newyear11),nrow(newyear11)*0.7)

tree.11=tree(DRate.3~.,newyear11,subset=train11)

summary(tree.11)

plot(tree.11)
text(tree.11,pretty=0)

cv.11=cv.tree(tree.11)

plot(cv.11$size,cv.11$dev,type='b')

prune.11=prune.tree(tree.11,best=13)

plot(prune.11)
text(prune.11,pretty = 0)

## Plot 2

par(mfrow=c(1,3))
plot(prune.13)
text(prune.13,pretty = 0)
plot(prune.12)
text(prune.12,pretty = 0)
plot(prune.11)
text(prune.11,pretty = 0)
