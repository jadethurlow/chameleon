#packages needed
install.packages("smatr")
library(smatr)
install.packages("paran")
library(paran)

chameleon<-Hawaii_guts_gonads_cleaned_1_
chameleon<-as.data.frame(chameleon)

sex<-chameleon$sex
svl<-chameleon$SVL
h.width<-chameleon$headwidth
j.length<-chameleon$jawlength
t.length<-na.omit(as.numeric(chameleon$tail))

#transform
cham<-na.omit(chameleon[,-c(1,2)])
cham.2<-cbind(cham$SVL,cham$tail,cham$headwidth,cham$jawlength,cham$casqheight,cham$snoutcasque,cham$snoutcasque2,cham$righthorn,cham$righthorn2,cham$lefthorn,cham$lefthorn2,cham$bottomhorn,cham$bottomhorn2)
cham.2<-log(matrix(as.numeric(cham.2),nrow(cham.2),ncol(cham.2)))
colnames(cham.2)=colnames(cham)[2:ncol(cham)]
cham<-data.frame(cham$sex,cham.2)
cham<-na.omit(cham)

#K-S test
ks.test(cham$SVL,rnorm(38*1)) 

#sma assumptions 
y<-qnorm(rank(cham$SVL)/(length(cham$SVL)+1))
hist(y,col='darkgreen',main=NA,xlab='rank SVL')

hist(log(h.width),main=NA,xlab='head width (mm)',col='red')
hist(log(svl),main=NA,xlab='snout-vent length (mm)',col='green')
hist(log(t.length),main=NA,xlab='tail length (mm)',col='purple')
hist(log(j.length),main=NA,xlab='jaw length (mm)',col='orange')
hist(log(chameleon$casqheight),main=NA,xlab='casque height (mm)',col='yellow')
hist(log(chameleon$snoutcasque),main=NA,xlab='snout casque (mm)',col='dark green')
hist(log(chameleon$snoutcasque2),main=NA,xlab='snout casque (mm)',col='blue')


#sma
for(i in 3:7){
  print(summary(sma(cham[,i]~cham$SVL+cham$cham.sex)))
}

#plot regression
plot(cham$SVL,cham$tail,xlab='SVL (mm)',ylab='Tail Length (mm)')
w<-which(cham$sex=="M")
length(w)
points(cham$SVL[w],cham$tail[w], col="white", pch=21, bg="blue")
w<-which(cham$sex=="F")
length(w)
points(cham$SVL[w],cham$tail[w], col="white", pch=21, bg="orange")
legend(4.7,4.0,legend=c('F','M'),col=c("orange","blue"),border='black',pch=19,title="Sex")


#pca
#male and female 
cham.matrix<-matrix(as.numeric(unlist(chameleon[,4:7])),nrow=nrow(chameleon),ncol=4)
cham.matrix<-sqrt(na.omit(cham.matrix))
summary(princomp(cham.matrix,scores=TRUE))
mf.pca.scores<-princomp(cham.matrix)$scores

#plot scores (male and female)
plot(mf.pca.scores,col='black',bg='red',pch=21,ylim=c(-1,1),xlab="PC1",ylab="PC2")

#male only
cham.matrix<-cham[cham$cham.sex == "M",]
summary(princomp(cham.matrix[,-1],scores=TRUE))
m.pca.scores<-princomp(cham.matrix[,-1])$scores

#do paran test to see which principal components are significant 
paran(cham.matrix[,-1])

#plot scores (males)
plot(m.pca.scores,col='black',bg='pink',pch=21,ylim=c(-1,1),xlab="PC1",ylab="PC2") 

#plot PC1 and PC3 of males
plot(m.pca.scores[,1],m.pca.scores[,3],cex=cham.matrix[,7]/5,col=hsv(h=cham.matrix[,7]/6),pch=19,xlab="PC1",ylab="PC3")
