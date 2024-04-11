chameleon<-Hawaii_guts_gonads_cleaned_1_
chameleon<-as.data.frame(chameleon)

summary(chameleon)
head(chameleon)


#want to look at changes in sex across SVL, jawlength, headwidth
#comparing categorical variable against 3 response variables 

sex<-chameleon$sex
svl<-chameleon$SVL
h.width<-chameleon$headwidth
j.length<-chameleon$jawlength

hist(h.width) #normal dsitribution 
hist(svl)
hist(j.length)

plot(svl,sex, ylim=, xlab='svl', ylab='sex')

aov(sex~svl)

#need to do: ignore NAs, fix plot error
#trial anova 