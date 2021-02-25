library(foreign)
library(mclogit)
library(survival)
library(lme4)

fly <-  read.spss(file.choose(), use.value.labels=TRUE,to.data.frame=TRUE,max.value.labels=Inf,trim.factor.names=FALSE)
fly <-   as.data.frame(fly)
names(fly) <- tolower(names(fly))
names(fly)
fly1 <- fly[which(is.na(fly$version)==F),]
fly1$taskid <- (fly1$version)*100+(fly1$task)


f1 <- clogit(biofuel ~ att2emissiedekking + att4tijdtotcompensatie + att6extrakosten + concept + strata(id) , data=fly1, method="efron") 
summary(f1)


biofuel2 <- glmer(biofuel ~ (I(scale(att6extrakosten))|id) + I(scale(att2emissiedekking))+ I(scale(att4tijdtotcompensatie)) + I(scale(att6extrakosten))+ concept, data=fly1, family = binomial)
summary(biofuel2)
biofuel2 <- glmer(biofuel ~ (I(scale(att6extrakosten)) + I(scale(att2emissiedekking))+ I(scale(att4tijdtotcompensatie))|id)+ concept, data=fly1, family = binomial)
summary(biofuel2)
hist(ranef(biofuel2)$id[,2]+ fixef(biofuel2)[4])

biofuel2 <- glmer(biofuel ~ (I((att6extrakosten)/1)|id) + I((att2emissiedekking)/100)+ I((att4tijdtotcompensatie)/1) + I((att6extrakosten)/1)+ concept+I(hidden_price/100), data=fly1, family = binomial)
summary(biofuel2)
costre <- (ranef(biofuel2)$id)
costre$id <- row.names(costre)

fly2 <- merge(fly1,costre, by= "id", all = T)
fly2 <- fly2[!is.na(fly2$q25_r2),]
fly2ag<- aggregate(fly2[,c(2:13,17:37,57:58)], by=list(fly2$id),FUN = mean)
fly2ag <- data.frame(fly2ag)
colnames(fly2ag)[36] <- "wtp"
names(fly2ag)
costef <- lm (wtp~q14+ q15 + q17+ hidden_price + q18 +q7, data = fly2ag )
summary(costef)

summary(fly1[,c("att6extrakosten", "att2emissiedekking","att4tijdtotcompensatie", "hidden_price")])

biofuel3 <- glmer(biofuel ~ (I(scale(displayprice))|id) + I(scale(att2emissiedekking))+ I(scale(att4tijdtotcompensatie)) + I(scale(displayprice))+ concept, data=fly1, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(biofuel3)

biofuel4 <- glmer(biofuel ~ (I(scale(displayprice))+I(scale(att2emissiedekking))+ I(scale(att4tijdtotcompensatie))|id) + I(scale(att2emissiedekking))+ I(scale(att4tijdtotcompensatie)) + I(scale(displayprice))+ concept, data=fly1, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(biofuel4)

costre <- (ranef(biofuel4)$id)
costre$id <- row.names(costre)

fly2 <- merge(fly1,costre, by= "id", all = T)
fly2 <- fly2[!is.na(fly2$q25_r2),]
fly2ag<- aggregate(fly2[,c(2:13,17:37,57:58)], by=list(fly2$id),FUN = mean)
fly2ag <- data.frame(fly2ag)
colnames(fly2ag)[36] <- "wtp"
names(fly2ag)
costef <- lm (wtp~q14+ q15 + q17+ q18 +q7, data = fly2ag )
summary(costef)             

biofuel5 <- glmer(biofuel ~ (I((displayprice)/100)+I((att2emissiedekking)/100)+ I((att4tijdtotcompensatie)/10)|id) + I((displayprice)/100) + I((att2emissiedekking)/100)+ I((att4tijdtotcompensatie)/10) + concept, data=fly1, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(biofuel5)


costre <- (ranef(biofuel5)$id)
costre$id <- row.names(costre)

fly2 <- merge(fly1,costre, by= "id", all = T)
fly2 <- fly2[!is.na(fly2$q25_r2),]
fly2ag<- aggregate(fly2[,c(2:13,17:37,57:58)], by=list(fly2$id),FUN = mean)
fly2ag <- data.frame(fly2ag)
colnames(fly2ag)[36] <- "wtp"
names(fly2ag)
costef <- lm (wtp~q14+ q15 + q17+ q18 +q7, data = fly2ag )
summary(costef)             


modelrefly2<- ranef(biofuel5)
hist(modelrefly2$id$`I((displayprice)/100)`)
hist(modelrefly2$id$`(Intercept)`)
plot(modelrefly2$id$`I((displayprice)/100)`,modelrefly2$id$`(Intercept)`)
intercepts <- modelrefly2$id[,1]
slopes <- modelrefly2$id[,2]


eq0 <-  function(x){ 1/(1+exp(-1*((fixef(biofuel5)[1] +intercepts[1]) + (fixef(biofuel5)[2] + slopes[1])*x))) }
plot((eq0), from=0, to =9, xlab="x", ylab="y", ylim = range(0:1) )

for (i in 2: length(intercepts)) {
  eq <-  function(x){1/(1+exp(-1*((fixef(biofuel5)[1] +intercepts[i]) + (fixef(biofuel5)[2] + slopes[i])*x))) }
  plot(eq, from=0, to =9, xlab="x", ylab="y", col=i ,add=T)
} 

probmatrix <- matrix(data=NA, nrow = length(intercepts), ncol=10)

for (j in 1 : ncol(probmatrix)) {
  for (i in 1: length(intercepts)) {
  probmatrix[i,j] <-  1/(1+exp(-1*((fixef(biofuel5)[1] +intercepts[i]) + (fixef(biofuel5)[2] + slopes[i])*(j-1))))
  }}

colMeans(probmatrix)

probmatrix2 <- matrix(data=NA, nrow = length(intercepts), ncol=10)
probmatrix2 <- ifelse(probmatrix > 0.5, probmatrix, NA)
sum(!is.na(probmatrix2[,1]))/620

na_count <-sapply(probmatrix2, function(y) sum(length(which(!is.na(y)))))
na_count <- data.frame(na_count)
na_count

summary(probmatrix)
