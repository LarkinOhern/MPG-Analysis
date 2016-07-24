###MPG Analysis

###key questions
###Is an automatic or manual transmission better for MPG? Better = more MPG
###Quantify the MPG difference between automatic and manual transmissions-i.e. manual associated with XX additional mpg ctp

###load required packages
library(ggplot2)
library(plyr)


###grab the data

data("mtcars")
str(mtcars)
summary(mtcars$mpg)
head(mtcars)
?mtcars
###key stats
###32 obs of 11 vars including our  of interest

table(mtcars$am)
###19 automatics and 13 manuals so a relativley even split



###get trans type as a factor 
mtcars$am<-factor(mtcars$am)
levels(mtcars$am)<-c("auto","manual")

###Lets add vehicle type as a variable just for fun
mtcars$model<-row.names(mtcars)


plot(mtcars$mpg)
###from the scatter plot mpg is all over the map


hist(mtcars$mpg, breaks = 10)
abline(v=mean(mtcars$mpg), col="red", lwd=2)
###the hist is vaugely normal-a bit weight to the left with mean 20 mpg

g<-ggplot(mtcars, aes(x=am, y=mpg))
g<-g+geom_boxplot(notch = F)+xlab("Transmission Type")+ylab("Fuel Economy (Miles Per Gallon)")+ggtitle("MPG by Transmision Type")+stat_summary(fun.y = "mean",shape=23, size=2, fill="white")
g
###looks like a big difference in both center and spread based on transmission type.  Manual transmissions have higher average mpg and greater variability

###lets arrange in order of mpg
mtcars<-arrange(mtcars, mpg)
g<-ggplot(mtcars, aes(y=mpg, x=seq_along(mpg), color=am))+geom_point()+xlab("Index")
g

###lets add the models for fun 
g<-ggplot(mtcars, aes(y=mpg, x=reorder(model, mpg, sum), color=am))+geom_point()+xlab("Index")+theme(axis.text.x=element_text(angle=70, hjust=1, vjust=1))
g

###rather than a standard scatterplot-I've arranged by mpg and we can see that thse at the low end are almost all automatics 


###looks like pretty good initial evidence that transmision type does matter and that manual is associated with greater fuel economy.  But we haven't explored other variables.

###Ho: Mean(mpg|auto)-mean(mpg|manual)=0
###Ha: Mean(mpg|auto)-mean(mpg|manual)!=0

### We could do a quick t test to see if the difference is signficant
auto<-subset(mtcars, am=="auto")
manual<-subset(mtcars, am=="manual")

g<-ggplot(mtcars, aes(x=mpg))
g<-g+geom_histogram(fill="white", color="black", binwidth = 3)+facet_grid(am~.)
g

h<-ggplot(mtcars, aes(x=mpg, fill=am))
h<-h+geom_histogram(position = "identity", alpha=0.3, binwidth = 3)
h

i<-ggplot(mtcars, aes(x=mpg, fill=am))+geom_density(alpha=.3)


          
          
test<-t.test(auto$mpg, manual$mpg, paired = FALSE)

test$conf
test$p.value
###0 not included in confidence interval and pvalue less than .05 good evidence that theobserbved diffrence is statsically signficant 


###now lets fit a linear model with mpg as the outcome and transmision type as the predictor 

fit<-lm(mpg~am, mtcars)
summary(fit)

###the coeffcient on tranmission type 7.245 is statistically signficant and implies that all else being equal a car with manual transmision wil get 7.245 mpg more than one with an automatic 
###transmission.  The R2 indicates that transmission types explains 35.8% of the observed variation in fuel economy. 

plot(fit$residuals, col=mtcars$am)
plot(mtcars$am, fit$residuals)

ggplot(fit$residuals)

fitted(fit)

###diagnostics-lets check on our model
plot(fit)

####now lets try all the vars
fitall<-lm(mpg~., data=mtcars)
summary(fitall)$coef

###diagnostics
plot(fit)

###the only thing signficant at all (at .1 is wt)

###interactions
###wt am and wt:m all sig at the .01 level r 2 .833

fitwtamint<-lm(mpg~am+wt+am:wt,mtcars)
plot(fitwtamint$residuals)
summary(fitwtamint)

plot(fitwtamint)