rm(list=ls())

#Setting your working directory allows you to call the two files you'll need without putting
#the full file path
setwd("C:/R Studio Files/Teaching/POLS6480-Fall2020-UH-lab/Lab 7 and lab 8")

#reads in the cereal.csv file - you'll need to either change C:/cereal.csv to match your file directory
#or set your working directory and delete the C:/
cereal <- read.csv("cereal.csv"); attach(cereal) 

#subsets into a dataframe with just children's cereal and one with just adult cereal
children <- cereal[Intended.for == "Children", ]
adults <- cereal[Intended.for == "Adults", ]


#2. Let us shift our attention to summary statistics, focusing on how many grams of sugar are contained in a serving of each cereal. To find the average and the standard deviation in each sample, type the following two lines of code:
m1 <- mean(children$Sugar); s1 <- sd(children$Sugar)

m2 <- mean(adults$Sugar); s2 <- sd(adults$Sugar)

#: In the sample, do children’s cereals and adults’ cereals have similar average sugar content?

#In the sample, do children’s and adults’ cereals have similar variability in their sugar content?

#We can also look at this visually with a histogram

par(mfrow=c(2,1))
hist(children$Sugar, seq(0,16,2), main="")
hist(adults$Sugar, seq(0,16,2), main="")
par(mfrow=c(1,1))

#For the next part we need to create a variable to represent the n for each of the sets

n1 <- length(children$Sugar)
n2 <- length(adults$Sugar)

#To get the t-stat we need the difference of means
diff.means = m1-m2

#We also need the combined standard error 
#So this is not your father's standard error - we're comparing two datasets (or subsets)
#So we get this combined standard error with this formula

se.welch = sqrt((s1^2/n1)+(s2^2/n2))

#The t-stat is the difference of means divided by the standard error

t.welch = diff.means/se.welch

#To use the t-test on the table we need to know the degrees of freedom

A=s1^2/n1; B=s2^2/n2
df.welch <- (A+B)^2/(A^2/(n1-1)+B^2/(n2-1))

#Now we have what we need to find significane
#You remember pnorm, qnorm, rnorm, etc.? Now we have the same things for Student's T-Distribution
?qnorm

?qt

#So we will find the critical t-value (We could also find this with a t-table)
t.critical <- qt(0.975, df.welch)

##QUESTION - Without running the next line of code
#Is the T-value we computed earlier (t.welch) significant at the .05 level? 

#Or, we can compute the p-value directly in R
p <- 2*(1-pt(t.welch,df.welch))

#we use .975 rather than .95 and we have to multiply by 2
#because R automatically assumes we are looking at one tail. 

#Next, we are going to use R's built-in t test command and see what we get
#First, look at the help file
?t.test

#This code runs a t-test using the adults and childrens subsets
t.test(children$Sugar, adults$Sugar, alternative="two.sided")

#This code runs a t-test using the main datafram (cereal) and testing the 
#the two options in the character vector "Intended.for"
t.test(cereal$Sugar ~ cereal$Intended.for)

#clearing the data
rm(list=ls())

#Setting your working directory allows you to call the two files you'll need without putting
#the full file path
setwd("C:/R Studio Files/Teaching/POLS6480-Fall2020-UH-lab/Lab 7 and lab 8")

#A study reported by Brian Everett (University College, London) examined the effectiveness of two
#different treatments for anorexia. Family therapy stresses the social causes of eating disorders 
#and attempts to achieve desirable behavior by improving a family’s norms. Cognitive behavioral 
#therapy stresses the psychological causes of eating disorders and attempts to achieve desirable 
#behavior by retraining and focusing the individual’s mental powers.

#read in the data from anorexia.csv and create a dataframe from it called "experiment"





experiment <- read.csv("anorexia.csv")




#We're subsetting the data by treatment type
#family therapy - f
#cognitive behavioral therapy - b
#and a control group -  c
treatment.b <- experiment[experiment$therapy == "b", ]
treatment.f <- experiment[experiment$therapy == "f", ]
control  <- experiment[experiment$therapy == "c", ]

#We're going to look at some graphic presentation of the data
#first using the subsets
boxplot(treatment.f$after,treatment.b$after,control$after, horizontal=TRUE, 
        names=c("Family","Behavioral","Control"))

#Now using the original dataset
boxplot(experiment$after ~ experiment$therapy, horizontal = TRUE)

#the order of presentation is different, but the results are the same

t.test(treatment.f$after, control$after)
t.test(treatment.f$after, control$after, alt="greater")

t.test(treatment.f$after, treatment.f$before, alt="greater")
treatment.f$delta <- treatment.f$after - treatment.f$before
cor(treatment.f$after, treatment.f$before)
plot(treatment.f$before, treatment.f$after, pch=19, xlim=c(70,100), ylim=c(70,110))
abline(lm(after~before, data=treatment.f)); abline(a=0,b=1, col="grey")
t.test(treatment.f$delta, mu=0, alt="greater")

t.test(treatment.f$after, treatment.f$before, alt="greater", paired = TRUE)

control$delta <- control$after - control$before
cor(control$after, control$before)
plot(control$before, control$after, pch=19, xlim=c(70,100), ylim=c(70,110))
abline(lm(after~before, data=control)); abline(a=0,b=1, col="grey")

t.test(treatment.f$delta, control$delta, alt="greater")




rm(list=ls())