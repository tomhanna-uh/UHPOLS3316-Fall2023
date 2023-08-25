rm(list=ls())

#Set your working directory
setwd("C:/R Studio Files/Teaching/POLS6480-Fall2020-UH-lab/Lab 10")

##The project today uses data on Tom Cruise movies. The goal is to predict box office
##sales based on Rotten Tomatoes reviews. There is other data included in the data set
##including The dataset also includes the year in which the movie was released, the genre, 
##the MPAA rating, and Tom Cruise’s character and role in the movie.

##We're going to start by reading in the data

data <- read.csv("Cruise.csv")

#I've also added a line to show the variable names - this just makes it easier to refer back
#to know what data is available and what it's actually called

names(data)

#Now we're going to subset the data to include include movies where Tom Cruise had the 
#leading role using the "Role" variable. U

str(data$Role)

#Using the str command, we can see what the variable values
#looks like and see that we're looking for the value "Lead"

leading <- data[data$Role == "Lead", ]

#The next two lines first create a "count" of movies by the variable "Rating"
#from the new "leading" dataset. Then we plot those counts in a bar plot.

counts <- table(leading$Rating)
barplot(counts, ylab="Number", xlab="Rating") 



##Next we're going to create a boxplot of the Domestic box office receipts, variabl "Domestic"
#based on the MPAA rating

boxplot(leading$Domestic ~ leading$Rating, horizontal=TRUE)

#And we're going to run an ANOVA test to see whether the three categories,
#which definitely overlap in the box plots, are significantly different.

anova <- aov(Domestic ~ Rating, data=leading)
summary(anova)

#Does rating have a significant effect on domestic box office?

##Moving along, we're going to see how the Rotten Tomatoes "freshness" rating, 
#variable "Freshness" impacts box office revenues

#First, a histogram to give us the frequency distribution for the Freshness ratings

hist(leading$Freshness)

#and a scatter plot of ratings versus reevenues

plot(leading$Domestic ~ leading$Freshness, pch=19)

#Do higher ratings seem to have any effect on revenues?

#Rather than a t-test or ANOVA, we're going to move into something you are going to see a lot of 
#for the rest of your career, an OLS regression. I know you've done some of these for your homeworks,
#but for anyone who needs to step back a bit and understand this, I'm going to go through this element
#by element. 

reg.simple <- lm(Domestic ~ Freshness, data=leading)
?lm
summary(reg.simple)


#reg.simple is just the model name we assign - it can be anything, as long as it isn't a reserved value
#It's a good idea to check the style guide from Google when naming models, just for clarity.
#The <- is just the command to assign a formula to the model name
#lm is the command to run a "linear model," which at its simplest is the ordinary least squares model.
#The ~ is the = sign in the regression equation "y = intercept + beta*x + error" becomes 
#"y ~ x" and the computer then figures the intercept, beta, and error.

#This comment shows the intercept and beta coefficitens only.

reg.simple$coefficients

#we can plot this

plot(leading$Domestic ~ leading$Freshness, pch=19); abline(reg.simple)

names(summary(reg.simple)) #returns the elements of the model summary - coefficients are #4
summary(reg.simple)[4] #This command returns the summary for both coefficients
names(reg.simple) #This returns the names of the model elements
reg.simple[8] #8 is the degrees of freedom
reg.simple[1]
t.critical = qt(.975,31, lower.tail = TRUE) 

summary(reg.simple)

#with the degrees of freedom and the t-score, we can get the significance of the coefficents

#after completing all this go back to the formula for reporting results: direction, magnitude (amount), 
#and significance 

#To summarize, the effect of increasing the Freshness rating of a Tom Cruise movie
#is to increase the expected domestic box office by _____ million dollars, 
#and this relationship _____ (is/is not) statistically significant at the 95% confidence level.

View(leading) #let's view the data quickly

#I notice 4 things that dominate the top 5: the franchise, the characeter, the year, and the MPAA rating

#What if we think the year has something to do with things

cor(leading$Freshness, leading$Year)
cor(leading$Domestic, leading$Year)

reg.multiple <- lm(Domestic ~ Freshness + Year, data = leading)
reg.multiple$coefficients
summary(reg.multiple)[4]
summary(reg.simple)[4]

#What about controlling for inflation
#fortunately we have a handy dandy dataset right here to help us

cpidata <- read.csv("cpi1983.csv") #loads the inflation data
combined <- merge(leading, cpidata, by=Year") #creates a new dataset name "combined" by using the "merge"
#                                               command and sorting 'by = "Year"' so it will fill in the 
#                                               CPI data for a particular year everywhere that the same year
#                                               is in the first dataframe "leading."

View(combined)

##The next two lines adjust the revenue for inflation 
#in the first line, we divide the domestic revenue by that years
#Consumer Price Index (CPI) - the 10,000 in the denominator is 
#a convenience to make the scale of the totals more manageable

combined$constant = combined$Domestic/(10000*combined$Annual) 
cor(combined$constant, combined$Year)

#we can run the regression on the inflation adjusted numbers

reg.new <- lm(constant ~ Freshness, data=combined)
reg.new$coefficients
summary(reg.new)[4]
reg.new[8]
t.critical = qt(.975,31, lower.tail = TRUE)

#the next line uses the 'predict' command to predict the revenues for some 
#hypothetical freshness ratings

predict(reg.new, data.frame(Freshness = c(33, 66, 100))) #Freshness = c assigns three values to check
?predict

#Waht if we add in the MPAA ratings? Something kind of interesting happens. First, I'm going to
#run a summary on the reg.new model coefficients

summary(reg.new)[4]

reg.mpaa <- lm(constant ~ Freshness + Rating, data=combined)
summary(reg.mpaa)[4]

summary(reg.mpaa)

rm(list=ls())