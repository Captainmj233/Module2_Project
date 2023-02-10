# Module2_Project
print("Plotting Basics:Kevin Appiah") #Prints name at the top of the script including the prefix : "Plotting Basics:"
    install.packages("plyr") install.packages("FSA") install.packages("FSAdata") install.packages("magrittr") install.packages("dplyr") install.packages("plotrix")
#Installs plyr pacakge #Installs FSA package
#Installs FSAdata package #Installs magrittr package
#Installs dplyr package #Installs plotrix package
install.packages("ggplot2") install.packages("moments")
#Installs ggplot2 package #Installs moments package
#Imports the libraries library(plyr) library(FSA) library(FSAdata) library(magrittr) library(dplyr) library(plotrix) library(ggplot2) library(moments)
data(BullTroutRML2)
#Loads BullTroutRML2 dataset
headtail(BullTroutRML2, n=3) #Prints the first and last 3 records from the BullTroutRML2 dataset
filtered_data<- filter(BullTroutRML2, lake !="Harrison") #Filters out all records except those from Harrison Lake
headtail(filtered_data, n=3) #Prints the first and last 3 records from the filtered dataset
str(filtered_data) #Displays the structure of the filtered dataset

t = summary(filtered_data) #Produces the summary of the filtered dataset t
#Creates a scatter plot for age y (variable) and fl(x variable) with specifications in the instruction.
plot(age~fl, data = BullTroutRML2, ylim = c(0,15), xlim = c(0,500), main = "Plot 1: Harrison Lake Trout", ylab = "Age (yrs)", xlab = "Fork Length (mm)", pch = 20)
#Plots an Age histogram
hist(BullTroutRML2$age, ylab = "Frequency", xlab = "Age (yrs)", main = "Plot 2: Harrison Fish Age Distribution", col = "cadetblue", col.main = "cadetblue")
level <- as.factor(BullTroutRML2$era) level
#Creates an overdense plot
plot(age~fl, data = BullTroutRML2, ylim = c(0,15), xlim = c(0,500), main = "Plot 3: Harrison Density Shaded by Era", ylab = "Age (yrs)", xlab = "Fork Length (mm)", pch = 20, col =c("black", "gray60")[level])
tmp = headtail(BullTroutRML2, n=3) #Creates a new object called "tmp" that includes the first and last 3 records of the whole dataset.
tmp

tmp$era #Displays the "era" column in the new "tmp" object
#Creates a pchs vector with the argument values for + and x. Then creates a cols vector with thetwo elements "red" and "gray60"
pchs <- c("+", "x")
cols <- c("red", "gray60")
numLake <- as.numeric(tmp$lake) #Converts tmp object values to numeric values
numEra <- as.numeric(tmp$era) #Creates a numeric numEra object from the tmp$era object
#Associates the cols vector with the tmp era values era_cols <- cols[numEra]
era_pchs <- pchs[numEra]
#Creates a plot of "Age(yrs)"(y variable) versus fork "Fork Length (mm)"(x variable) plot(age~fl, data = tmp, ylim = c(0,15), xlim = c(0, 500), main = "Plot 4: Symbol & Color by Era", xlab = "Fork Length (mm)", ylab ="Age (yrs)", pch = era_pchs, col = era_cols)
#Plots a regression line of the previous plot with a dashed line with width 2 and color "cadetblue"
regLine <- lm(age~fl, data = tmp)
regLine
abline(regLine, col = "cadetblue", lty = 2, lwd = 2)

#Places a legend of levels by era with pchs symbols in the top left of the plot with specifications in the instruction.
legend("topleft", inset = 0.05, legend = c(1,2), pch = pchs, col = cols, bty = "n", cex = 0.75)
