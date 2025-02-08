# Read Su_raw_matrix.txt into a variable called su
su <- read.delim("C:/Users/18102/Downloads/Su_raw_matrix.txt")

# Use mean and sd functions to find mean and standard deviation of Liver_2.CEL column
sumean <- mean(su$Liver_2.CEL)
susd <- sd(su$Liver_2.CEL)

# Use colMeans and colSums functions to get the average and total values of each column
sucolmean <- colMeans(su)
sucolsum <- colSums(su)

# Generate 10000 numbers for the following (mean, sigma) pairs and plot histogram for each
randNums1 <- rnorm(10000, mean = 0, sd = 0.2)
randNums2 <- rnorm(10000, mean = 0, sd = 0.5)
hist(randNums1)
hist(randNums2)








library(ggplot2)

diabetes <- read.csv("C:/Users/18102/Downloads/diabetes_train.csv")

# Read diabetes_train.csv into a variable called diabetes
diabetes <- read.csv("C:/Users/18102/Downloads/diabetes_train.csv")

# Overlaid histograms
print(ggplot(diabetes, aes(x = mass, fill = class)) + geom_histogram(binwidth = 0.5, alpha = 0.5, position = "identity") + ggtitle("Overlaid Histograms for Mass"))

# Interleaved histograms 
print(ggplot(diabetes, aes(x = mass, fill = class)) + geom_histogram(binwidth = 0.5, position = "dodge") + ggtitle("Interleaved Histograms for Mass"))

# Density plots
print(ggplot(diabetes, aes(x = mass, colour = class)) + geom_density() + ggtitle("Density Plots for Mass"))

# Density plots with semitransparent fill 
print(ggplot(diabetes, aes(x = mass, fill = class)) + geom_density(alpha = 0.3) + ggtitle("Density Plots with Semitransparent Fill for Mass"))









library(tidyverse)

# Read the CSV file into a variable called passengers
passengers <- read.csv("C:/Users/18102/Downloads/titanic.csv")

# (a) Drop missing values and summarize the data
print("Summary after dropping missing values:")
print(summary(passengers %>% drop_na()))

# (b) Filter for male passengers
print("First few rows of male passengers:")
print(head(passengers %>% filter(Sex == "male")))

# (c) Arrange the data in descending order of Fare
print("First few rows arranged by descending Fare:")
print(head(passengers %>% arrange(desc(Fare))))

# (d) Create a new column FamSize as the sum of Parch and SibSp
passengers <- passengers %>% mutate(FamSize = Parch + SibSp)
print("First few rows with the new column FamSize:")
print(head(passengers))

# (e) Group by Sex and calculate mean Fare and number of survivors
print("Summary statistics grouped by Sex:")
print(passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived)))






# Calculate the 10th, 30th, 50th, and 60th percentiles of the skin attribute
percentiles <- quantile(diabetes$skin, probs = c(0.10, 0.30, 0.50, 0.60))

print("Percentiles of the skin attribute:")
print(percentiles)