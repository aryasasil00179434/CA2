install.packages("readxl")
library(readxl)

data <- read_excel("Dataset_2024.xlsx")
str(data)
# Renaming
colnames(data)[colnames(data) == "Age (years)"] <- "Age"
colnames(data)[colnames(data) == "Body fat (%)"] <- "Body_fat"
colnames(data)[colnames(data) == "Chest circumference (cm)"] <- "Chest_circumference"
colnames(data)[colnames(data) == "Density (g/cm³)"] <- "Density"
colnames(data)[colnames(data) == "Knee circumference (cm)"] <- "Knee_circumference"
colnames(data)[colnames(data) == "Weight (lbs)"] <- "Weight"
str(data)

#Pairplot-Examine initial linearity between variables in the dataset
library(psych)
windows(20,10)
pairs.panels(data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)        # If TRUE, adds confidence intervals

#Examine linearity in more detail using scatter plots

windows(20,16)
par(mfrow= c(3,2))

scatter.smooth(x = data$`Age`,
               y = data$`Body_fat`,
               xlab = "Age in Years",
               ylab = "Body Fat in %", main = "Correlation of Body fat ~ Age")
scatter.smooth(x = data$`Chest_circumference`,
               y = data$`Body_fat`,
               xlab = "Chest circumference in cm",
               ylab = "Body Fat in %", main = "Correlation of Body fat ~ Chest Circumference")

scatter.smooth(x = data$`Density`,
               y = data$`Body_fat`,
               xlab = "Density",
               ylab = "Body Fat in %", main = "Correlation of Body fat ~ Density")

scatter.smooth(x = data$`Knee_circumference`,
               y = data$`Body_fat`,
               xlab = "Knee circumference",
               ylab = "Body Fat in %", main = "Correlation of Body fat ~ Knee circumference")

scatter.smooth(x = data$`Weight`,
               y = data$`Body_fat`,
               xlab = "Weight",
               ylab = "Body Fat in %", main = "Correlation of Body fat ~ Weight")

# Examining correlation between Body Fat and Independent variables
cor(data)
attach(data)
# Examining the other variables
paste("Correlation for Body Fat and Age: ", round(cor(Body_fat, Age),2))
paste("Correlation for Body Fat and Chest_circumference: ", round(cor(Body_fat, Chest_circumference),2))
paste("Correlation for Body Fat and Density: ", round(cor(Body_fat, Density),2))
paste("Correlation for Body Fat and Knee_circumference: ", round(cor(Body_fat, Knee_circumference),2))
paste("Correlation for Body Fat and Weight: ", round(cor(Body_fat, Weight),2))



# Check for outliers
windows(20,10)
par(mfrow = c(3, 2)) # divide graph area in 3 rows by 2 columns

boxplot(Age,
        main = "Age") # box plot for 'Age'
boxplot(Body_fat,
        main = "Body Fat") # box plot for 'Body Fat'
boxplot(Chest_circumference,
        main = "Chest Circumference") # box plot for 'Chest Circumference'
boxplot(Density,
        main = "Density") # box plot for 'Density'
boxplot(Knee_circumference,
        main = "Knee_circumference") # box plot for 'Knee_circumference'
boxplot(Weight,
        main = "Weight") # box plot for 'Weight'


#Body fat,Chest_circumference,Knee_circumference,Weight variables contains outliers.
# Use boxplot.stats() function to generate relevant outliers
#Body Fat
outlier_values <- boxplot.stats(Body_fat)$out # outlier values.
paste("Population outliers: ", paste(outlier_values, sep =", "))


# Remove outliers
#Body Fat
data <- subset(data,data$Body_fat != "47.5")

outlier_values <- boxplot.stats(Body_fat)$out # outlier values.
paste("Body fat outliers: ", paste(outlier_values, sep =", "))


#chest_circumference
outlier_values <- boxplot.stats(Chest_circumference)$out # outlier values.
paste("Chest_circumference outliers: ", paste(outlier_values, sep =", "))

data <- subset(data,
               data$Chest_circumference!= 136.2
               & data$Chest_circumference!=128.3)

outlier_values <- boxplot.stats(Chest_circumference)$out # outlier values.
paste("Chest_circumference outliers: ", paste(outlier_values, sep =", "))


#Knee_circumference
outlier_values <- boxplot.stats(Knee_circumference)$out # outlier values.
paste("Knee_circumference outliers: ", paste(outlier_values, sep =", "))
data <- subset(data,
               data$Knee_circumference != 49.1
               & data$Knee_circumference !=45
               & data $Knee_circumference!=46)
outlier_values <- boxplot.stats(Knee_circumference)$out # outlier values.
paste("Knee_circumference outliers: ", paste(outlier_values, sep =", "))


#Weight
outlier_values <- boxplot.stats(Weight)$out # outlier values.
paste("Weight: ", paste(outlier_values, sep =", "))
data <- subset(data,
               data$Weight != 363.15
               & data$Weight !=262.75)
outlier_values <- boxplot.stats(Weight)$out # outlier values.
paste("Weight: ", paste(outlier_values, sep =", "))



#Check for normality
library(e1071)
windows(30,20)
par(mfrow = c(3,2))

plot(density(data$Age),
     main = "Density plot :Age",
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness : ", round(e1071::skewness(data$Age), 2)))
polygon(density(data$Age), col = "red")

plot(density(data$Body_fat),
     main = "Density plot :body fat",
     ylab = "Frequency", xlab = "body fat",
     sub = paste("Skewness : ", round(e1071::skewness(data$Body_fat), 2)))
polygon(density(data$Body_fat), col = "green")

plot(density(data$Chest_circumference),
     main = "Density plot :Chest_circumference",
     ylab = "Frequency", xlab = "Chest_circumference",
     sub = paste("Skewness : ", round(e1071::skewness(data$Chest_circumference), 2)))
polygon(density(data$Chest_circumference), col = "red")

plot(density(data$Density),
     main = "Density plot :Density",
     ylab = "Frequency", xlab = "Density",
     sub = paste("Skewness : ", round(e1071::skewness(data$Density), 2)))
polygon(density(data$Density), col = "green")

plot(density(data$Knee_circumference),
     main = "Density plot :Knee_circumference",
     ylab = "Frequency", xlab = "Knee_circumference",
     sub = paste("Skewness : ", round(e1071::skewness(data$Knee_circumference), 2)))
polygon(density(data$Knee_circumference), col = "red")

plot(density(data$Weight),
     main = "Density plot :Weight",
     ylab = "Frequency", xlab = "Weight",
     sub = paste("Skewness : ", round(e1071::skewness(data$Weight), 2)))
polygon(density(data$Weight), col = "green")


# Check normality of all variables using normality test

shapiro.test(data$Age)
shapiro.test(data$Body_fat)
shapiro.test(data$Chest_circumference)
shapiro.test(data$Density)
shapiro.test(data$Knee_circumference)
shapiro.test(data$Weight)
# If p-value < 0.05 then variable is not normally distributed
#age and Chest_circumference p value are less than 0.05.its not normally distributed


#Need to transform Age and Chest_circumference
attach(data)
library(MASS)
box_cox_transform <- boxcox(Body_fat~Age)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Age <-(Body_fat^lamda-1)/lamda
normalised_Age
hist(normalised_Age)
shapiro.test(normalised_Age)

#modify data set
data$Age_new <- normalised_Age
shapiro.test(data$Age_new)

#Chest_circumference
box_cox_transform <- boxcox(Body_fat~Chest_circumference)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Chest_circumference <-(Body_fat^lamda-1)/lamda
normalised_Chest_circumference
hist(normalised_Chest_circumference)
shapiro.test(normalised_Chest_circumference)

#modify data set
data$Chest_circumference_new <- normalised_Chest_circumference
shapiro.test(data$Chest_circumference_new)
View(data)

# check multicollinearity
cor(data)

str(data)
attach(data)
model<-lm(Body_fat~Age_new +
            Chest_circumference_new +
            Density +
            Knee_circumference +
            Weight,data = data)
model
# Print the summary of the regression model
summary(model)
#Body_Fat ~ 1.619 -3.136*Age_new + 4.09*Chest_circumference_new -5.170* Density

#Model 2
model_2<-lm(Body_fat~Age_new +
            Chest_circumference_new +
            Density,data = data)
model_2
# Print the summary of the regression model
summary(model_2)
#Body_Fat ~ 1.5955 -3.136*Age_new + 4.09*Chest_circumference_new -0.49* Density

AIC(model)
AIC(model_2)
BIC(model)
BIC(model_2)

#from this we can say that model 1 is best


#Model 1
#residuals normally distributed
shapiro.test(residuals(model))
#residuals diffrent from zero
t.test(residuals(model),mu=0)
#Model 2
#residuals normally distributed
shapiro.test(residuals(model_2))
#residuals diffrent from zero
t.test(residuals(model_2),mu=0)


#Check for multi_collinearity
install.packages("faraway")
library(faraway)
#Model 1
v1 <-vif(model)
v1
#Model 2
v2<-vif(model_2)
v2
