rm(list = ls())


library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)


df <- read.csv("D:\\NEO Earth Close Approaches.csv")

print(head(df))
str(df)
summary(df)
dim(df)
sapply(df, class)
sapply(df, n_distinct)


missing_counts <- colSums(is.na(df))
print(missing_counts)


library(ggplot2)  
library(tidyr)  

set.seed(123)  
df <- data.frame(CA.DistanceNominal..au. = rnorm(1000, mean = 50, sd = 1))  


mean_value <- mean(df$CA.DistanceNominal..au.)  
median_value <- median(df$CA.DistanceNominal..au.) 
mode_value <- df$CA.DistanceNominal..au.[which.max(table(df$CA.DistanceNominal..au.))]

# Adjust the y position of annotations as needed
ggplot(df, aes(x = CA.DistanceNominal..au.)) +   
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "lightblue", color = "black", alpha = 0.6) +   
  geom_density(color = "orange", size = 1) +   
  
  geom_vline(aes(xintercept = mean_value), color = "blue", linetype = "dashed", size = 1) +  
  geom_vline(aes(xintercept = median_value), color = "green", linetype = "dashed", size = 1) +  
  geom_vline(aes(xintercept = mode_value), color = "red", linetype = "dashed", size = 1) +  
  
  annotate("text", x = mean_value, y = 0.43, label = "Mean", color = "blue", vjust = -1) +  
  annotate("text", x = median_value, y = 0.35, label = "Median", color = "black", vjust = -1) +  
  annotate("text", x = mode_value, y = 0.25, label = "Mode", color = "red", vjust = -1) +  
  labs(title = "Histogram of Minimum Close-Approach Distance",   
       x = "Minimum Distance (au)",   
       y = "Density") +  
  theme_minimal()





library(ggplot2)  
library(tidyr)  

set.seed(123)  
df <- data.frame(CA.DistanceMinimum..au. = rnorm(1000, mean = 55, sd = 1))  


mean_value <- mean(df$CA.DistanceMinimum..au.)  
median_value <- median(df$CA.DistanceMinimum..au.) 
mode_value <- df$CA.DistanceMinimum..au.[which.max(table(df$CA.DistanceMinimum..au.))]


ggplot(df, aes(x = CA.DistanceMinimum..au.)) +   
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "lightblue", color = "black", alpha = 0.6) +   
  geom_density(color = "orange", size = 1) +   
  
  geom_vline(aes(xintercept = mean_value), color = "blue", linetype = "dashed", size = 1) +  
  geom_vline(aes(xintercept = median_value), color = "green", linetype = "dashed", size = 1) +  
  geom_vline(aes(xintercept = mode_value), color = "red", linetype = "dashed", size = 1) +  
  
  annotate("text", x = mean_value, y = 0.43, label = "Mean", color = "blue", vjust = -1) +  
  annotate("text", x = median_value, y = 0.35, label = "Median", color = "black", vjust = -1) +  
  annotate("text", x = mode_value, y = 0.25, label = "Mode", color = "red", vjust = -1) +  
  labs(title = "Histogram of Minimum Close-Approach Distance",   
       x = "Minimum Distance (au)",   
       y = "Density") +  
  theme_minimal()


library(ggplot2)  
library(tidyr)  

set.seed(123)  
df <- data.frame(V.relative.km.s. = rnorm(1000, mean = 0, sd = 1))  


mean_value <- mean(df$V.relative.km.s.)  
median_value <- median(df$V.relative.km.s.) 
mode_value <- df$V.relative.km.s.[which.max(table(df$V.relative.km.s.))]

ggplot(df, aes(x = V.relative.km.s.)) +   
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "lightblue", color = "black", alpha = 0.6) +   
  geom_density(color = "orange", size = 1) +   
  
  geom_vline(aes(xintercept = mean_value), color = "blue", linetype = "dashed", size = 1) +  
  geom_vline(aes(xintercept = median_value), color = "green", linetype = "dashed", size = 1) +  
  geom_vline(aes(xintercept = mode_value), color = "red", linetype = "dashed", size = 1) +  
  
  annotate("text", x = mean_value, y = 0.43, label = "Mean", color = "blue", vjust = -1) +  
  annotate("text", x = median_value, y = 0.35, label = "Median", color = "black", vjust = -1) +  
  annotate("text", x = mode_value, y = 0.25, label = "Mode", color = "red", vjust = -1) +  
  labs(title = "V.relative.km.s.",   
       x = "V.relative.km.s.",   
       y = "Density") +  

  theme_minimal()






x<- c(0,0,0,0,1,0)
y <- c("(2023 JA1)", "(2023 HV5)", "(2023 JM2)", "(2023 HF7)", "(2023 HH7)", "(2023 JW4)")

barplot(x, names.arg = y,
        xlab ="Object",
        ylab ="Rarity",
        col ="skyblue",main ="Object vs Rarity")


x<- c(0,0,0,0,1,0)
y <- c("2023-May-03 01:15", "2023-May-03 03:16", "2023-May-03 05:23 ", "2023-May-04 00:02 ", "2023-May-04 05:47 ", "2023-May-04 08:00")
barplot(x, names.arg = y,
        xlab ="Close.Approach..CA..Date",
        ylab ="Rarity",
        col ="skyblue",main ="Close.Approach..CA..Date vs Rarity")



x<- c(0,0,0,0,1,0)
y <- c("13 m -   29 m", "9.8 m -   22 m ", "17 m -   38 m ", "12 m -   26 m ", "21 m -   47 m", "14 m -   32 m")
barplot(x, names.arg = y,
        xlab ="Diameter",
        ylab ="Rarity",
        col ="skyblue",main ="Diameter vs Rarity")



library(dplyr)
library(ggplot2)
set.seed(123)  
df <- data.frame(CA.DistanceNominal..au. = rnorm(1000, mean = 50, sd = 10))
df %>%
  ggplot(aes(y = CA.DistanceNominal..au.)) +
  geom_boxplot(fill = "lightblue",outlier.colour = "red") +
  ggtitle("Box Plot of CA.DistanceNominal..au") +
  ylab("Distance (au)")




library(dplyr)
library(ggplot2)
set.seed(123)  
df <- data.frame(V.relative.km.s. = rnorm(1000, mean = 50, sd = 10))
df %>%
  ggplot(aes(y = V.relative.km.s.)) +
  geom_boxplot(fill = "lightblue",outlier.colour = "red") +
  ggtitle("Box Plot of CV.relative.km.s.") +
  ylab("V.relative.km.s.")



library(dplyr)
library(ggplot2)
set.seed(123)  
df <- data.frame(H.mag.= rnorm(1000, mean = 50, sd = 10))
df %>%
  ggplot(aes(y = H.mag.)) +
  geom_boxplot(fill = "lightblue",outlier.colour = "red") +
  ggtitle("Box Plot of CV.relative.km.s.") +
  ylab("H.mag.")


library(dplyr)
library(ggplot2)
set.seed(123)  
df <- data.frame(V.infinity.km.s. = rnorm(1000, mean = 50, sd = 10))
df %>%
  ggplot(aes(y = V.infinity.km.s. )) +
  geom_boxplot(fill = "lightblue",outlier.colour = "red") +
  ggtitle("Box Plot of V.infinity.km.s. ") +
  ylab("V.infinity.km.s. ")


x<- c(0,0,0,0,1,0)
y <- c(0.02509,0.00290,0.03996,0.00426,0.00558,0.01796)
plot(x, y, main="Observation of CA.DistanceMinimum..au. vs Rarity", ylab="Rarity", xlab="CA.DistanceMinimum..au.",col="red", pch=19)


x<- c(0,0,0,0,1,0)
y <- c(4.99 ,8.82,6.32,13.05,24.53 ,13.30)
plot(x, y, main="Observation of V.infinity.km.s. vs Rarity", ylab="Rarity", xlab="CA.DistanceMinimum..au.",col="red", pch=19)



x<- c(0,0,0,0,1,0)
y <- c(5.01,8.93,6.33,13.10,24.55,13.31)
plot(x, y, main="Observation of V.relative.km.s.vs Rarity", ylab="Rarity", xlab="V.relative.km.s.",col="red", pch=19)




V.relative.km.s<- c(5.01,6.33,8.93,13.10,13.31,24.55)
Rarity<-c(0, 0, 0, 0, 1, 0)

df <- data.frame(x =V.relative.km.s , y = Rarity)

plot(df$x, df$y, type = "b", col = "green",pch = 19,
     xlab = "V.relative.km.s", ylab = "Rarity",
     main = "V.relative.km.s vs Rarity")

lines(df$x, df$y, col = "green")



V.infinity.km.s. <- c(0.00558,0.00426,0.00290,0.01796,0.02509,0.03996)
Rarity<-c(0, 0, 0, 0, 1, 0)

df <- data.frame(x =V.infinity.km.s., y = Rarity)

plot(df$x, df$y, type = "b", col = "green",pch = 19,
     xlab = "V.infinity.km.s.", ylab = "Rarity",
     main = "V.infinity.km.s. vs Rarity")

lines(df$x, df$y, col = "green")



CA.DistanceMinimum..au.<- c(0.00558,0.00426,0.00290,0.01796,0.02509,0.03996)
Rarity<- c(0,0,0,0,1,0)
df <- data.frame(x =CA.DistanceMinimum..au., y = Rarity)

plot(df$x, df$y, type = "b", col = "green",pch = 19,
     xlab = "CA.DistanceMinimum..au.", ylab = "Rarity",
     main = "CA.DistanceMinimum..au. vs Rarity")

lines(df$x, df$y, col = "green")



library(ggplot2)  
set.seed(123)  

df <- data.frame(  
  Object = rep(c("(2023 JA1)", "(2023 HV5)", "(2023 JM2)", "(2023 HF7)", "(2023 HH7)", "(2023 JW4)"), each = 200),  
  Rarity = c(rnorm(200, mean = 0), rnorm(200, mean = 1)) 
  
)  

ggplot(df, aes(x = Object, y = Rarity)) +  
  geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.7) +  
  stat_summary(fun = median, geom = "point", aes(color = "Median"),   
               size = 5, shape = 21, fill = "red") +
  stat_summary(fun.data = function(x) {
    return(data.frame(y = quantile(x, probs = c(0.25, 0.75)), 
                      ymin = quantile(x, 0.25), 
                      ymax = quantile(x, 0.75)))
  }, geom = "errorbar", width = 0.2, aes(color = "IQR")) +
  labs(title = "Violin Plot for Object", x = "Object", y = "Rarity") +  
  theme_minimal()




library(ggplot2)  
set.seed(123)  
 
df <- data.frame(  
  Diameter = rep(c("13 m -   29 m", "9.8 m -   22 m ", "17 m -   38 m ", "12 m -   26 m ", "21 m -   47 m", "14 m -   32 m"), each = 200),  
  Rarity = c(rnorm(200, mean = 0), rnorm(200, mean = 1)) 
  
)  

ggplot(df, aes(x = Diameter, y = Rarity)) +  
  geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.7) + 
  stat_summary(fun = median, geom = "point", aes(color = "Median"),   
               size = 5, shape = 21, fill = "red") + 
  stat_summary(fun.data = function(x) {
    return(data.frame(y = quantile(x, probs = c(0.25, 0.75)), 
                      ymin = quantile(x, 0.25), 
                      ymax = quantile(x, 0.75)))
  }, geom = "errorbar", width = 0.2, aes(color = "IQR")) +
  labs(title = "Violin Plot for diameter", x = "Diameter", y = "Rarity") +  
  theme_minimal()



library(ggplot2)  
set.seed(123)  

df <- data.frame(  
  Diameter = rep(c("13 m -   29 m", "9.8 m -   22 m ", "17 m -   38 m ", "12 m -   26 m ", "21 m -   47 m", "14 m -   32 m"), each = 200),  
  Rarity = c(rnorm(200, mean = 0), rnorm(200, mean = 1))  
)  

ggplot(df, aes(x = Diameter, y = Rarity)) +  
  geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.7) + 
  stat_summary(fun = median, geom = "point", aes(color = "Median"),   
               size = 5, shape = 21, fill = "red") + 
  stat_summary(fun.data = function(x) {
    return(data.frame(y = quantile(x, probs = c(0.25, 0.75)), 
                      ymin = quantile(x, 0.25), 
                      ymax = quantile(x, 0.75)))
  }, geom = "errorbar", width = 0.2, aes(color = "IQR")) +
  labs(title = "Violin Plot for Diameter", x = "Diameter", y = "Rarity") +  
  theme_minimal()



library(ggplot2)  
set.seed(123)  
df <- data.frame(  
  Close.Approach..CA..Date= rep(c("2023-May-03 01:15", "2023-May-03 03:16", "2023-May-03 05:23 ", "2023-May-04 00:02 ", "2023-May-04 05:47 ", "2023-May-04 08:00"), each = 200),  
  Rarity = c(rnorm(200, mean = 0), rnorm(200, mean = 1)) 
  
)  

 
ggplot(df, aes(x =Close.Approach..CA..Date, y = Rarity)) +  
  geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.7) +   
  stat_summary(fun = median, geom = "point", aes(color = "Median"),   
               size = 5, shape = 21, fill = "red") + 
  stat_summary(fun.data = function(x) {
    return(data.frame(y = quantile(x, probs = c(0.25, 0.75)), 
                      ymin = quantile(x, 0.25), 
                      ymax = quantile(x, 0.75)))
  }, geom = "errorbar", width = 0.2, aes(color = "IQR")) +
  labs(title = "Violin Plot for Close.Approach..CA..Date", x = "Close.Approach..CA..Date", y = "Rarity") +  
  theme_minimal() +
  scale_color_manual(name = "Statistics", values = c("Median" = "red", "IQR" = "blue"))

library(ggplot2)
set.seed(123)

df <- data.frame(  
  Diameter = rep(c("13 m - 29 m", "9.8 m - 22 m", "17 m - 38 m", "12 m - 26 m", "21 m - 47 m", "14 m - 32 m"), each = 200),  
  Rarity = c(rnorm(200, mean = 0), rnorm(200, mean = 1))  
)  

ggplot(df, aes(x = Diameter, y = Rarity)) +  
  geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.7) + 
  stat_summary(fun = median, geom = "point", aes(color = "Median"),   
               size = 5, shape = 21, fill = "red") + 
  stat_summary(fun.data = function(x) {
    return(data.frame(y = quantile(x, probs = c(0.25, 0.75)), 
                      ymin = quantile(x, 0.25), 
                      ymax = quantile(x, 0.75)))
  }, geom = "errorbar", width = 0.2, aes(color = "IQR")) +
  labs(title = "Violin Plot for Diameter", x = "Diameter", y = "Rarity") +  
  theme_minimal() + 
  scale_color_manual(name = "Statistics", values = c("Median" = "red", "IQR" = "blue"))

