library(ggplot2)
#install.packages('psych')
library(psych)
#install.packages('polycor')
library(polycor)
#install.packages('rcompanion')
library(rcompanion)
library(reshape2)
# tetrachoric corr -> calculate correlation between binary categorical variables
# polychoric corr -> calculate correlation between ordinal categorical variables
# cramer v -> calculate correlation between nominal categorical variables

data <- matrix(c(19,12,30,39),nrow=2)
tetrachoric(data)

#define movie ratings
x <- c(1, 1, 2, 2, 3, 2, 2, 3, 2, 3, 3, 2, 1, 2, 2, 1, 1, 1, 2, 2)
y <- c(1, 1, 2, 1, 3, 3, 3, 2, 2, 3, 3, 3, 2, 2, 2, 1, 2, 1, 3, 3)

#calculate polychoric correlation between ratings
polychor(x, y)

#create table
data = matrix(c(6, 9, 8, 5, 12, 10), nrow=2)

#view table
data

#calculate Cramer's V
cramerV(data)

# heatmap
df <- data.frame(points=c(22, 25, 30, 16, 14, 18, 29, 22),
                 assists=c(4, 4, 5, 7, 8, 6, 7, 12),
                 rebounds=c(10, 7, 7, 6, 8, 5, 4, 3),
                 blocks=c(12, 4, 4, 6, 5, 3, 8, 5))

#view data frame
df

cor_df <- round(cor(df),2)
melted_cor <- melt(cor_df)
head(melted_cor)

ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())


#bar chart
data("mtcars")
attach(mtcars)
g <- ggplot(mpg, aes(class))+
  geom_bar()
g
g <- ggplot(mpg)+
  geom_bar(aes(y=class));g
