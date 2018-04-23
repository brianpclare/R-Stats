library(tidyverse)

mtcars
?mtcars

# let's look at the relationship between mpg and other variables. Start by plotting

ggplot(data = mtcars) + geom_point(mapping = aes(x = hp, y = mpg))

ggplot(data = mtcars) + geom_point(mapping = aes(x = disp, y = mpg))

# displacement and horsepower seem likely to be related to MPG

# lm is linear model (regresion). the format is lm ( Y ~ x)

regression <- lm(data = mtcars, mpg ~ disp)
regression
summary(regression)

# AIC and BIC are measures of model fit. Lower is better

AIC(regression)
BIC(regression)

# if this doesn't all show up for you, try options(verbose = TRUE)

# Now let's consider more than one independent variable

regression2 <- lm(data = mtcars, mpg ~ disp + hp)
regression2
summary(regression2)

AIC(regression2)
BIC(regression2)
# both of these are very minor improvements over the first regression


cor(mtcars$hp, mtcars$disp)

cor(mtcars$disp, mtcars$carb)

regression3 <- lm(data = mtcars, mpg ~ disp + carb)
regression3
summary(regression3)

# Interaction terms: what if carb has a different effect on mpg depending on the value of disp?

regression4 <- lm(data = mtcars, mpg ~ disp * carb)
regression4
summary(regression4)


regression5 <- lm(data = mtcars, mpg ~ .) # . means all variables
regression5
summary(regression5)

AIC(regression5)
BIC(regression5)


ggplot(data = mtcars) + geom_point(mapping = aes(x = carb, y = cyl))
# that looks weird. let's try jitter
ggplot(data = mtcars) + geom_jitter(mapping = aes(x = carb, y = cyl))

# don't forget you can customize the jitter amount
ggplot(data = mtcars) + geom_jitter(mapping = aes(x = carb, y = cyl), width = 0.2, height = 0.2)

# Seems like there are a few different types of cars that have different configurations

# K-Means Clustering

clusters <- kmeans(mtcars %>% select(cyl, carb), 3)

mtcars$cluster <- as.factor(clusters$cluster)
mtcars$name <- rownames(mtcars)

# tibbles / dplyr don't like rownames. if you have rownames and want to keep them, make them a real column

# Let's visualize

ggplot(data = mtcars) + geom_jitter(aes(x = carb, y = cyl, color = cluster),
                                    width = 0.2, height = 0.2)

View(mtcars %>% arrange(cluster))

# If you know something about cars, maybe you can label these clusters as types of cars,
# or different countries of origin, or price, or whatever

# As often happens in statistics, this domain knowledge matters a lot for interpreting results

# If you have a project that you know nothing about, you should either consult with an expert
# or get ready to learn about that topic yourself


# Random forest for classification

rm(mtcars) # getting rid of that cluster and rowname stuff

library(randomForest)
set.seed(12)

?randomForest

mtcars$cyl <- as.factor(mtcars$cyl)

tree_model <- randomForest(cyl ~ . , data = mtcars)

varImpPlot(tree_model)

tree_pred <- predict(tree_model, mtcars)

mtcars_pred <- as.tibble(cbind(rownames(mtcars), mtcars$cyl, tree_pred))

# So what does this mean?
