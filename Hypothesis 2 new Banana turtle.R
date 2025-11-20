# Turtle thesis hypothesis 2

setwd("C:/Users/emmad/Desktop/Sea turtle thesis/Analysis")
getwd()

library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

Turtles <- read.csv("Turtle_densityV3.csv")

Turtles_long <- Turtles %>%
  pivot_longer(
    cols = starts_with("Density"),
    names_to = "Replicate",
    values_to = "Turtle.density"
  )

# Summary dataframe of the data

Turtle_summary <- Turtles_long %>%
  group_by(Bay) %>%
  summarise(
    MEAN_density  = mean(Turtle.density),
    SEM_density = sd(Turtle.density) / sqrt(length(Turtle.density))
  )


# Making a plot of the data

ggplot(Turtle_summary, aes(x=Bay, y=MEAN_density)) +
  geom_bar(stat="identity", fill = "turquoise", color = "black") +
  geom_errorbar(aes(ymin=MEAN_density-SEM_density, ymax=MEAN_density+SEM_density), width=.2) + labs (title = "Mean Density of Turtles in Four Bays on Water Island", x = "Bay", y = "Mean Density (turtles per ha.)") + theme_minimal() 


# Run an ANOVA

anova_result <- aov(Turtle.density ~ Bay, data = Turtles_long) 
summary(anova_result) 

#Null hypothesis: There is no difference among group means.
#Alternative hypothesis: At least one group mean differs from the others. 

#Shapiro-Wilk test

TurtleResiduals <- rstandard(anova_result)

shapiro.test(TurtleResiduals)

#Null hypothesis: The data follow a normal distribution.

hist(TurtleResiduals) 

# Bartlett test

bartlett.test(Turtle.density ~ Bay, data = Turtles_long) 

# One-way ANOVA assumptions were not met, so I will do the non-parametric test for a one-way ANOVA, Kruskal-Wallis test

# Performing Kruskal-Wallis Test
kruskal_test_result <- kruskal.test(Turtle.density ~ Bay, data = Turtles_long)
print(kruskal_test_result)


