# Non parametric tests

kruskal.test() # Equivalent to ANOVA on ranked data
wilcox.test() # Equivalent to paired t-test

# N way ANOVA

model.1 <- lm(dep ~ ind1 + ind2)
aov(model.1)

model.1 <- lm(dep ~ ind1:ind2)
aov(model.1)

model.1 <- lm(dep ~ ind1*ind2)
aov(model.1)
