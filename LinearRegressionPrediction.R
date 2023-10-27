
# Linear Regression

#########################################
# Step 1
#########################################

#Part A
summary(score)
#Part B
boxplot(score, col="blue")
#Part C
hist(score, col="blue")

#Part D
summary(sales_global)
#Part E
boxplot(sales_global, col="green")
#Part F
hist(sales_global, col="green")

#Part G
summary(release_year)
#Part H
boxplot(release_year, col="orange")
#Part I
hist(release_year, col="orange")

#Part J
summary(count_critic)
#Part K
boxplot(count_critic, col="purple")
#Part L
hist(count_critic, col="purple")

#Part M
color_mapping = rainbow(length(unique(genre)))[as.integer(as.factor(genre))]
plot(sales_global,score, col=color_mapping)
plot(release_year,score, col=color_mapping)
plot(count_critic,score, col=color_mapping)
par(mfrow=c(1,3))

#########################################
# Step 2 and 3
#########################################

sales_global.fit = lm(score~sales_global)
sales_global.fit

b0=coef(sales_global.fit)[1]
b1=coef(sales_global.fit)[2]
summary(sales_global.fit)

confint(sales_global.fit,'sales_global',level=0.95)
require(visreg)
visreg(sales_global.fit,alpha=0.05)

b1*0.750000+b0


model=lm(score~release_year)

b0=coef(model)[1]
b1=coef(model)[2]
r_squared=summary(model)$r.squared
b0
b1
r_squared
summary(lm.fit)

model_summary=summary(model)
confint(lm.fit, 'release_year', level=0.95)

# 95% Confidence Interval for b1 (release_year)
conf_interval=confint(model, level = 0.95)[2, ]

#t-test statistic for b1
t_statistic=model_summary$coefficients["release_year", "t value"]


# p-value for b1
p_value=summary(model)$coefficients
p_value

install.packages("visreg")
require(visreg)
visreg(lm.fit, alpha=0.05)

coefficients=coef(model)

# Calculate the predicted score
predicted_score = coefficients[1] + coefficients[2] * 2009
predicted_score


model=lm(score~count_critic)

b0=coef(model)[1]
b1=coef(model)[2]
r_squared=summary(model)$r.squared
b0
b1
r_squared
summary(lm.fit)

model_summary=summary(model)
confint(lm.fit, 'count_critic', level=0.95)

# 95% Confidence Interval for b1 (count_critic)
conf_interval=confint(model, level = 0.95)[2, ]

#t-test statistic for b1
t_statistic=model_summary$coefficients["count_critic", "t value"]


# p-value for b1
p_value=summary(model)$coefficients
p_value

install.packages("visreg")
require(visreg)
visreg(lm.fit, alpha=0.05)

coefficients=coef(model)

# Calculate the predicted score
predicted_score = coefficients[1] + coefficients[2] * 80
predicted_score


#########################################
# Step 4
#########################################

# score = bo+ b1(sales_global) + b2(release_year) + b3(count_critic)
attach(video_games_fall_2023)
View(video_games_fall_2023)

mreg=lm(score~sales_global+release_year+count_critic)
summary(mreg)

b0=coef(mreg)[1]  
b1=coef(mreg)[2] 
b2=coef(mreg)[3]
b3=coef(mreg)[4]
confint(mreg, 'score', level=0.99)

# Prediction for: 
#(i) it has 750,000 in global sales, 
#(ii) was released in 2009,
# (iii) was reviewed by 80 critics :

predicted_score=b0+b1 * (0.750)+ b2 * (2009) + b3 * (80)
predicted_score

#########################################
# Step 5
#########################################

# score = bo+ b1(release_year) + b2(Nintendo)

attach(video_games_fall_2023)
View(video_games_fall_2023)

video_games_fall_2023$Nintendo=ifelse(video_games_fall_2023$publisher == "Nintendo", 1, 0)

mreg5=lm(score~release_year+Nintendo,data=video_games_fall_2023)
summary(mreg5)

b0=coef(mreg5)[1]
b1=coef(mreg5)[2]
b2=coef(mreg5)[3]

install.packages("ggplot2")
library(ggplot2)

#squares for scatter plot dots
#regression lines dashed and width of 2
#Nintendo in green, non-nintendo in blue
#legend:
#     title: Publisher
#      position: top
ggplot(video_games_fall_2023, aes(x=release_year, y=score, color=factor(Nintendo))) + 
  geom_point(shape=15) + # Use squares for scatter plot dots
  geom_smooth(method=lm, se=FALSE, linetype="dashed", linewidth=2) +
  scale_color_manual(values=c("blue", "green"), labels=c("Non-Nintendo", "Nintendo")) +
  labs(color="Publisher") +
  theme(legend.position="top")

#########################################
# Step 6
#########################################
### Categorical Variables with Multiple Categorie ###
#A,B,C
video_game$genre = as.factor(video_game$genre)
genre=relevel(genre, ref="Racing")
attach(video_game)
table(genre)
mreg=lm(score~genre)
summary(mreg)

#D
genre=relevel(genre, ref="Racing")
attach(video_game)
table(genre)
mreg=lm(score~genre)
summary(mreg)

#E
genre=relevel(genre, ref="Racing")
attach(video_game)
table(genre)
mreg=lm(score~genre)
summary(mreg)

#########################################
# Step 7
#########################################

#### Interaction terms###
#A
video_game$nintendo <- ifelse(video_game$publisher == 'Nintendo',1,0)
attach(video_game)
video_game$strategy <- ifelse(video_game$genre == 'Strategy',1,0)
attach(video_game)
mreg2=lm(score~nintendo+strategy+nintendo*strategy)
summary(mreg2)

#B&C
mreg3=lm(score~release_year+nintendo+nintendo*release_year)
summary(mreg3)
b0=coef(mreg3)[1]
b1=coef(mreg3)[2]
b2=coef(mreg3)[3]
b3=coef(mreg3)[4]
plot(release_year, score, col=ifelse(nintendo=="1", "green", "blue"),pch = 2)

abline(b0+b2,b1+b3, col= "green",lwd = 2, lty = "dashed")
abline(b0, b1, col= "blue", lwd = 2, lty = "dashed")
legend("topleft", pch=2, col=c( "green", "blue" ), c( "nintendo", "non-nintendo"),lty=c(2,2))
