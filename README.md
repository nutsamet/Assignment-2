# Assignment-2

The following code, creates a toy dataset with a treatment variable, D, an outcome variable, Y, and other variables V1 to V4.
n = 1000
## Generating a random data set here
#Syntax for the normal distribution here is rnorm(sample size, mean, SD)
V1 = rnorm(n, 45, 10)
#getting a binary variable
V2 = sample(c(1,0), 
             replace = TRUE, 
             size = n,
             prob = c(.4,.6))
V3 = rnorm(n, V1/10, 1)
V4 = rnorm(n, 0, 1)
D  = as.numeric(pnorm(rnorm(n, .01*V1 + .8*V2 + 0.3*V3 + V4, 1), .45 + .32 + .3*4.5, 1) > .5)
Y  = rnorm(n, .8*D - 0.45*V2 - .4*V3 + 2, .2)
# combining everything in a data frame
df = data.frame(V1, V2, V3, V4, D, Y)
STEP 1
From the variables V1, V2, V3, and V4, which one(s) are not confounding variable(s) (covariates that cause confounding)? Remember, a rule of thumb (although not a perfect rule) is that the variable is correlated with both the treatment variable and the outcome variable. Explain!

V3 - negative correlation with outcome Y variable (-0.7394)
V4 - correlation with treatment D variable (0.5033)
cor(df) 
- code line
STEP 2
Can you figure out the true treatment effect by looking at the data generating process above?



The true treatment effect is 0.8 (from the line with variable Y, the D variable is multiplied by 0.8 - it’s a treatment effect) 



STEP 3
Plot the outcome variable against the treatment variable. Make sure you label your axes. Do you see a trend?

plot(D,Y, xlab="Treatment", ylab="Outcome Variable")


STEP 4
Are the variables V1, V2, V3, and V4 balanced across the treatment and control groups? You can use any R function from any package to check this (for instance, you can check the cobalt package). Make sure you check all variables.
Note: This is optional but you can use the gridExtra package and its grid.arrange() function to put all the 4 graphs in one 2 x 2 graph. Read more about the package and how to use it here: https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html. Set nrow = 2.
df$D <- as.factor(df$D)
df.means.0 <- c(0, mean(V1[which(D == 0)]), mean(V2[which(D == 0)]), mean(V3[which(D == 0)]), mean(V4[which(D == 0)]))
df.means.1 <- c(1, mean(V1[which(D == 1)]), mean(V2[which(D == 1)]), mean(V3[which(D == 1)]), mean(V4[which(D == 1)]))
df.means <- data.frame(rbind(df.means.0, df.means.1))
colnames(df.means) <- c("D", "V1", "V2", "V3", "V4")
df.means$D <- as.factor(df.means$D)
 
 
V1.plot <- ggplot(df, aes(x = V1, fill = D)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") +
  geom_vline(data = df.means, aes(xintercept = V1, color = D), linetype = "dashed", size = 1)
 
V2.plot <- bal.plot(df, 
            treat = df$D, 
            var.name = "V2",
            which = "unadjusted", 
            type = "histogram", mirror = TRUE)
 
V3.plot <- ggplot(df, aes(x = V3, fill = D)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") +
  geom_vline(data = df.means, aes(xintercept = V3, color = D), linetype = "dashed", size = 1)
 
V4.plot <- ggplot(df, aes(x = V4, fill = D)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") +
  geom_vline(data = df.means, aes(xintercept = V4, color = D), linetype = "dashed", size = 1)
 
grid.arrange(V1.plot, V2.plot, V3.plot, V4.plot, nrow = 2)
print(df.means)
STEP 5
Write code that would simply calculate the Prima Facie treatment effect in the data above. What’s the Prima Facie treatment effect? Note that the outcome variable is binary.

*code for the Prima Facie will be here soon*
df.treat <- subset(df$Y, df$D == 1)
df.control <- subset(df$Y, df$D == 0)
primafacie <- mean(df.treat) - mean(df.control)
print(primafacie)
STEP 6
Explain why the Prima Facie effect is not the true average causal effect from Step 2.

STEP 7
We can use matching to create a better balance of the covariates. Use a propensity score model that includes all the variables V1, V2, V3, and V4.
df2 = data.frame(V1, V2, V3, V4, D, Y)
df2$D <- as.integer(df2$D)
df.prop <- glm(D ~ V1 + V2 + V3 + V4, data = df2, family = "binomial")
X <- df.prop$fitted.values
matchout.prop <- Match(Y = df2$Y, Tr=df2$D, X=X)
 
STEP 8
Check the balance of the covariates. Do you see any improvements after matching?
mb.out <- MatchBalance(D ~ V1 + V2 + V3 + V4, data=df2, match.out = matchout.prop, nboots=500)
STEP 9
What is the treatment effect after matching? Is this surprising given your answer to Step 2. Is the treatment effect found in this Step closer to the treatment effect in Step 2 than the treatment effect before matching?
matchout.prop$est
The treatment effect after matching is 0.81, which is much closer to the true treatment effect. 


QUESTION 2: Daughters
Read Section 5 (which is only about 1 page long!) of Iacus, King, Porro (2011), Multivariate Matching Methods That Are Monotonic Imbalance Bounding, JASA, V 106, N. 493, available here: https://gking.harvard.edu/files/gking/files/cem_jasa.pdf. Don’t worry about the “CEM” elements. Focus on the “daughters” case study.
Data for this case study is available in “daughters” below.
daughters = read.csv(url("http://bit.ly/daughters_data")) %>% 
  clean_names()
STEP 1
Before doing any matching, run a regression, with the outcome variable being nowtot, the treatment variable being hasgirls, and the independent vars mentioned below: - dems, - repubs, - christian, - age, - srvlng, - demvote
Show the regression specification. Use the regression to estimate a treatment effect and confidence interval. Check the balance of this not-matched data set using any method of your choice (balance tables, balance plots, love plots, etc).
daughters <- read.csv(url("http://bit.ly/daughters_data"))  
 
 
reg <- lm(nowtot ~ hasgirls + Dems + Repubs + Christian + age + srvlng + demvote, data=daughters)
reg  
summary(reg)
Estimate<-summary(reg)$coefficients[2][1]
Std.Error<- summary(reg)$coefficients[, 2][2]
lwrb <- Estimate - Std.Error
uprb <- Estimate + Std.Error
sprintf("For the treated (those who have girls), the treatment effect is %f, with 95%% CI [%f, %f]", Estimate, lwrb, uprb)
 
 
 
library(cobalt)
bal.plot(x=reg, data=daughters, var.name = "Dems", which="Unadjusted", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=reg, data=daughters, var.name = "Repubs", which="Unadjusted", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=reg, data=daughters, var.name = "Christian", which="Unadjusted", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=reg, data=daughters, var.name = "age", which="Unadjusted", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=reg, data=daughters, var.name = "srvlng", which="Unadjusted", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=reg, data=daughters, var.name = "demvote", which="Unadjusted", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
 
----------------------------------------------------- Patrycja 
summary(reg)
#we use summary() to obtain regression specification 
summary(reg)
#standard error is 1.9 and estimate is -0.45 
sd_error <- 1.9036
estimate <- -0.4523
lwrb <- estimate - sd_error
uprb <- estimate + sd_error
lwrb
uprb
install.packages("cobalt")
library(cobalt)
 
love.plot(x=reg, formula = (nowtot ~ hasgirls + Dems + Repubs + Christian + age + srvlng + demvote), data = daughters, method = "weighting", estimand = "ATT", stars ="std")
 
STEP 2

Note: When you use the GenMatch() function, wrap everything inside the following function invisible(capture.output()). This will reduce the unnecessary output produced from the GenMatch() function. For instance, you can say: invisible(capture.output(genout_daughters <- GenMatch(...)))
install.packages("Matching")
library(rgenoud)
library(Matching)
Y <- daughters$nowtot
Tr <- daughters$hasgirls
X <- cbind(daughters$dems,daughters$depubs, daughters$christian, daughters$age, daughters$srvlng, daughters$demvote)
invisible(capture.output(genout2 <- GenMatch(Tr=Tr, X=X, estimand = "ATT", M=1, pop.size = 50, max.generations = 50, wait.generations = 25, unif.seed = 100, int.seed = 100)))
m_out <- Match(Y = Y, Tr=Tr, X=X, M=1, Weight.matrix = genout2)
m_out$est
----------------------------------------------------- Patrycja 
### Step 2 
library(Matching)
library(rgenoud)
 
Y <- daughters$nowtot
Tr <- daughters$hasgirls
X <- cbind(daughters$Dems,daughters$Repubs, daughters$Christian, daughters$age, daughters$srvlng, daughters$demvote)
 
invisible(capture.output(genout_daughters <- GenMatch(Tr=Tr, X=X, estimand = "ATT", pop.size = 15, max.generations = 10, wait.generations = 10, unif.seed = 26, int.seed = 26)))
 
mout <- Match(Y = Y, Tr=Tr, X=X, Weight.matrix = genout_daughters)
mb <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = daughters, match.out=mout, nboots=500)
mb
 
mout <- Match(Y = Y, Tr=Tr, X=X, replace=TRUE, caliper=c(1000), Weight.matrix = genout_daughters)
mout$est
 
 
STEP 3
Summarize (in 5-15 sentences) the genetic matching procedure and results, including what you matched on, what you balanced on, and what your balance results were. Provide output for MatchBalance() in the body of your submission.


p value before: $p.value [1] 0.3557053
p value after: $p.value [1] 0.6810706
 - improvement
matched on all covariates
 
 
library(cobalt)
bal.plot(x=m.out2, data=daughters, var.name = "Dems", which="both", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=m.out2, data=daughters, var.name = "Repubs", which="both", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=m.out2, data=daughters, var.name = "Christian", which="both", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=m.out2, data=daughters, var.name = "age", which="both", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=m.out2, data=daughters, var.name = "srvlng", which="both", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote))
bal.plot(x=m.out2, data=daughters, var.name = "demvote", which="both", formula = (hasgirls ~ Dems  + Repubs + Christian+ age + srvlng + demvote)
STEP 4
Is your treatment effect different from the one reported before matching? By how much? If your numbers are different, provide some explanation as to why the two numbers are different. If they’re not, provide an explanation why they’re not different.
The post-matching treatment effect (0.83) is different from the pre-matching treatment effect (-0.45). This is due to the fact that we reduced the bias Genetic Matching. It allowed us to get a better balance between the covariates of the treatment and the control groups and made the causal inference more precise. Following the discussion in the paper, after matching, we confirm that the observed politicians are 0.83 times more supportive of women on every additional girl child.

STEP 5

#version 1
invisible(capture.output(genout_1 <- GenMatch(Tr=Tr, X=X, estimand = "ATT", M=2, pop.size = 60, max.generations = 15, wait.generations = 5, unif.seed = 333, int.seed = 3333)))
m.out_1<- Match(Y = Y, Tr=Tr, X=X, M=2, Weight.matrix = genout_1)
Estimate_1<-m.out_1$est
Std.Error_1<- m.out_1$est
lwrb <- Estimate_1 - 1.96*Std.Error_1
uprb <- Estimate_1 + 1.96*Std.Error_1
         
sprintf("The new (version 1) treatment effect is %f, with 95%% CI [%f, %f]", Estimate_1, lwrb, uprb)
 
 
#version 2:
invisible(capture.output(genout_2 <- GenMatch(Tr=Tr, X=X, estimand = "ATT", M=3, pop.size = 30, max.generations = 20, wait.generations = 10, unif.seed = 333, int.seed = 3333, caliper = c(1e16, 1e16, 0.5, 1e16))))
m.out_2<- Match(Y = Y, Tr=Tr, X=X, M=3, Weight.matrix = genout_2)
m.out_2$est
 
#version 3:
invisible(capture.output(genout_3 <- GenMatch(Tr=Tr, X=X, estimand = "ATT", M=4, pop.size = 60, max.generations = 40, wait.generations = 20, unif.seed = 333, int.seed = 3333, caliper = c(0.5, 1e16, 0.5, 1e16, 0.5, 1e16))))
m.out_3<- Match(Y = Y, Tr=Tr, X=X, M=4, Weight.matrix = genout_3)
m.out_3$est
Although the three different models show variability in outcomes, they are pretty similar and do not deviate much from zero, which means that, given our CI’s from step 1, the null hypothesis (no significant difference) should most likely be accepted.
STEP 6


new_daughters <- daughters %>% filter((nboys >= 2 & ngirls == 0) | (ngirls >= 2 & nboys == 0))
head(new_daughters)
 
lm1 <- lm(nowtot ~ hasgirls + Dems + Repubs + Christian + age + srvlng + demvote, data= new_daughters)
summary(lm1)
 
#Std. Error for hasgirls is 3.5008 and the Estimate is 12.2925
 
sd <- 3.5008
est <- 12.2925
lwrb <- est - sd
uprb <- est + sd
lwrb
uprb
#treatment effect is 12.292542, the 95% CI is lower bound 8.791772, upper bound 15.7933
 
Y <- new_daughters$nowtot
Tr <- new_daughters$hasgirls
X <- cbind(new_daughters$Dems,new_daughters$Repubs, new_daughters$Christian, new_daughters$age, new_daughters$srvlng, new_daughters$demvote)
 
invisible(capture.output(genout_new <- GenMatch(Tr=Tr, X=X, estimand = "ATT", pop.size = 15, max.generations = 10, wait.generations = 10, unif.seed = 26, int.seed = 26)))
 
mout <- Match(Y = Y, Tr=Tr, X=X, Weight.matrix = genout_new )
mb <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = new_daughters, match.out=mout, nboots=500)
 
mout <- Match(Y = Y, Tr=Tr, X=X, replace=TRUE, caliper=c(1000), Weight.matrix = genout_new)
mout$est
 
STEP 7
It is NOT wise to match or balance on “totchi”. What is the reason? Hint: You will have to look at what variables mean in the data set to be able to answer this question.







QUESTION 3: COPD
Most causal studies on the health effects of smoking are observational studies (well, for very obvious reasons). In this exercise, we are specifically after answer the following question: Does smoking increase the risk of chronic obstructive pulmonary disease (COPD)? To learn more about the disease, read here: https://www.cdc.gov/copd/index.html
We’ll use a sub-sample of the 2015 BRFSS survey (pronounced bur-fiss), which stands for Behavioral Risk Factor Surveillance System. The data is collected through a phone survey across American citizens regarding their health-related risk behaviors and chronic health conditions. Although, the entire survey has over 400,000 records and over 300 variables, we only sample 5,000 observations and 7 variables.
Let’s load the data first and take a look at the first few rows:
brfss = read.csv("http://bit.ly/BRFSS_data") %>% 
  clean_names()
head(brfss)
##   copd smoke     race   age    sex wtlbs avedrnk2
## 1   No     0    White 18-24 Female   180        4
## 2   No     0    White 55-64 Female   170        1
## 3   No     0    White   65+   Male   170        1
## 4   No     0 Hispanic 18-24   Male   185        1
## 5   No     0    White   65+ Female   150        1
## 6   No     0    White   65+   Male   180        2
A summary of the variables is as follows:
copd: Ever told you have chronic obstructive pulmonary disease (COPD)?
smoke: Adults who are current smokers (0 = no, 1 = yes)
race: Race group
age: age group
sex: gender
wtlbs: weight in pounds (lbs)
avedrnk2: During the past 30 days, when you drank, how many drinks did you drink on average?
STEP 1
Check the balance of covariates before matching using any method of your choice. You can look at balance tables, balance plots, or love plots from any package of your choice. Do you see a balance across the covariates?
Note: This is optional but you can use the gridExtra package and its grid.arrange() function to put all the 4 graphs in one 2 x 2 graph. Read more about the package and how to use it here: https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html. Set nrow = 2.
library(ggplot2)
library(gridExtra)
plot_race <- ggplot(brfss, aes(x=race, fill=smoke)) + geom_histogram(stat="count", position="dodge", binwidth=0.5, alpha=.7) +
  labs(x="Race", fill = "Adults who are 
current smokers") +
  theme(axis.text.x=element_text(size=9, angle=45, vjust=0.5))
 
plot_age <- ggplot(brfss, aes(x=age, fill=smoke)) + geom_histogram(stat="count", position="dodge", binwidth=0.5, alpha=.7) +
  labs(x="Age", fill = "Adults who are 
current smokers") +
  theme(axis.text.x=element_text(size=9, angle=45, vjust=0.5))
 
plot_sex <- ggplot(brfss, aes(x=sex, fill=smoke)) + geom_histogram(stat="count", position="dodge", binwidth=0.5, alpha=.7) +
  labs(x="Sex", fill = "Adults who are 
current smokers") +
  theme(axis.text.x=element_text(size=9, angle=45, vjust=0.5))
 
plot_weight <- ggplot(brfss, aes(x=wtlbs, fill=smoke)) +
  geom_histogram(binwidth=2, alpha=.7, position="identity") +
  labs(x="Weight (lbs)", fill = "Adults who are 
current smokers") +
  theme(axis.text.x=element_text(size=9, angle=45, vjust=0.5))
 
plot_av_drink <- ggplot(brfss, aes(x=avedrnk2, fill=smoke)) +
  geom_histogram(stat="count", binwidth=.5, alpha=.5, position="dodge") +
  labs(x="During the past 30 days, when you drank, how many drinks did you drink on average?", fill = "Adults who are 
current smokers")
 
grid.arrange(plot_race, plot_age, plot_sex, plot_weight, plot_av_drink, nrow=3)
STEP 2
Now, let’s do Mahalanobis distance matching. Note that you can use the same old Match() function. Use all covariates in the data set to match on, however, make sure you convert all categorical variables into factor variables (Google to see how). We are going to specify estimand = "ATT" in the Match() function. What’s the treatment effect after matching?
factor_cols <- c("copd", "race", "age", "sex")
brfss[factor_cols] <- lapply(brfss[factor_cols], factor)
 
brfss$smoke <- as.logical(brfss$smoke)
sapply(brfss, function(x) sum(is.na(x)))
 
Y1 = brfss$copd
Tr = brfss$smoke
X <- cbind(brfss$race, brfss$age, brfss$sex, brfss$wtlbs, brfss$avedrnk2)
 
maha_att <- Match(Y = Y1, Tr = Tr, X = X, Weight = 2, estimand = "ATT")
 
summary(maha_att)
 
print(paste("The treatment effect after matching is:" , round(maha_att$est,4))) #estimate is the difference between treatment and control group, p.value is small, so it's statistically significant, ATE is like a T-test
 
length(maha_att$index.dropped)
bal.tab(maha_att, formula = (smoke ~ race + age + sex + wtlbs + avedrnk2), data = brfss)


maha_ate <- Match(Y=Y1, Tr=Tr, X = X, Weight = 2, estimand = "ATE")
 
summary(maha_ate)
diff_ate_att <- round(maha_ate$est - maha_att$est,4)
 
print(paste("The difference in the average causal effect (ATE - ATT) is:", diff_ate_att))
