## Load Packages and Set Seed install.packages("MASS")
library(MASS)
set.seed(1)
data <- read.csv(file.choose())
fit <- lda(segment ~ Health + Finc + Sales + Advt + Edu + Cons + Eng + Tech + Retail + SMB + FB_Insta + Twit + Snap + YouTube + Pod_radio + TV + NewsP + Age + Gender + Income + Education, data = data)
fit 

## print the summary statistics of your discriminant analysis
## Check which Discriminant Functions are Significant ldaPred <- predict(fit, data)
ld <- ldaPred$x

anova(lm(ld[,1] ~ data$segment)) 
anova(lm(ld[,2] ~ data$segment))