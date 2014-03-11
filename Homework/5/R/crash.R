# Transforming parameters (which are not random) is fine.
# Transforming R.V.'s can be problematic.
# To interpret beta, consider a plot of Y ~ Xj, HOLDING ALL OTHER X's CONSTANT!

crash <- read.csv("../Data/crash.csv")
colnames(crash)
#pairs(crash)

mod.1 <- glm(Fatal ~ 1, data=crash, family=binomial("logit"))
mod.f <- glm(Fatal ~ ., data=crash, family=binomial("logit"))

#mod.hour <- glm(Fatal ~ Hour, data=crash, family=binomial("logit"))
#mod.A <- glm(Fatal ~ Age, data=crash, family=binomial("logit"))
mod.G  <- glm(Fatal ~ Sex, data=crash, family=binomial("logit"))           # ***
mod.S  <- glm(Fatal ~ Speed.Related, data=crash, family=binomial("logit")) # ***
mod.L  <- glm(Fatal ~ Speed.Limit, data=crash, family=binomial("logit"))   # ***
mod.D  <- glm(Fatal ~ Distracted, data=crash, family=binomial("logit"))    # ***
mod.Dg <- glm(Fatal ~ Drugs, data=crash, family=binomial("logit"))        # ***

#mod.s <- step(mod.1,scope=list(lower=mod.1,upper=mod.f),data=crash,direction="both")
