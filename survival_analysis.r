# CORE IDEA: In medicine, survival analysis asks "How long until a patient dies?".
            #In finance, we ask "How long until a borrower defaults?".
            # Same math but different event
#TODO: Address assumptions (weibull dist, binom dist)
# ------------------- STEP 1 -------------------
set.seed(42)
n <- 300

credit_score <- sample(c("High", "Low"), n, replace = TRUE, prob = c(0.55,0.45)) 
time_high <- rweibull(n, shape = 1.5, scale = 36) # generates n random times from a weibull distribution for "High" credit score group
time_low <- rweibull(n, shape = 1.2, scale = 18) # same for "Low" credit score group

time <- ifelse(credit_score=="High", time_high, time_low)
time <- round(time) #converts the ampled times to whole months by rounding
time <- pmax(time,1) #replaces any value below 1 with 1 so the minimum time is 1 month
time <- pmin(time, 60) # replaces any time above 60 with 60 so the maximum time is 60 months

# rweibull is essentially generating random "life times"
# "High" credit score borrowers get drawn from a distribution spanning 36 months (longer than "low" since they're less likely to default earleir)

censored <- rbinom(n, 1, prob = 0.35)
defaulted <- ifelse(censored == 1, 0, 1)
# A censored borrower (censored == 1) gets default=0 since we never saw them default
# they just left the study


#Adding more covariates
loan_amount <- sample(c("Large", "Small"), n, replace = TRUE)
employed <- rbinom(n, 1, prob = 0.75)


#putting df together
loan <- data.frame(
    borrower_id = 1:n,
    time = time,
    defaulted = defaulted, 
    credit_score = credit_score,
    loan_amount  = loan_amount,
    employed     = employed
)

#just to check the data
head(loan)
table(loan$credit_score, loan$defaulted)

# ------------------- STEP 2 -------------------
library(survival)

surv_object <- Surv(time = loan$time, event = loan$defaulted)
surv_object

# ------------------- STEP 3 -------------------
# fit Kaplan_Meier Curve
km_fit <- survfit(Surv(time = loan$time, event = loan$defaulted) ~ credit_score, data = loan)
# The ~credit_score part is used to fit a seperate survival curve for each credit score group

# Now we want to know for each moment of time (12, 24,... months)
# what fraction of people have still not defaulted
summary(km_fit, times = c(12, 24, 36, 48, 60))

# ------------------- STEP 4 -------------------
#plotting
plot(km_fit,
    col = c("blue", "red"),
    lwd = 2,
    xlab = "Months",
    ylab = "Probability of No Defaults",
    main = "Kaplan - Meier: Loan Default Survival Curves")

    legend("topright",
        legend = c("High Credit Score", "Low Credit Score"),
        col = c("blue","red"),
        lwd = 2
    )
