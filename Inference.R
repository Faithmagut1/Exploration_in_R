B <- c(-5,-4,-3,-2,-1,0)# preference for B over A
A <- c(0,1,2,3,4,5)# Preference for A over B
Patient.rank <- as.matrix(expand.grid(B,A)) # possible choice combination for a patient
ind <- Patient.rank[,1]==-Patient.rank[,2] # no patients put their crosses at identical distances from the point of no  preference
Patient.rank <- Patient.rank[!ind,] # to exclude combination with identical distance from 0 (e.g. -1 and 1)
Patient.choice <- rowSums(Patient.rank) # Preference of a patient (if negative then the patient prefers B over A other he prefers A over B)

All.patien.choice <- as.matrix(expand.grid(Patient.choice,Patient.choice,
                                           Patient.choice,Patient.choice,Patient.choice)) # all combination possible of the five patients

choice <- rowSums(All.patien.choice)# sum of preference rank test statistics
Choice.sample <- unique(choice)# which is symetric vector from -25 to 25

choiceB <- c(choice)
prob <- c(table(choiceB)/length(choice))
prob

plot(unique(choiceB),prob)
M <- mean(-25:25)
M
var(-25:25)
var(-15:15)


rank <- c(3.1, 5.2, −0.7, 4.8, 5.0)
sum(rank)
S <- sum(rank)
pnorm(S/sqrt(221)),lower.tail = F)


success <- 0:5

plot(success, dbinom(success, size=5, prob=.3),type='h')
dbinom(0:5, 5, 0.5)

var(rank)
2*pnorm(9/sqrt(82.67),lower.tail = F)
