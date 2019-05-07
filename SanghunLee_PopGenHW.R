################# Population Genetics ##############

# Your submission for this homework assignment should give
# the outputs requested by numbers 7 and 8 in the handout.
# Number 9 is a discussion question.
# Answer it in your e-mail when you submit the assignment.

# You may also include any of your "future explorations" code.

# Please don't include ALL of your test code, side projects, etc.
# in this file.
# It will be easier for me to see that it works if it only outputs
# what I am looking for.


##Number one and two
ind <- numeric(1000) # population of 1000 individuals
ind[1000] <- 1 # The 1000th individual is the mutant
selco <- numeric(1000) #selection coefficient.
selco[1:999] <- 0.97 # the 1000th individual has selection coefficient is 1
selco[1000] <- 1 # all other are 0.97.

for(iGen in 1:1000){
  ind <- sample(ind, 1000, replace = T, prob = selco)
  for(iInd in 1:1000){
    if(ind[iInd] == 0){
      selco[iInd] <- 0.97
    }
    if(ind[iInd] == 1){
      selco[iInd] <- 1
    }
  }
}

q <- data.frame(x = 1:1000, y = ind)
plot(1:1000, q$y, 
     xlab = "Generations",
     ylab = "Individuals",
     type = 'l')






######## Function declarations ###########

# If you write any of your own functions for this project, define them here.

lookForOne <- function(x){
  c <- 0
  for(i in 1:length(x)){
    if(x[i] == 1){
      c <- c + 1
    }
  }
  return(c)
}



######### Number 7: Selection with your while loop version of question 4 ###############

# Number 7 should run your population simulation WITH SELECTION
# 10,000 times, or at least 100,
# and calculate the probability of fixation of the mutant allele.
print("Number 7")

FixOrExt <- numeric(1000) # Has the queer strain been fixed or went extinct?

for(it in 1:1000){
  print(it)
  normies <- numeric(1000) # WT
  normies[1000] <- 1 # Mutant
  selCo <- numeric(1000) #WT have selco of 0.97
  selCo[1:999] <- 0.97
  selCo[1000] <- 1 # Mut has selco of 1
  pop <- normies # reset population for while loop
  q <- 1/1000 #Initial frequency of queer strain
  c <- 0 #Number of generation
  while(q > 0 & q < 1){ # While the q allele lives...
    c <- c + 1
    pop <- sample(pop,
                  1000,
                  replace = T,
                  prob = selCo) # Sample again until it dies or lives
    q <- sum(pop) / (1000) # Frequency of queer strain
    
    for(j in 1:1000){
      if(pop[j] == 0){
        selCo[j] <- 0.97 # Renew selection coefficient as according to WT or M
      }
      if(pop[j] == 1){
        selCo[j] <- 1 # Renew selelction coefficient as according to WT or M
      }
    }
    if(q == 0 | q == 1){
      FixOrExt[it] <- q
      break
    }
  }
}

print(paste("The beneficial allele has fixed", lookForOne(FixOrExt), "time(s)."))



# Output the probability of fixation to the console.
# You may also plot all of the curves on one graph, if you wish.




############# Number 8: No selection ##########

# Number 8 should run the simulation many times with NO SELECTION.
print("Number 8")

NFixOrExt <- numeric(1000) # Has the queer strain been fixed or went extinct?

for(it in 1:1000){
  Nnormies <- numeric(1000) # WT
  Nnormies[1000] <- 1 # Mutant
  NselCo <- numeric(1000) #WT have selco of 0.97
  NselCo[1:999] <- 1
  NselCo[1000] <- 1 # Mut has selco of 1
  Npop <- Nnormies # reset population for while loop
  Nq <- 1/1000 #Initial frequency of queer strain
  Nc <- 0 #Number of generation
  for(Rep in 1:1000){
    while(Nq > 0 & Nq < 1){ # While the q allele lives...
      Nc <- Nc + 1
      Npop <- sample(Npop,
                     1000,
                     replace = T,
                     prob = NselCo) # Sample again until it dies or lives
      Nq <- sum(Npop) / (1000) # Frequency of queer strain
      if(Npop[Rep] == 0){
        NselCo[Rep] <- 1 # Renew selection coefficient as according to WT or M
      }
      if(Npop[Rep] == 1){
        NselCo[Rep] <- 1 # Renew selelction coefficient as according to WT or M
      }
      
      if(Nq == 0 | Nq == 1){
        NFixOrExt[it] <- Nq
        break
      }
    }
  }
}

print(paste("The neutral allele has fixed", lookForOne(NFixOrExt), "time(s)."))



# Output the probability of fixation to the console.







############## Question 9 ##############

BenAlleleaNsWeR <- "if an allele has a fitness higher than the wild type, so a positive selection coefficient, then that allele has an advantage when it is competing with unchanged alleles in the population. And if it has an advantage in competition, it can survive better than the wild types and consequently can reproduce with greater numbers rearing greater number of offspring at a greater rate than the wild type. This raises the possibility of the altered allele fixing in the population. We can see this happening in Question 7, at a rate of 2s, or 2(0.03) in Question 7's case. There's a relative low number of fixation events because the fitness of the altered allele (1.00) wasn't tremendously higher than the wild type's fitness (0.97)."
print(BenAlleleaNsWeR)


NeutAlleleAnswer <- "if an allele has a fitness equal to the wild type, then there is no advantage nor disadvantage for the altered allele in competition. Therefore, the rate of a neutral allele surviving in the population is just the number of that allele in individuals over the number of total individuals in a population, since no individuals have a greater or lesser chance to reproduce than the wild type. In Question 8's case, since there was only one individual with the neutral altered allele in the population, the neutral allele fixed one time out of 1000 iteration."
print(NeutAlleleAnswer)

