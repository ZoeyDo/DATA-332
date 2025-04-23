#Chapter 8

#Exercise 1: Modify play to return a prize that contains the symbols associated with it as an attribute named symbols. Remove the redundant call to print(symbols):
#play <- function() { 
#symbols <- get_symbols() 
#print(symbols) 
#score(symbols)
#}

play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols) attr(prize, "symbols") <- symbols prize
}

#Exercise 2: Write a new print method for the slots class. The method should call slot_display to return well-formatted slot-machine output. What name must you use for this method?

print.slots <- function(x, ...) { 
  slot_display(x)
}

Name: print.slots 

#Exercise 3: Modify the play function so it assigns slots to the class attribute of its output:
#play <- function() {
#symbols <- get_symbols() 
#structure(score(symbols), symbols = symbols)
#}

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}

class(play())

play()

#Chapter 9

#Exercise 1: Use expand.grid to make a data frame that contains every possible combination of three symbols from the wheel vector:
#wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
#Be sure to add the argument stringsAsFactors = FALSE to your expand.grid call; otherwise, expand.grid will save the combinations as factors, an unfortunate choice that will disrupt the score function.

combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)

combos

get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0") 
  sample(wheel, size = 3, replace = TRUE,
    prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
}

#Exercise 2: Isolate the previous probabilities in a lookup table. What names will you use in your table?

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06,
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

#Exercise 3: Look up the probabilities of getting the values in Var1. Then add them to combos as a column named prob1. Then do the same for Var2 (prob2) and Var3 (prob3).

combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]

head(combos, 3)

#Exercise 4: Calculate the overall probabilities for each combination. Save them as a column named prob in combos, then check your work. You can check that the math worked by summing the probabilities. The probabilities should add up to one, because one of the combinations must appear when you play the slot machine. In other words, a combination will appear, with probability of one.
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3

head(combos, 3)

sum(combos$prob)

symbols <- c(combos[1, 1], combos[1, 2], combos[1, 3])

#Exercise 5: Construct a for loop that will run score on all 343 rows of combos. The loop should run score on the first three entries of the ith row of combos and should store the results in the ith entry of combos$prize.
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3]) 
  combos$prize[i] <- score(symbols)
}

head(combos, 3)

sum(combos$prize * combos$prob)

#Exercise 6: Calculate the expected value of the slot machine when it uses the new score function. You can use the existing combos data frame, but you will need to build a for loop to recalculate combos$prize.
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3]) 
  combos$prize[i] <- score(symbols)
}

sum(combos$prize * combos$prob)
