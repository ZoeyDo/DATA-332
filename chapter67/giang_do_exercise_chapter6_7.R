#Chapter 6
#Exercise 1: Will R be able to find deck and return an answer when I call the new version of deal, such as deal()?
-> Yes. deal will still work the same as before. R will run deal in a runtime environment that is a child of the global environment. 

#Exercise 2: Rewrite the deck <- deck[-1, ] line of deal to assign deck[-1, ] to an object named deck in the global environment. Hint: consider the assign function.
deal <- function() {
  card <- deck[1, ]
  assign("deck", deck[-1, ], envir = globalenv()) 
  card
}

deal()


#Exercise 3: Rewrite shuffle so that it replaces the copy of deck that lives in the global environment with a shuffled version of DECK, the intact copy of deck that also lives in the global environment. The new version of shuffle should have no arguments and return no output.
shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}

#Chapter 7
#Exercise 1: Turn the preceding statement into a logical test written in R. Use your knowledge of logical tests, Boolean operators, and subsetting from Chapter 4. The test should work with the vector symbols and return a TRUE if and only if each element in symbols is the same. Be sure to test your code on symbols.
symbols[1] == symbols[2] & symbols[2] == symbols[3]

#Exercise 2: Use R’s logical and Boolean operators to write a test that will determine whether a vector named symbols contains only symbols that are a type of bar. Check whether your test works with our example symbols vector. Remember to describe how the test should work in English, and then convert the solution to R.
all(symbols %in% c("B", "BB", "BBB"))

#Exercise 3: How can you tell which elements of a vector named symbols are a C? Devise a test and try it out.
symbols <- c("C", "DD", "C")
symbols == "C"

#Exercise 4: Write a method for adjusting prize based on diamonds. Describe a solution in English first, and then write your code.
The adjusted prize will equal:
  prize * 2 ^ diamonds
which gives us our final score script:
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")

  if (same) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
      "B"=10,"C"=10,"0"=0) 
    prize <- unname(payouts[symbols[1]])
  } else if (all(bars)) { prize <- 5
  }else{
    cherries <- sum(symbols == "C") prize <- c(0, 2, 5)[cherries + 1]
  }
  diamonds <- sum(symbols == "DD")
  prize * 2 ^ diamonds

