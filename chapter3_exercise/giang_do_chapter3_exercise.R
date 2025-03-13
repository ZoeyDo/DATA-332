#exercise 1: Can you spot the difference between a character string and a number? Here’s a test: Which of these are character strings and which are numbers? 1, "1", "one".

1 -> a number
"1" -> a character string because of the quotation marks
"one" -> a character string because of the quotation marks

#exercise 2: Create an atomic vector that stores just the face names of the cards in a royal flush, for example, the ace of spades, king of spades, queen of spades, jack of spades, and ten of spades. The face name of the ace of spades would be “ace,” and “spades” is the suit. Which type of vector will you use to save the names?
faces <- c("ace", "king", "queen", "jack", "ten")

type of vector: character 

#exercise 3: Create the following matrix, which stores the name and suit of every card in a royal flush.
##      [,1]    [,2]
## [1,] "ace"   "spades"
## [2,] "king"  "spades"
## [3,] "queen" "spades"
## [4,] "jack"  "spades"
## [5,] "ten"   "spades"

hand <- c("ace", "spades", "king", "spades", "queen", "spades", "jack",
           "spades", "ten", "spades")
matrix(hand, nrow = 5, byrow = TRUE) 
matrix(hand, ncol = 2, byrow = TRUE)

#exercise 4: Many card games assign a numerical value to each card. For example, in blackjack, each face card is worth 10 points, each number card is worth between 2 and 10 points, and each ace is worth 1 or 11 points, depending on the final score. Make a virtual playing card by combining “ace,” “heart,” and 1 into a vector. What type of atomic vector will result? Check if you are right.
card <- c("ace", "hearts", 1) 
card

type of vector: character 

#exercise 5: Use a list to store a single playing card, like the ace of hearts, which has a point value of one. The list should save the face of the card, the suit, and the point value in separate elements.
card <- list("ace", "hearts", 1) 
card
