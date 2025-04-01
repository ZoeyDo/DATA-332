#Prepare data
deck <- data.frame(
  face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
           "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten",
           "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace",
           "king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five",
           "four", "three", "two", "ace", "king", "queen", "jack", "ten", "nine",
           "eight", "seven", "six", "five", "four", "three", "two", "ace"),
  suit = c("spades", "spades", "spades", "spades", "spades", "spades",
           "spades", "spades", "spades", "spades", "spades", "spades", "spades",
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs",
           "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", "diamonds",
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "diamonds",
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "hearts",
           "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts",
           "hearts", "hearts", "hearts", "hearts", "hearts"),
  value=c(13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11,10,9,8, 7,6,5,4,3,2,1,13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11, 10,9,8,7,6,5,4,3,2,1)
)

deal(deck)
head(deck)
ls()

install.packages("deal")  
library(deal)

deal <- function(deck) {
  deck[1, ]  
}

write.csv(deck, file = 'cards.csv', row.names = FALSE)

deck[c(1,1), c(1,2,3)]

vec <- c(6,1,3,6,10,5)
vec[1:3]

deck[1, c("face", "suit", "value")]



deck2 <- deck[1:52, ]
head(deck2)

deck3 <- deck[c(2,1,3:52),]

head(deck3)

random <- sample(1:52, size = 52)
random

deck4 <- deck[random, ]
head(deck4)


deck2 <- deck
deck2$new <- 1:52
head(deck2)
deck2$new <- NULL
head(deck2)

deck2[c(13,26,39,52), ]


deck2[c(13, 26, 39, 52), 3]



deck2$value[c(13, 26, 39, 52)]

deck2$value[c(13, 26, 39, 52)] <- c(14, 14, 14, 14)
deck2$value[c(13, 26, 39, 52)] <- 14

head(deck2, 13)

deck3 <- shuffle(deck)
shuffle <- function(deck) {
  deck[sample(nrow(deck)), ]  
}
head(deck3)
deck4 <- deck
deck4$value <- 0
head(deck4, 13)


#Exercise 1: Extract the face column of deck2 and test whether each value is equal to ace. As a challenge, use R to quickly count how many cards are equal to ace.

deck2$face

deck2$face == "ace"

#Exercise 2: Assign a value of 1 to every card in deck4 that has a suit of hearts.

deck4$suit == "hearts"

deck4$value[deck4$suit == "hearts"]

deck4$value[deck4$suit == "hearts"] <- 1

deck4$value[deck4$suit == "hearts"]

deck4[deck4$face == "queen", ]

deck4[deck4$suit == "spades", ]

#Exercise 3: If you think you have the hang of logical tests, try converting these sentences into tests written with R code. To help you out, I’ve defined some R objects after the sentences that you can use these to test your answers:
#Is w positive?
#Is x greater than 10 and less than 20?
#Is object y the word February?
#Is every value in z a day of the week?

w<-c(-1,0,1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")

w>0
10<x&x<20
y == "February"
all(z %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
             "Saturday", "Sunday"))


