# FUnction PokerReturn
# Features: returns exact poker probabilities

pokerprobs <- function (handtype) {
  if (handtype == 'RF') {
    # Royal Flush
    numerator <- choose(n=4,k=1)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'SF') {
    # Straight flush
    numerator <- choose(n=4,k=1) * choose(n=10,k=1)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == '4C') {
    # Four of a kind
    numerator <- choose(n=13,k=1) * choose(n=4,k=4) * choose(n=12,k=1) * choose(n=4,k=1)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'F') {
    # Flush
    numerator <- choose(n=4,k=1) * choose(n=13,k=5)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'S') {
    # Straight
    numerator <- (choose(n=4,k=1)^5) * choose(n=13,k=5)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'C3') {
    # Three of a kind
    numerator <- choose(n=4,k=3) * choose(n=13,k=1) * choose(n=12,k=2) * (choose(n=4,k=1)^2)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == '2P') {
    # Two pairs
    numerator <- choose(n=4,k=2) * choose(n=13,k=2) * choose(n=4,k=2) * choose(n=11,k=1) * choose(n=4,k=1)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == '1P') {
    # One pair
    numerator <- choose(n=4,k=2) * choose(n=13,k=1) * choose(n=12,k=3) * (choose(n=4,k=1)^3)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'PP') {
    # Pocket Pair
    numerator <- choose(n=4,k=2)*choose(n=13,k=1)
    denominator <- choose(n=52,k=2)
    return (numerator/denominator)
  }
}
