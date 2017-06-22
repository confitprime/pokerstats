# FUnction PokerReturn
# Features: returns exact poker probabilities

pokerprobs <- function (handtype) {
  if (handtype == 'RF') {
    numerator <- choose(n=4,k=1)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'SF') {
    numerator <- choose(n=4,k=1) * choose(n=10,k=1)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == '4C') {
    numerator <- choose(n=13,k=1) * choose(n=4,k=4) * choose(n=12,k=1) * choose(n=4,k=1)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'F') {
    numerator <- choose(n=4,k=1) * choose(n=13,k=5)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'S') {
    numerator <- (choose(n=4,k=1)^5) * choose(n=13,k=5)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'C3') {
    numerator <- choose(n=4,k=3) * choose(n=13,k=1) * choose(n=12,k=2) * (choose(n=4,k=1)^2)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == '2P') {
    numerator <- choose(n=4,k=2) * choose(n=13,k=2) * choose(n=4,k=2) * choose(n=11,k=1) * choose(n=4,k=1)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == '1P') {
    numerator <- choose(n=4,k=2) * choose(n=13,k=1) * choose(n=12,k=3) * (choose(n=4,k=1)^3)
    denominator <- choose(n=52,k=5)
    return(numerator/denominator)
  }
  else if (handtype == 'PP') {
    numerator <- choose(n=4,k=2)*choose(n=13,k=1)
    denominator <- choose(n=52,k=2)
    return (numerator/denominator)
  }
}
