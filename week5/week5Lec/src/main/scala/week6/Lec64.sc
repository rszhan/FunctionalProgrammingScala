val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

//capitalOfCountry("andorra") //NoSuchElementException
capitalOfCountry get "andorra"
capitalOfCountry get "US"
