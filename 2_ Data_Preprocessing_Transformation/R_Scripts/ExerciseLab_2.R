
# One time activity
#mtcars$cyl <- as.factor(mtcars$cyl)
#mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"), levels = c(0, 1))

# 1 - Are there more automatic or manual cars?
if (sum(mtcars$am == "Automatic") > sum(mtcars$am == "Manual")) {
  print(" Automatic cars are more")
} else { 
  print("Manual cars are more")
}

# 2 - Which car is heaviest, and which is fastest?
print(paste("heaviest car : ", row.names(mtcars)[which.max(mtcars$wt)]))
print(paste("fastest car : ", row.names(mtcars)[which.max(mtcars$qsec)]))

# 3 - Do automatic or manual cars have on average a better mpg?
if (mean(mtcars$mpg[which(mtcars$am == "Automatic")]) > mean(mtcars$mpg[which(mtcars$am == "Manual")])) {
  print("Automatic cars have better mpg on average")
} else if (mean(mtcars$mpg[which(mtcars$am == "Automatic")]) < mean(mtcars$mpg[which(mtcars$am == "Manual")])) {
  print("Manual cars have better mpg on average")
} else {
  print ("Both cars have same mpg on average")
}

# 4 - How many cars have above average hp?
print(sum(mtcars$hp > mean(mtcars$hp)))

# 5 - Of the cars that have above average hp, how many have 6 cylinders?
print(sum(mtcars$hp > mean(mtcars$hp) & mtcars$cyl == 6))

# 6 - Of the cars that have above average hp, and 6 cylinders, how many are automatic? Questions covering material we haven’t covered yet (in case you finish early)
print(sum(mtcars$hp > mean(mtcars$hp) & mtcars$cyl == 6 & mtcars$am == "Automatic"))