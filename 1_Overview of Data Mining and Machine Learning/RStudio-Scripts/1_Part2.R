s <- paste(1:9, c("X","Y") , sep = "")
print(s)

z <- c(1:3, NA)
print(z == NA)

fruit <- c(5, 10, 1 , 20)
names(fruit) <- c("orange", "banana", "apple", "peach")
lunch <- fruit[c("apple", "orange")]
print(lunch)



x <- c(-5:-1, NA, NA, 1:3)
x[is.na(x)] <- mean(x[!is.na(x)])
print(x)
