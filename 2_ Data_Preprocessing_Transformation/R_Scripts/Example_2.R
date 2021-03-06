name <- c("Amy", "Bill", "Carl")
DAD <- c(80, 65, 50) 
BDA <- c(70, 50, 80)
gender <- as.factor(c("F", "M", "M")) 
nationality <- as.factor(c("IRL", "UK", "IRL")) 
age <- c(20, 21, 22)
student.df <- data.frame(name, age, gender, nationality, DAD, BDA)
attributes(student.df)
student.df$average <- (student.df$BDA + student.df$DAD)/2
student.df$name <- as.character(student.df$name)
student.df <- rbind(student.df, c("Has", 25, "F", "AUS", 55, 70))
student.df$nationality <- as.character(student.df$nationality)
student.df$nationality <- as.factor(student.df$nationality)
student.df$BDA <- as.numeric(student.df$BDA)
student.df$DAD <- as.numeric(student.df$DAD)
tapply(student.df$BDA, student.df$nationality, mean)
table(student.df$nationality)
barplot(table(student.df$nationality),xlab = "Nationality", ylab = "Count")
str(student.df)

mtcars
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"), levels=c(0,1))
hist(x = mtcars$mpg, breaks = 7)
