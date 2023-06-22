# Module 3, Lab 1
# from "Data Science and Big Data Analytics" 
#
###################################################
## Step 2: Examine the Workspace 
###################################################
ls()

###################################################
## Step 3: Getting Familiar with R 
###################################################
help()
help.start()
demo()
demo(graphics)

###################################################
## Step 5: Working with R: reading external data 
###################################################
setwd("~/LAB01")
lab1 <- read.table("lab1_01.txt", sep="|", header=TRUE)
lab2 <- read.table("lab1_02.txt", sep="|", header=TRUE) 

###################################################
## Step 6: Verify the Contents of the Tables 
###################################################
head(lab1, n=10)
tail(lab2, n=10)

###################################################
## Step 7: Manipulating data frames in R 
###################################################
summary(lab1)

#Remove some extraneous variables (columns)
nlab1 <- lab1[,2:3]

#What did we get? 

dim(nlab1)
typeof(nlab1)
class(nlab1)

###################################################
## Step 8: Investigate Your Data 
###################################################
summary(nlab1)

#Are the correlation values the same or different? 
cor(nlab1)

###################################################
## Step 9: Save the Data Sets 
###################################################

rm(lab1) 
lab1 <- nlab1
save(lab1, lab2, file="Labs.Rdata")
rm(lab1, lab2)
ls()      # make sure they are not in the workspace

###################################################
## Step 10: Continue Investigating  the Data   
###################################################

tellme <- function(x) { 
  p1 <- paste("Type of", x, " is",typeof(x),sep=" ")
  print(p1)
  p2 <- paste("Class of", x, "is", class(x), sep=" ")
  print(p2)
  p3 <- paste("String rep of ",x," is", str(x), sep=" ")
  print(p3)
  p4 <- paste("Names for ", x, "is", names(x), sep=" ")
  print(p4)
  invisible()
}
tellme(t)

###################################################
## Here are examples relating to Module 3 Lesson 1
###################################################
## Example 1: Scalars and Strings
###################################################

n <- 1  # scalar
s <- "Columbus, Ohio"   # string 

###################################################
## Example 2: Vectors of Strings and Numbers
################################################### 

levels <- c("Worst", "Bad", "Mediocre", "Good", "Awesome")
ratings <- c("Worst", "Worst", "Bad", "Bad", "Good", "Bad", "Bad") 
critics <- c("Siskel", "Ebert", "Rowen", "Martin")
movies <- c("The Undefeated", "Snakes on a Plane", "Encino Man", "Casablanca")
attendance <- c(15, 350,175, 400)
reviewers <- c("Siskel", "Siskel", "Ebert", "Ebert", "Rowan", "Martin", "Rowan")

###################################################
## Example 3: Factors and Lists
###################################################

f <- factor(ratings, levels)  
fl <- list(ratings=ratings, critics=critics, 
		movies=movies, attendance=attendance)
	
###################################################
## Example 4: Matrices, Tables, and Data Frames
###################################################

mdat <- matrix(c(1:3, 11:13), nrow = 2, ncol=3, byrow=TRUE,
               dimnames = list(c("row1", "row2"),
					 c("C1", "C2", "C3")))
					 
t <- table(ratings, reviewers)  

###################################################
## Example 5: Defining a Function
###################################################

std <- function(x) {sd(x)}   # defining a function 
v <- c(1:100)              # create a test vector
std(v)


