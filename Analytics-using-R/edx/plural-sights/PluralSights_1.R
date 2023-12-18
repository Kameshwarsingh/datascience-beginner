#https://www.stat.berkeley.edu/~s133/dates.html
d <- as.Date("2020/03/27",format='%Y/%m/%d')
class(d)
print(d)

#current date
format(Sys.time(), "%a %b %d %H:%M:%S %Y")


##function

f <- function(x) { 
  x+1 
}


f(10)

## vectors
v <- c(1,2,3,4)
v
class(v)


## sequence
s=1:10
s

##matrix
m <- matrix(data=1:6,nrow=2,ncol=3)
m
class(m)


#array(data = NA, dim = length(data), dimnames = NULL)
a <- array(data=1:8, dim=c(2,4))
print(a)
?array
is.array(a)


#list - hetrogeneous data type
l<-list(TRUE,2,"test")
l

#factor - categorical variables\
?factor
categories <- c("male", "female","male","Male", "female")
f <- factor(tolower(categories))
f
levels(f)
unclass(f)


df <- data.frame(
              Name=c("Cat","Dog","Pig","Cow"),
              HowMany=c(1,2,3,4),
              IsPet=c(TRUE,TRUE,FALSE,FALSE)
              )
print(df)
df[1,2]
df[2:4,]

#subsetting - all rows, whose IsPet is TRUE
df[df$IsPet==TRUE,]
df[df$Name %in% c("Cat","Pig"),]


### R is vectorized language, that means every data type inherites vector behaviour.
1+2
c(1,2,3)+c(4,5,6)


### identical compares two matrix/array
identical(df,df)
identical(df,df[df$IsPet==TRUE,])

