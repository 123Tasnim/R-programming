install.packages("tibble")# for packages install
libary
patientID<-c(1,2,3,4)
age <- c(25,34,28,52)
diabetes <- c("Type1","Type2","Type1","Type1")
status<-c("poor","Improved","Excellent","poor")
patientdata <- data.frame(patientID,age,diabetes,status)
patientdata #then run and righside patientdata click
patientdata[1,2]
patientdata[c("age","diabetes","status")]#for add coloum
patientdata$bloodgrop <-c("a+","a-","o+","o-")
patientdata
patiendata[nrow(patientdata) + 1,] <- c(5,20,"type2","improved","a+")
patientdata


g<-"My First List"
h<-c(25,26,18,39)
j<-matrix(1:10,nrow=5)
k<-c("one","two","three")
mylist<-list(title=g,ages=h,j,k)
mylist



#line by line run
var1 = readline (prompt = "Enter any value : ")#console value input

var2 = readline (prompt = "Enter any value : ")#console value input

var2 =as.integer (var2)
print (var1)
print (var2)


x=scan()
print(x)

mydata <- data.frame(age=numeric(0),gender=character(0),weights=numeric(0))
mydata<-edit(mydata)

mydata1 <- read.csv("C:/Users/user/Downloads/Book1.csv", header=TRUE, sep=",")
mydata1
