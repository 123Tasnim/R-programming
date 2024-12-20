1+1
2+2
10-8
3^2
sqrt(10)
pi
2+3*4
9+3i
x<-1
print(x)
x<-5.8
y<-7.2
# z<-7
x*y*z # this is comment
abs(x)
ceiling(y)
floor(x)
x<- "hello world"
toupper(x)#capital conv
strsplit(x,split=" ")# split but strin type
strsplit(x,split="")# splite charater
strsplit(x,split="e")#split but after e
str<-"Hello World"
nchar(str)#num of char
str1<- "Tasnim"
str2<-"Sumiya"
paste(str1,str2)#with space
paste(str1,str2,sep="")#paste but without space
cat(str)# print but there haven't any change 
str<- "i am tasnim\"sumiya\",abcd."# include a string for this para
str
cat(str)
a<-80# if else condition 
b<-50
if (a>b){
  print("a is greater then b")
}else{
  print("a is greater then b")
  
}
a<-200 # else if condition 
b<-330
if (b>a){
  print("b is greater then a")
}else if(a==b){
  print("a and b are equal")
}else{
  print("a is greater then b")
  
}
#switch case
switch (2,"mango","lichi","apple")
switch ("length","color"="light red","shape"= "small","length=10")
#loop
i<-3
while (i<10){
  print(i)
i<-i+1
}

#loop while(break)
i<-1
while (i<6){
  print(i)
  i <-i+1
  if (i == 7){
    break
  }
}
#loop while (next)
i<-0
while (i<6){
  i <-i+1
  if (i == 3){
    next
  }
  print (i)
}


#for loop
for(x in 5:10){
  print (x)
}
# for(nested)
for(x in 1:2){
  for (y in 1:3){
  print (x*y)
}
}

#function
add_numbers<-function (a,b){
  sum <-a+b
  return(sum)
  
}
print (add_numbers(4,5))