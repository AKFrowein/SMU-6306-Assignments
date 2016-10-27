##########
##Anna Frowein
##Live Session 7 Assignment
##MSDS 6306 Section 404
##########

#Question 1
#Part A Function to Count in base 7
#To be perfectly honest I have never really delt with changing bases or I do not truely understand why base 7 is counted like is in order to actually write the function for it.
#What I was atempting to do was create a function that would return a vector (y) with the length of variable (n) provided in function (x), by taking the values 0-6 and repeating them adding 10 each time.

p7<-function(n){
i=0:6
for (i in 1:n) 
      V<-i
  {i=i+10}
  return(V)
 }
#Test of p7 function.
p7(7)


#Part B Function to Convert Base 10 to Base 7, as provided to by Professor
base10to7<-function(x){
  i=0
  sum=0
  while(x%/%7!=0){
    sum<-sum+((x%%7)*(10^i))
    i=i+1
    x<-x%/%7
  }
  sum<-sum+((x%%7)*(10^i))
  return(sum)
}

#Test of base10to7 Function
base10to7(100)

#Part B Create Function for Base 7 to Base 10 conversion

#This function will take a number in base 7 and convert it to a decimal (Base10) value
#It requires one arguement to be inputed, the base 7 value (y).
base7to10<-function(y){
  i=0
  sum=0
  while(y%/%10!=0){
    sum<-sum+((y%%10)*(7^i))
    i=i+1
    y<-y%/%10
  }
  sum<-sum+((y%%10)*(7^i))
  return(sum)
}
#Test of the base7to10 function
  base7to10(202)
 
  
#Part D Base k to Base 10 and Base 10 to Base k.

#This function will take a decimal (base10) value and convert to to the desired base
#The function requires that two argurments be provided, first the decimal value (x) to be converted and then the desired base (k)
base10toK<-function(x,K){
    i=0
    sum=0
    while(x%/%K!=0){
      sum<-sum+((x%%K)*(10^i))
      i=i+1
      x<-x%/%K
    }
    sum<-sum+((x%%K)*(10^i))
    return(sum)
  }
  #Test of Base10toK Function
  base10toK(100,4)
  
  #Base k to Base 10 Function this will take a base k and convert it base 10.  
  #The funcition requires that both the number (y) and the Base (k) arguments be provided.
  baseKto10<-function(y,k){
    i=0
    sum=0
    while(y%/%10!=0){
      sum<-sum+((y%%10)*(k^i))
      i=i+1
      y<-y%/%10
    }
    sum<-sum+((y%%10)*(k^i))
    return(sum)
  }
  
  #Test of BaseKto10 Function, tested with Base4 values
  baseKto10(1210,4)