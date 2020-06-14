#Brian Holliday
#Professor Li
#Intro to Data Minning
#20 March 2020


#Project 4b



#Problem 1
#The data was cleaned in excel
setwd('/home/holliday4434/Downloads')
data <- read.csv('Attribute DataSet.csv', header = TRUE, sep = ',')
head(data, 5)
head(data[c('Price', 'Size', 'Season','Waistline','Recommendation')])
data1 <- data[c('Price', 'Size', 'Season','Waistline','Recommendation')]
#Making sure each attribute has unique data
unique(data1$Price)
unique(data1$Size)
unique(data1$Season)
unique(data1$Waistline)
unique(data1$Recommenndation)
print(data1$Recommendation)
#unique set of three generation
#Generate all k-itemsets 
#attr_list has to be factor
library(CombMSC)
gen_f = function(k,attr_list){
  library(CombMSC)   
  #Clean up the NA
  for (i in 1:length(attr_list)){ 
    #i=1
    x <- attr_list[[i]]
    x<-x[!is.na(x)]   
    attr_list[[i]] <- x
  }
  m=length(attr_list)
  sub <- subsets(m,k)
  #count the total number od cases
  length_all <-0
  for (i in 1:nrow(sub)){
    length_all<-length_all+nrow(expand.grid(attr_list[sub[i,]]))    
  }
  all <- data.frame(matrix(rep("NAP",length_all*m),
                           ncol=m,nrow=length_all, 
                           dimnames=list(NULL, names(attr_list))))
  #combine levels
  for (i in 1:ncol(all)){
    levels(all[,i]) <- c(levels(all[,i]),
                         levels(data.frame(attr_list[i])[,1]))
  }
  expd_sum =0 
  for (i in 1:nrow(sub)){ 
    ## i=5
    expd <- expand.grid(attr_list[sub[i,]])       
    for (j in 1:nrow(expd)){
      all[expd_sum+j, sub[i,]] <- expd[j,]
    }   
    expd_sum <- expd_sum+nrow(expd)
    #########all[1, sub[1,]]<- expd[1,], doesn't match becuase the factor values don't match
  }
  return(all)
}
head(data1,1)


#Making sure each attribute has unique data
unique(data1$Price)
unique(data1$Size)
unique(data1$Season)
unique(data1$Waistline)
unique(data1$Recommenndation)

clothes <- list(price=c("Average", "Low", "Medium", "High", "very-high", "-"),
                  size=c("free", "L", "M", "S", "XL", "-"),
                  season=c("Summer", "Winter", "Spring", "Autumn", "-"),
                  waistline=c("empire", "natural","null", "dropped", "-"),
                  recommendation=c("1", "0"))

comb3 <- gen_f(3, clothes)
#Now that we have all combinations of three lets get the support

#Define support function

supp_f = function(itemset,data)
{ # itemset = list(pizza="NAP",apple="NAP",coke="NAP",meat="1",vegetable="-1") 
  # itemset <-x
  index_item <- match(names(itemset),names(data))
  ndata <- data[,index_item]
  itemset <- as.vector(t(itemset))
  n <- nrow(ndata)
  sel <-c(1:n)
  L = length(itemset)
  for (i in 1: L)
  {
    if (itemset[i]!="NAP")
    {
      sel <- sel[ndata[sel,i]==itemset[i]]
    }
  }
  supp <- length(sel)/n
  return(supp)
}

head(data1)
comb3

#1022 length of comb3
store <- rep(0:1022)
store[3]
#loop to compute support of every combination
for (i in 1:1023) {
  itemset <-  list(Price=toString(comb3[i,1]),
                 Size=toString(comb3[i,2]),
                 Season=toString(comb3[i,3]),
                 Waistline=toString(comb3[i,4]),
                 Recommendation=toString(comb3[i,5]))  
  store[i] <- supp_f(itemset,data1)
}

#Values for max1

max1_index <- which.max(store)
max1_columns <- comb3[max1_index,]
max1 <- max(store)

max1_index
max1_columns
max1

#Values for max2

max2_index
max2_columns
max2

max2
max1
    
comb3[679,] <- NA

comb3 <- na.omit(comb3)

comb3[679,] 

#Now the top support value has been omited we will repeat the previous process to get the second highest support value

store2 <- rep(1:1022)

for (i in 1:1022) {
  itemset <-  list(Price=toString(comb3[i,1]),
                   Size=toString(comb3[i,2]),
                   Season=toString(comb3[i,3]),
                   Waistline=toString(comb3[i,4]),
                   Recommendation=toString(comb3[i,5]))  
  store2[i] <- supp_f(itemset,data1)
}

store2[1022] <- 0.0

store2[679] <- 0.0
which.max(store2)
max2_index <- which.max(store2)

max2_value <- store2[max2_index]
max2_columns <- comb3[950,]
max2_columns

#Now the top support value has been omited we will repeat the previous process to get the second highest support value


comb3[950,] <- NA
comb3 <- na.omit(comb3)
store3 <- rep(1:1020)
length(comb3)

for (i in 1:1020) {
  itemset <-  list(Price=toString(comb3[i,1]),
                   Size=toString(comb3[i,2]),
                   Season=toString(comb3[i,3]),
                   Waistline=toString(comb3[i,4]),
                   Recommendation=toString(comb3[i,5]))  
  store3[i] <- supp_f(itemset,data1)
}
store3


store3[679]


max3_columns <- comb3[679,]
max3_value <- store3[679]
max3_index <- which.max(store3)


#Now we have the columns for three highest support values
max1_columns
max2_columns
max3_columns

#Problem2
#Now that we are done with the Frequent Itemset Generation
#We are going to do the Rules Generation

#lets get our confidence function

conf_f = function(A,B,data)
{ #A <- c("Average","NAP","NAP","Natural","NAP")
  #B <- c("NAP","NAP","NAP","NAP","0")
  #First define a merging function
  union_f = function(A,B)
  { 
    n=length(A)
    AB = A
    for (i in 1:n)
    { # i=4
      if (B[i]!="NAP")
      {
        AB[i]=B[i]
      }
    }
    return(AB)
  }
  n <- nrow(data)
  AB <- union_f(A,B)
  conf <- supp_f(AB,data)/supp_f(A,data)
  return(conf)
}

#Value of max support values
max1_columns
#We are going to find the implication rules for these columns

#1 of 12
#{Average, natural} <- {0}
A <- list(Price="Average", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="NAP")
B <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="0")

conf1 <- conf_f(A,B,data1)

#2 0f 12
#{0} <- {Average, natural}
B<- list(Price="Average", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="NAP")
A<- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="0")

conf2 <- conf_f(A,B,data1)

#3 of 12
#{Average,0} <- {natural}
A <- list(Price="Average", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="0")
B <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="NAP")
conf3 <- conf_f(A,B,data1)

#4 of 12
#{natural} <- {Average,0}
B <- list(Price="Average", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="0")
A <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="NAP")
conf4 <- conf_f(A,B,data1)

#5 of 12
#{natural,0} <- {Average}
A <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="0")
B <- list(Price="Average", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="NAP")
conf5 <- conf_f(A,B,data1)

#6 of 12
#{Average} <- {natural,0}
B<- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="0")
A <- list(Price="Average", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="NAP")
conf6 <- conf_f(A,B,data1)

#7 of 12
#{Average} <- {Natural}
A <- list(Price="Average", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="NAP")
B <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="NAP")
conf7 <- conf_f(A,B,data1)

#8 of 12
#{natural} <- {Average}
A <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="NAP")
B <- list(Price="Average", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="NAP")
conf8 <- conf_f(A,B,data1)

#9 of 12
#{Average} <- {0}
A <- list(Price="Average", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="NAP")
B <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="0")
conf9 <- conf_f(A,B,data1)

#10 of 12
#{0} <- {Average}
A <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="0")
B <- list(Price="Average", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="NAP")
conf10 <- conf_f(A,B,data1)

#11 of 12
#{0} <- {natural}
A <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="NAP", Recommendation ="0")
B <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="NAP")
conf11 <- conf_f(A,B,data1)

#12 of 12
#{natural} <- {0}
A <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="NAP")
B <- list(Price="NAP", Size="NAP",Season ="NAP", Waistline="natural", Recommendation ="0")
conf12 <- conf_f(A,B,data1)

#Lets find the highest confidence

conf_list <- list(conf1, conf2, conf3, conf4, conf5, conf6,
                  conf7, conf8, conf9, conf10, conf11, conf12)

conf_max_index <- which.max(conf_list)
conf_list[conf_max_index]
#We get that conf11 is the highest vaulue for confidence with 
#{0} <- {natural}

#Lets find the second highest confidence

conf_list[11] <- 0
conf_max_index2 <- which.max(conf_list)
conf_list[conf_max_index2]
#We get conf3 is the second highest value with
#{Average,0} <- {natural}

#These results show that {Average, natural, 0}
#is the set with the highest support. Within the set
#the rule with the high confidence level is {0} <- {natural}








