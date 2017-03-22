rmsd <- function(set1, set2)
{

  difference <- (set1-set2) #finding difference between coordinates
  squareDifference <- difference*difference
  sumSquareDifference <- (squareDifference[,1] + squareDifference[,2] + squareDifference[,3])
  return(sqrt(mean(sumSquareDifference))) #calculate and return root mean square
   
}
  
rmsd1 <- rmsd(set1, set2)

print(rmsd1)

#calcualte centriod and center both sets at origin for translational motion, not rotation
centriod1 <- colMeans(set1) #compute means of column matrix for set 1
centriod2 <- colMeans(set2) #same as above, for set 2
set1Center <- sweep(set1,MARGIN=2,centriod1,FUN="-") #used default value
set2Center <- sweep(set2,MARGIN=2,centriod2,FUN="-") 
rmsd2 <- rmsd(set1Center,set2Center)

print(rmsd2)
