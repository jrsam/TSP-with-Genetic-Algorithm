input.data = read.table("D:\\Sem8\\Soft Computing\\FinalPackage\\tsp_xy.txt")
input.matrix = matrix(unlist(input.data),ncol = ncol(input.data))
pop.size = 20 #20 50 100
pc = 0.65
pm = 0.05
generations = 100

options(scipen=999)
#every item of population has complete tour of all cities
#fitness is distance in TSP
getUniquePop <- function(pop) {
  while( nrow( unique(pop)  ) != pop.size ) {
    for (i in 1:pop.size) {
      print("not unique")
      pop[i,] = pop[i,sample(ncol(pop))] #shuffle elements of each row
    }
  }
  return(pop)
}
plotTour <- function(population) {
  for( i in 1:pop.size ) {
    dist<-c()
    for( j in 1: ncol(population) - 1) {
      XDist =  (input.data[ population[i,j],1 ] -  input.data[ population[i,j+1],1 ] )^2
      yDist =  (input.data[ population[i,j],2 ] -  input.data[ population[i,j+1],2 ] )^2
      dist <-c(dist,sqrt(XDist + yDist))
    }
    distance[i,1]=sum(dist)
    
  }
  finalTour = matrix(population[which.min(distance), ],1,ncol(population))
  x = c()
  y = c()
  print("final tour")
  print(finalTour)
  for(i in 1:ncol(finalTour)) {
    x = c(x,input.matrix[finalTour[1,i],1])  
    y = c(y,input.matrix[finalTour[1,i] ,2])
  }
  #back to first city
  x = c( x,input.matrix[finalTour[1,1],1] )
  y = c( y,input.matrix[finalTour[1,1],2] )
  plot(x,y,type = "l",main=paste("Generations Roulette Wheel = ",generations) )
}

calcFitness <- function(population) {
  for( i in 1:pop.size ) {
    dist<-c()
    for( j in 1: ncol(population) - 1) {
      XDist =  (input.data[ population[i,j],1 ] -  input.data[ population[i,j+1],1 ] )^2
      yDist =  (input.data[ population[i,j],2 ] -  input.data[ population[i,j+1],2 ] )^2
      dist <-c(dist,sqrt(XDist + yDist))
    }
    distance[i,1]=sum(dist)
    #fitness[i,1]=1/distance[i,1]
    fitness[i,1] = distance[i,1]
  }
  cat("fittest ", min(distance),"\n"  ) #printing fittest tour distance
 #scat( "tour ", population[which.min(distance), ]," \n"  )

}
pop.matrix = matrix(1:nrow(input.matrix),nrow = pop.size,ncol = nrow(input.matrix),byrow = TRUE )

for (i in 1:pop.size) {
  
  pop.matrix[i,] = pop.matrix[i,sample(ncol(pop.matrix))] #shuffle elements of each row
  
}
pop.matrix = getUniquePop(pop.matrix)

for ( i in 1:generations  ){
    
  distance <-matrix(0,nrow=pop.size,ncol=1)
  fitness<-matrix(0,nrow=pop.size,ncol=1)
  prob<-matrix(0,nrow=pop.size,ncol=1)
  cumprob<-matrix(0,nrow=pop.size,ncol=1)

  #assigning distance of each tour
  for( i in 1:pop.size ) {
    dist<-c()
    for( j in 1: ncol(pop.matrix) - 1) {
      XDist =  (input.data[ pop.matrix[i,j],1 ] -  input.data[ pop.matrix[i,j+1],1 ] )^2
      yDist =  (input.data[ pop.matrix[i,j],2 ] -  input.data[ pop.matrix[i,j+1],2 ] )^2
      dist <-c(dist,sqrt(XDist + yDist))
    }
    distance[i,1]=sum(dist)
    fitness[i,1]=1/distance[i,1] #minimising distance
    #fitness[i,1] = distance[i,1] #Roulette wheel not suitable for minimisaiton prob. 
    #hence min function converted to max function. but causes confusion, by selecting max fitness
  }
  cum_fit=sum(fitness)
  for(i in 1:pop.size){
    prob[i,1]= 1 - ( fitness[i,1]/cum_fit ) #1 - prob to overcome drawback of selecting min prob
    cumprob[i,1] = sum(prob[1:i,1])
  }
  str<-matrix(0,nrow=pop.size,ncol=1) #String number
  trueCount<-matrix(0,nrow=pop.size,ncol=1) #True count
  for(i in 1:pop.size){
    rno = runif(1,0,1)
    for(j in 1:pop.size){
      if(cumprob[j,1] > rno){
        str[i,1]=j
        trueCount[j,1]=trueCount[j,1]+1
        break
      }
      
    }
  }
  mating.pool = matrix(0,nrow = pop.size,ncol = nrow(input.matrix))
  for( i in 1:pop.size) {
    mating.pool[i,] = pop.matrix[str[i,1],]  
  }
  
  i = 1
  inter.pop = matrix(0,nrow = pop.size,ncol = nrow(input.matrix))
  while( i<= pop.size-1 ) {
    co.point = as.integer( runif(1,1,nrow(input.matrix)) )
    if( pc  >= runif(1,0,1) ) {
      #first cross-over child    
      inter.pop[i,1:co.point] = mating.pool[i,1:co.point]
      inter.pop[i,(co.point+1):nrow(input.matrix)] = setdiff(mating.pool[i+1,] , mating.pool[i,1:co.point])
      #second cross-over child      
      inter.pop[i+1,1:co.point] = mating.pool[i+1,1:co.point]
      inter.pop[i+1,(co.point+1):nrow(input.matrix)] = setdiff(mating.pool[i,] , mating.pool[i+1,1:co.point])
      
      
      
    }
    else {
      inter.pop[i,] = mating.pool[i,] 
      inter.pop[i+1,] = mating.pool[i+1,] 
    }
    
    i = i + 2
    
  }
  #print(inter.pop)
  for (i in 1:pop.size  ) {
    site1 = as.integer( runif(1,1,nrow(input.matrix)) )
    site2 = as.integer( runif(1,1,nrow(input.matrix)) )
    #cat("site 1",site1)
    #cat("site 2",site2)
    if( pm  >= runif(1,0,1) ) {
      temp = inter.pop[i,site1]
      inter.pop[i,site1] = inter.pop[i,site2]
      inter.pop[i,site2] = temp
    }
  }   
  #print(inter.pop)
  pop.matrix = inter.pop
  calcFitness(pop.matrix)    
}
plotTour(pop.matrix)
