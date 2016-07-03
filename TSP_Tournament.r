input.data = read.table("D:\\Sem8\\Soft Computing\\FinalPackage\\tsp_xy.txt")
input.matrix = matrix(unlist(input.data),ncol = ncol(input.data))
pop.size = 20 # 50 100
pc = 0.65
pm = 0.05
generations = 300 #100 300
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
optimalTour <- function(population) {
  fitness<-matrix(0,nrow=pop.size,ncol=1)
  distance<-matrix(0,nrow=pop.size,ncol=1)
  for( i in 1:pop.size ) {
    dist<-c()
    for( j in 1: ncol(population) - 1) {
      XDist =  (input.data[ population[i,j],1 ] -  input.data[ population[i,j+1],1 ] )^2
      yDist =  (input.data[ population[i,j],2 ] -  input.data[ population[i,j+1],2 ] )^2
      dist <-c(dist,sqrt(XDist + yDist))
    }
    distance[i,1]=sum(dist)

  }
  cat("fittest ", min(distance),"\n"  ) #printing fittest tour distance
  #cat( "tour ", population[which.min(distance), ]," \n"  )
  
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
  plot(x,y,type = "l",main=paste("Generations Tournament Sel",generations) )
}
calcFitness <- function(tour) { #distance of the tour
    #cat("dim of tour ",dim(tour),"\n")
    #cat("tour ",tour,"\n")
    dist<-c()
    for( j in 1: (ncol(tour) - 1) ) {
      XDist =  (input.data[ tour[1,j],1 ] -  input.data[ tour[1,j+1],1 ] )^2
      yDist =  (input.data[ tour[1,j],2 ] -  input.data[ tour[1,j+1],2 ] )^2
      dist <-c(dist,sqrt(XDist + yDist))
    }
    return( sum(dist) )

}

selectFittest <- function(pop.matrix) { #tournament size 2
  competitor1 = pop.matrix[ as.integer( runif(1,1,nrow(pop.matrix))  ),]
  competitor2 = pop.matrix[as.integer( runif(1,1,nrow(pop.matrix))  ),]
  if( calcFitness(matrix(competitor1,1,ncol(pop.matrix) ) ) <= calcFitness( matrix(competitor2,1,ncol(pop.matrix) )  ) ) {
    return (competitor1)
  }
  else {
    return (competitor2)
  }
}
#initialise population for first run
flag = 0

pop.matrix = matrix(1:nrow(input.matrix),nrow = pop.size,ncol = nrow(input.matrix),byrow = TRUE )

for (i in 1:pop.size) {
  
  pop.matrix[i,] = pop.matrix[i,sample(ncol(pop.matrix))] #shuffle elements of each row
  
}
pop.matrix = getUniquePop(pop.matrix)
for ( i in 1:generations  ){

    distance <-matrix(0,nrow=pop.size,ncol=1)



    mating.pool = matrix(0,nrow = pop.size,ncol = nrow(input.matrix))
    for( i in 1:pop.size) {
        mating.pool[i,] = selectFittest(pop.matrix)
    }
    
    i = 1
    inter.pop = matrix(0,nrow = pop.size,ncol = nrow(input.matrix))
    while( i<= pop.size-1 ) {
      ran = nrow(input.matrix)/2
      co.point= as.integer( runif(1,1,ran) )
      co.point1= as.integer( runif(1,1,ran) )
      co.point2= as.integer( runif(1,ran+1,nrow(input.matrix)) )
      if( pc  >= runif(1,0,1) ) {
        setElt<-c()
        setElt1<-c()
        if(flag == 0){
          #Two point crossover
          #First child
          inter.pop[i,1:co.point1] = mating.pool[i,1:co.point1]
          #p1 and p2 are the populations. setdiff of p2's middle part and p1's first part.
          setElt<-c(setdiff(mating.pool[i+1,(co.point1+1):co.point2] ,inter.pop[i,1:co.point1]))
          for( d in 1:nrow(input.matrix)){
            #Checking d(cities) occurs in first part of p1 and second part of p2. If not add to middle part of p1.
            if(!(d %in% setElt)&& !(d %in% inter.pop[i,1:co.point1])&& !(d %in% mating.pool[i,(co.point2+1):nrow(input.matrix)])){
              setElt<-c(setElt,d)
            }
            setElt<-c(setdiff(setElt,mating.pool[i,(co.point2+1):nrow(input.matrix)]))
          }
          inter.pop[i,(co.point1+1):co.point2] = setElt
          inter.pop[i,(co.point2+1):nrow(input.matrix)] =mating.pool[i,(co.point2+1):nrow(input.matrix)]
          
          #second child
          inter.pop[i+1,1:co.point1] = mating.pool[i+1,1:co.point1]
          setElt1<-c(setdiff(mating.pool[i,(co.point1+1):co.point2] ,inter.pop[i+1,1:co.point1]))
          for( k in 1:nrow(input.matrix)){
            if(!(k %in% setElt1)&& !(k %in% inter.pop[i+1,1:co.point1])&& !(k %in% mating.pool[i+1,(co.point2+1):nrow(input.matrix)])){
              setElt1<-c(setElt1,k)
            }
            setElt1<-c(setdiff(setElt1,mating.pool[i+1,(co.point2+1):nrow(input.matrix)]))
            
          }
          inter.pop[i+1,(co.point1+1):(co.point2)] = setElt1
          inter.pop[i+1,(co.point2+1):nrow(input.matrix)] =mating.pool[i+1,(co.point2+1):nrow(input.matrix)]
        }
        else{
          #first cross-over child    
          inter.pop[i,1:co.point] = mating.pool[i,1:co.point]
          inter.pop[i,(co.point+1):nrow(input.matrix)] = setdiff(mating.pool[i+1,] , mating.pool[i,1:co.point])
          #second cross-over child      
          inter.pop[i+1,1:co.point] = mating.pool[i+1,1:co.point]
          inter.pop[i+1,(co.point+1):nrow(input.matrix)] = setdiff(mating.pool[i,] , mating.pool[i+1,1:co.point])
          
          
        }
        
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
    optimalTour(pop.matrix)  

}
plotTour(pop.matrix)
