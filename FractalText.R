install.packages("ggplot2")
library(ggplot2)

charmatrix <- function (c) {
  
  #computes the blocks of a character
  
  B=list()
  
  
  if (c==" ") {
    
    B[[1]] <- c(0,0,0,0,0,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    do.call(rbind,B)
    
  }
  
  else if (c=="a" | c=="A") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,3,0,3,-1,0)
    B[[3]] <- c(0,6,3,0,0,1)
    B[[4]] <- c(1,3,3,0,0,1)
    B[[5]] <- c(3,7,0,-3,1,0)
    B[[6]] <- c(3,3,0,-3,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="e" | c=="E") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(0,3,3,0,0,1)
    B[[4]] <- c(1,6,3,0,0,1)
    B[[5]] <- c(1,0,3,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="i" | c=="I") {
    
    B[[1]] <- c(0,0,4,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,6,4,0,0,1)
    B[[3]] <- c(2.5,1,0,5,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="o" | c=="O") {
    
    B[[1]] <- c(4,1,-4,0,0,-1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,6,4,0,0,1)
    B[[3]] <- c(1,1,0,5,-1,0)
    B[[4]] <- c(3,6,0,-5,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="u" | c=="U") {
    
    B[[1]] <- c(0,7,0,-3,1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,4,0,-3,1,0)
    B[[3]] <- c(0,0,4,0,0,1)
    B[[4]] <- c(4,1,0,3,-1,0)
    B[[5]] <- c(4,4,0,3,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="b" | c=="B") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,6,3,0,0,1)
    B[[4]] <- c(0,3,3,0,0,1)
    B[[5]] <- c(1,0,3,0,0,1)
    B[[6]] <- c(3,6,-1,-2,1,0)
    B[[7]] <- c(3,4,0,-3,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="c" | c=="C") {
    
    B[[1]] <- c(1,0,3,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,0,0,3,-1,0)
    B[[3]] <- c(1,3,0,3,-1,0)
    B[[4]] <- c(0,6,4,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="d" | c=="D") {
    
    B[[1]] <- c(2,1,-2,0,0,-1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,1,0,5,-1,0)
    B[[3]] <- c(0,6,2,0,0,1)
    B[[4]] <- c(2,7,1,-2,1,0)
    B[[5]] <- c(3,5,0,-3,1,0)
    B[[6]] <- c(3,2,-1,-2,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="f" | c=="F") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,6,3,0,0,1)
    B[[4]] <- c(0,3,3,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="g" | c=="G") {
    
    B[[1]] <- c(2,3,2,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(3,3,0,-3,1,0)
    B[[3]] <- c(3,1,-3,0,0,-1)
    B[[4]] <- c(1,1,0,3,-1,0)
    B[[5]] <- c(1,4,0,3,-1,0)
    B[[6]] <- c(1,6,3,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="h" | c=="H") {
    
    B[[1]] <- c(1,0,0,4,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,3,3,0,0,1)
    B[[4]] <- c(3,7,0,-3,1,0)
    B[[5]] <- c(3,3,0,-3,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="j" | c=="J") {
    
    B[[1]] <- c(3,7,0,-3,1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(3,4,0,-3,1,0)
    B[[3]] <- c(4,1,-3,0,0,-1)
    B[[4]] <- c(1,0,0,3,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="k" | c=="K") {
    
    B[[1]] <- c(1,0,0,4,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,3,3,3,0,1)
    B[[4]] <- c(1,3,2,-3,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="l" | c=="L") {
    
    B[[1]] <- c(0,0,3.5,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,1,0,3,-1,0)
    B[[3]] <- c(1,4,0,3,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="m" | c=="M") {
    
    B[[1]] <- c(1,0,0,4,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,5.5,1,-2,0,1.5)
    B[[4]] <- c(2,3.5,1,2,0,1.5)
    B[[5]] <- c(3,7,0,-3,1,0)
    B[[6]] <- c(3,4,0,-4,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="n" | c=="N") {
    
    B[[1]] <- c(1,0,0,4,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,5,2,-5,0,2)
    B[[4]] <- c(4,0,0,3,-1,0)
    B[[5]] <- c(4,3,0,4,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="p" | c=="P") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,3,0,3,-1,0)
    B[[3]] <- c(0,6,3,0,0,1)
    B[[4]] <- c(3,7,0,-3,1,0)
    B[[5]] <- c(4,4,-3,0,0,-1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="q" | c=="Q") {
    
    B[[1]] <- c(1,1,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,6,3,0,0,1)
    B[[4]] <- c(3,6,0,-4,1,0)
    B[[5]] <- c(4,2,-3,0,0,-1)
    B[[6]] <- c(2,1,1,-1,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="r" | c=="R") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,3,0,3,-1,0)
    B[[3]] <- c(0,6,3,0,0,1)
    B[[4]] <- c(3,7,0,-3,1,0)
    B[[5]] <- c(4,4,-3,0,0,-1)
    B[[6]] <- c(1,3,2,-3,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="s" | c=="S") {
    
    B[[1]] <- c(0,0,3,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(4,0,0,4,-1,0)
    B[[3]] <- c(3,4,-3,0,0,-1)
    B[[4]] <- c(1,4,0,3,-1,0)
    B[[5]] <- c(1,6,3,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="t" | c=="T") {
    
    B[[1]] <- c(2.5,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(2.5,3,0,3,-1,0)
    B[[3]] <- c(0,6,4,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="v" | c=="V") {
    
    B[[1]] <- c(0,7,0,-3,1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,4,0.75,-3,1,0)
    B[[3]] <- c(0.875,0,2.25,0,0,1)
    B[[4]] <- c(3.25,1,0.75,3,-1,0)
    B[[5]] <- c(4,4,0,3,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="w" | c=="W") {
    
    B[[1]] <- c(0,7,0.25,-3.5,0.5,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0.25,3.5,0.25,-3.5,0.5,0)
    B[[3]] <- c(1.5,0,0.5,3,-0.5,0)
    B[[4]] <- c(2,3,0.5,-3,0.5,0)
    B[[5]] <- c(3.5,0,0.25,3.5,-0.5,0)
    B[[6]] <- c(3.75,3.5,0.25,3.5,-0.5,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="x" | c=="X") {
    
    B[[1]] <- c(1,0,1,3.5,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,7,1,-3.5,1,0)
    B[[3]] <- c(3,7,-1,-3.5,1,0)
    B[[4]] <- c(4,0,-1,3.5,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="y" | c=="Y") {
    
    B[[1]] <- c(0,7,1,-4,1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(3,3,1,4,-1,0)
    B[[3]] <- c(2.7,0,0,3,-1.4,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="z" | c=="Z") {
    
    B[[1]] <- c(0,0,4,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,6,4,0,0,1)
    B[[3]] <- c(3,6,-3,-5,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="") {
    
    B[[1]] <- c() #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c()
    B[[3]] <- c()
    B[[4]] <- c()
    B[[5]] <- c()
    B[[6]] <- c()
    B[[7]] <- c()
    
    do.call(rbind,B)
    
  }
  
  else if (c=="") {
    
    B[[1]] <- c() #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c()
    B[[3]] <- c()
    B[[4]] <- c()
    B[[5]] <- c()
    B[[6]] <- c()
    B[[7]] <- c()
    
    do.call(rbind,B)
    
  }
  
  else if (c=="") {
    
    B[[1]] <- c() #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c()
    B[[3]] <- c()
    B[[4]] <- c()
    B[[5]] <- c()
    B[[6]] <- c()
    B[[7]] <- c()
    
    do.call(rbind,B)
    
  }
  
  else {
    
    B[[1]] <- c(0,0,0,0,0,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    do.call(rbind,B)
    
  }
  
}

textmatrix <- function (word) {
  
  #assembles the blocks for every character in a word
  
  word <- gsub("[[:digit:]]+","",word) #remove digits
  word <- gsub("[ ]{2,}"," ",word) #remove all spaces but the first in a row
  
  M=matrix(0,0,6) #empty matrix with 6 columns
  K <- 0 #accumulated kerring
  m <- 0 #accumulated rows
  
  if (word != "") {
  
    for (i in 1:nchar(word)){
    
      C <- charmatrix(substr(word, i, i))
      C <- C + 5*(i-1) * rep(1,dim(C)[1]) %*% matrix(c(1,0,0,0,0,0),1,6)
      M <- rbind(M,C)
      
      if (substr(word, i, i) == "l" | substr(word, i, i) == "L") {
        
        K <- K + 0.7
        M <- M + 0.7 * rep(1,dim(M)[1]) %*% matrix(c(1,0,0,0,0,0),1,6)
        
      }
      
      else if (substr(word, i, i) == "f" | substr(word, i, i) == "F") {
        
        K <- K + 0.4
        M <- M + 0.4 * rep(1,dim(M)[1]) %*% matrix(c(1,0,0,0,0,0),1,6)
        
      }
      
      else if (m>0 & (substr(word, i, i) == "j" | substr(word, i, i) == "J")) {
        
        K <- K + 0.3
        M[1:m,] <- M[1:m,] + 0.3 * rep(1,m) %*% matrix(c(1,0,0,0,0,0),1,6)
        
      }
      
      else if (substr(word, i, i) == "t" | substr(word, i, i) == "T") {
        
        K <- K + 0.2
        M <- M + 0.2 * rep(1,dim(M)[1]) %*% matrix(c(1,0,0,0,0,0),1,6)
        
        if (m>0){
          
          K <- K + 0.2
          M[1:m,] <- M[1:m,] + 0.2 * rep(1,m) %*% matrix(c(1,0,0,0,0,0),1,6)
          
          }
        
      }
      
      m <- dim(M)[1] #accumulated rows before the next character
      
    }
    
    #reajust to left margin by substracting the accumulated kerring
    M <- M - K * rep(1,dim(M)[1]) %*% matrix(c(1,0,0,0,0,0),1,6)
    
  }
  
  return(list(M,K))
  
}

blockarea <- function (block, D) abs(det(matrix(D[block,3:6],2,2)))

fractaltext <- function(word, dots, iter) {
  
  word <- gsub("[[:digit:]]+","",word) #remove digits
  word <- gsub("[ ]{2,}"," ",word) #remove all spaces but the first in a row
  
  if (word == "" | word == " ") {
    
    fractal <- as.data.frame(matrix(NA,0,2))
    names(fractal) <- c("x","y")
    return(fractal)
    
  }
  
  textlist <- textmatrix(word)
  D <- textlist[[1]] #textmatrix
  K <- textlist[[2]] #kerring
  
  nblock=dim(D)[1] #number of blocks
  
  a <- nchar(word)*(4+1)-1-K #character width = 4
  b <- 7 #character height =7
  
  S=matrix(c(1/a,0,0,1/b),2,2) #normalization
  W=matrix(c(a,0,0,b),2,2) #denormalization
  
  blockareas <- prop.table(apply(matrix(1:nblock) ,1 ,blockarea, D))
  blockndots <- blockareas*dots #number of dots sent to each block
  blockstartindex <- floor(cumsum(c(0,blockndots[1:nblock]))+1) #start index for each block
  blockstartindex[nblock+1] <- dots+1 #adjust
  
  P <- matrix(runif(2*dots),2,dots)
  
  for (i in 1:iter) {
    
    for (j in 1:nblock) {
    
      L <- matrix(D[j,3:6],2,2) #linear deformation matrix
      O <- matrix(D[j,1:2],2,1) #translation vector
      
      if (blockstartindex[j] != blockstartindex[j+1]) {
      
        P[,blockstartindex[j]:(blockstartindex[j+1]-1)] <- L %*% P[,blockstartindex[j]:(blockstartindex[j+1]-1)] + O %*% rep(1,blockstartindex[j+1]-blockstartindex[j])
    
      }
      
    }
    
    P <- S %*% P #normalization
    
    P <- P[,sample(1:dots, dots, replace = F)] #shuffle the dots
    
  }
  
  P <- W %*% P #denormalization
  
  
  fractal <- as.data.frame(t(P))
  names(fractal) <- c("x","y")
  
  return(fractal)
  
}

plotfractaltext <- function(word, dots=30000, iter=3, textcolor='coral3', backcolor='cornsilk2', dotsize=.5, computed=F) {
  
  #word can be: or a string of characters or the return of function 'fractaltext'.
  #In the last case, specify with the argument computed=TRUE
  #this is done in order to avoid recalculations when you just want to change the plot
  
  if (computed==F) fractal <- fractaltext(word, dots, iter) #compute and then plot
  else fractal <- word #then just plot:

  ggplot(fractal, aes(x=x, y=y)) +
    geom_point(size=dotsize,alpha=0.5, color=textcolor) + coord_fixed() +
    guides(fill=FALSE) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(panel.background = element_rect(fill = backcolor))+
    theme(plot.background  = element_rect(fill = backcolor))+
    theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank())

}

#compute and then plot
plotfractaltext("vosgeda",40000,4)

#compute
fractal <- fractaltext("vosgeda",40000,4)
#then just plot
plotfractaltext(fractal, computed=T)

