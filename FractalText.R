library(ggplot2)

textmatrix <- function (word) {
  
  #assembles the blocks for every character in a word
  
  M=matrix(0,0,6) #empty matrix with 6 columns
  
  for (i in 1:nchar(word)){
    
    C <- charmatrix(substr(word, i, i))
    C <- C + 5*(i-1) * rep(1,dim(C)[1]) %*% matrix(c(1,0,0,0,0,0),1,6)
    M <- rbind(M,C)
    
  }
  
  return(M)
  
}

fractaltext <- function(word, dots, iter) {
  
  a <- nchar(word)*(4+1)-1 #character width = 4
  b <- 7 #character height =7
  
  S=matrix(c(1/a,0,0,1/b),2,2) #normalization
  W=matrix(c(a,0,0,b),2,2) #denormalization
  
  D <- textmatrix(word)
  nblock=dim(D)[1] #number of blocks
  
  blockareas <- prop.table(apply(matrix(1:nblock) ,1 ,blockarea, D))
  
  fractal <- matrix(NA,dots,2)
  
  
  for (d in 1:dots){
    
    p <- matrix(runif(2),2,1)
    
    for (i in 1:iter){
      
      block <- sample(1:nblock, 1, replace = T, prob = blockareas) 
      
      L <- matrix(D[block,3:6],2,2) #linear deformation matrix
      O <- matrix(D[block,1:2],2,1) #translation vector
      
      p <- L %*% p + O
      
      p <- S %*% p #normalization
      
    }
    
    p <- W %*% p #denormalization
    
    fractal[d,] <- p
    
    
  }
  
  fractal <- as.data.frame(fractal)
  names(fractal) <- c("x","y")
  return(fractal)
  
}

blockarea <- function (block, D) abs(det(matrix(D[block,3:6],2,2)))

charmatrix <- function (c) {
  
  #computes the blocks of a character
  
  B=list()
  
  
  if (c==" ") {
    
    B[[1]] <- c(0,0,0,0,0,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    do.call(rbind,B)
    
  }
  
  else if (c=="a") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,3,0,3,-1,0)
    B[[3]] <- c(0,6,3,0,0,1)
    B[[4]] <- c(1,3,3,0,0,1)
    B[[5]] <- c(3,7,0,-3,1,0)
    B[[6]] <- c(3,3,0,-3,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="e") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(0,3,3,0,0,1)
    B[[4]] <- c(1,6,3,0,0,1)
    B[[5]] <- c(1,0,3,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="i") {
    
    B[[1]] <- c(0,0,4,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,6,4,0,0,1)
    B[[3]] <- c(2.5,1,0,5,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="o") {
    
    B[[1]] <- c(4,1,-4,0,0,-1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,6,4,0,0,1)
    B[[3]] <- c(1,1,0,5,-1,0)
    B[[4]] <- c(3,6,0,-5,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="u") {
    
    B[[1]] <- c(0,7,0,-3,1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,4,0,-3,1,0)
    B[[3]] <- c(0,0,4,0,0,1)
    B[[4]] <- c(4,1,0,3,-1,0)
    B[[5]] <- c(4,4,0,3,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="b") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,6,3,0,0,1)
    B[[4]] <- c(0,3,3,0,0,1)
    B[[5]] <- c(1,0,3,0,0,1)
    B[[6]] <- c(3,6,-1,-2,1,0)
    B[[7]] <- c(3,3,1,-2,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="c") {
    
    B[[1]] <- c(1,0,3,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,0,0,3,-1,0)
    B[[3]] <- c(1,3,0,3,-1,0)
    B[[4]] <- c(0,6,4,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="d") {
    
    B[[1]] <- c(2,1,-2,0,0,-1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,1,0,5,-1,0)
    B[[3]] <- c(0,6,2,0,0,1)
    B[[4]] <- c(2,7,1,-2,1,0)
    B[[5]] <- c(3,5,0,-3,1,0)
    B[[6]] <- c(3,2,-1,-2,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="f") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,6,3,0,0,1)
    B[[4]] <- c(0,3,3,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="g") {
    
    B[[1]] <- c(2,3,2,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(3,3,0,-3,1,0)
    B[[3]] <- c(3,1,-3,0,0,-1)
    B[[4]] <- c(1,1,0,3,-1,0)
    B[[5]] <- c(1,4,0,3,-1,0)
    B[[6]] <- c(1,6,3,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="h") {
    
    B[[1]] <- c(1,0,0,4,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,3,3,0,0,1)
    B[[4]] <- c(3,7,0,-3,1,0)
    B[[5]] <- c(3,3,0,-3,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="j") {
    
    B[[1]] <- c(3,7,0,-3,1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(3,4,0,-3,1,0)
    B[[3]] <- c(4,1,-3,0,0,-1)
    B[[4]] <- c(1,0,0,3,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="k") {
    
    B[[1]] <- c(1,0,0,4,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,3,3,3,0,1)
    B[[4]] <- c(1,3,2,-3,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="l") {
    
    B[[1]] <- c(0,0,4,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,1,0,3,-1,0)
    B[[3]] <- c(1,4,0,3,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="m") {
    
    B[[1]] <- c(1,0,0,4,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,5.5,1,-2,0,1.5)
    B[[4]] <- c(2,3.5,1,2,0,1.5)
    B[[5]] <- c(3,7,0,-3,1,0)
    B[[6]] <- c(3,4,0,-4,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="n") {
    
    B[[1]] <- c(1,0,0,4,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,5,2,-5,0,2)
    B[[4]] <- c(4,0,0,3,-1,0)
    B[[5]] <- c(4,3,0,4,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="p") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,3,0,3,-1,0)
    B[[3]] <- c(0,6,3,0,0,1)
    B[[4]] <- c(3,7,0,-3,1,0)
    B[[5]] <- c(4,4,-3,0,0,-1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="q") {
    
    B[[1]] <- c(1,1,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,4,0,3,-1,0)
    B[[3]] <- c(1,6,3,0,0,1)
    B[[4]] <- c(3,6,0,-4,1,0)
    B[[5]] <- c(4,2,-3,0,0,-1)
    B[[6]] <- c(2,1,1,-1,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="r") {
    
    B[[1]] <- c(1,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(1,3,0,3,-1,0)
    B[[3]] <- c(0,6,3,0,0,1)
    B[[4]] <- c(3,7,0,-3,1,0)
    B[[5]] <- c(4,4,-3,0,0,-1)
    B[[6]] <- c(1,3,2,-3,1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="s") {
    
    B[[1]] <- c(0,0,3,0,0,1) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(4,0,0,4,-1,0)
    B[[3]] <- c(3,4,-3,0,0,-1)
    B[[4]] <- c(1,4,0,3,-1,0)
    B[[5]] <- c(1,6,3,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="t") {
    
    B[[1]] <- c(2.5,0,0,3,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(2.5,3,0,3,-1,0)
    B[[3]] <- c(0,6,4,0,0,1)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="v") {
    
    B[[1]] <- c(0,7,0,-3,1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,4,0.75,-3,1,0)
    B[[3]] <- c(0.875,0,2.25,0,0,1)
    B[[4]] <- c(3.25,1,0.75,3,-1,0)
    B[[5]] <- c(4,4,0,3,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="w") {
    
    B[[1]] <- c(0,7,0.25,-3.5,0.5,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0.25,3.5,0.25,-3.5,0.5,0)
    B[[3]] <- c(1.5,0,0.5,3,-0.5,0)
    B[[4]] <- c(2,3,0.5,-3,0.5,0)
    B[[5]] <- c(3.5,0,0.25,3.5,-0.5,0)
    B[[6]] <- c(3.75,3.5,0.25,3.5,-0.5,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="x") {
    
    B[[1]] <- c(1,0,1,3.5,-1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(0,7,1,-3.5,1,0)
    B[[3]] <- c(3,7,-1,-3.5,1,0)
    B[[4]] <- c(4,0,-1,3.5,-1,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="y") {
    
    B[[1]] <- c(0,7,1,-4,1,0) #coodinates for (origin x, origin y, L11, L21, L21, L22)
    B[[2]] <- c(3,3,1,4,-1,0)
    B[[3]] <- c(2.7,0,0,3,-1.4,0)
    
    do.call(rbind,B)
    
  }
  
  else if (c=="z") {
    
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

plotfractaltext <- function(word="", dots=30000, iter=3, textcolor='coral3', backcolor='cornsilk2', dotsize=.01) {

  fractal <- fractaltext(word, dots, iter)

  ggplot(fractal, aes(x=x, y=y)) +
    geom_point(size=dotsize,alpha=0.4, color=textcolor) + coord_fixed() +
    guides(fill=FALSE) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(panel.background = element_rect(fill = backcolor))+
    theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank())

}


plotfractaltext("fractal",40000,4)
