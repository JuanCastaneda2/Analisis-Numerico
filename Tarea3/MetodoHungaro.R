gaussPP = function(A){
  if(is.matrix(A)) {
    n = nrow(A); m = ncol(A)
    if (m != n) stop("'A' debe ser una matriz cuadrada.")
  }

  
  for (k in 1:(n)){
    if(k <= n ){
      columna = which.min( A[k, ] );
      cat (k, " , ",columna, "\n");
      A[k,]= A[k,] - A[k,columna]
    }
  }
  for (k in 1:(n)){
    if(k <= n){
      fila = which.min( A[ ,k] );
      cat (fila, " , ",k, "\n");
      A[ , k]= A[ ,k] - A[fila,k]
    }
  }
  print(A)
  S = A; 
  k=1;
  cont =0;
  contl = 1;
  l <- list(0)
  pos <- which(S == 0 , arr.ind=TRUE)
  repeat {
    pos <- which(S == 0 , arr.ind=TRUE)
  if (k > nrow(A)){
      break;
  }else{
    nf <-sum(pos[,1] %in% k)
    nc <-sum(pos[,2] %in% k)
    if (nf>1){
      
      l [[contl]] <- c(k,0)
      contl = contl+1
      S[k,] <- -1
      print(S)
      cont = cont+1
      cat ("Removi la fila ", k, "\n")
      
      k = 0
    }else if (nc>1){
      
      l [[contl]] <- c(0,k)
      contl = contl+1
      S[,k] <- -1
      cat ("Removi la columna ", k, "\n")
      cont = cont+1
      k = 0
    }
    if (dim(pos)[1] == 0){
      break;
  }
    k=k+1;
  }
  }
  inte<-0;  
  repeat{
      pos <- which(S == 0 , arr.ind=TRUE)
      if (dim(pos)[1] == 0){
        break;
       } else{
          l [[contl]] <- S[pos[1],]
          contl = contl+1
          S[pos[1],] <- c(pos[1],0)
          cat ("Removi la filax ", pos[1], "\n")
          cont = cont+1
          }
    }
   cat ("Num " , cont, "\n")
   
   if (nrow(A)==cont){
     print ("Ya")
     
   }else{
     
     vec = c(S)
     vec = vec[!vec %in% -1]
     minn = which.min(vec);
     S = S - vec[minn];
     print (S)
     for (z in 1:(length(l))){
      
       for (s in 2:(length(l))){
         if (l[[z]][1] == 0 && l[[s]][2] == 0 ){
           x <- c(l[[z]][2],l[[s]][1])
           inte <- c(x)
         }else if (l[[z]][2] == 0 && l[[s]][1] == 0){
           x <- c(l[[z]][1],l[[s]][2])
           inte <- c(x)
         }
         
       }
       #inte <-  c(intersect(v1,v2))
     }
     #inte<- inte[!inte %in% 0]
     cat ("Hola ",inte)
     
     for (z in 1:length(l)){
       if (l[[z]][1] == 0){
         v1 = A[,l[[z]][2]]
         for (e in 1:length(v1)){
           if (v1[e] %in% inte){
             v1[e]=v1[e] + vec[minn];
           }
         }
         S [,l[[z]][2]] = v1
       }else{
         v1 = A[l[[z]][1],]
         for (e in 1:length(v1)){
           if (v1[e] %in% inte){
             v1[e]=v1[e] + vec[minn];
           }
         }
         S [l[[z]][1],] = v1
       }
     }
     
     
     
   }
 print (S)
       
}

#--- Pruebas
A = matrix(c( exp(1), pi, 
              1/3, -1/5)
           , nrow=2, byrow=TRUE)

#gaussPP(A) 


B = matrix(c( 10, 9, 5, 
              9, 8, 3, 
              6, 4, 7), nrow=3,ncol=3, byrow=TRUE)
gaussPP(B) 

#------------

