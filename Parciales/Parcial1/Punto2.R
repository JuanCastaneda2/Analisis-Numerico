f <- function(x){
  tan(pi*x)-sin(pi*x)
}

parteA=function(x0){
  error=1000; #Inicialisamos el error para que pueda entrar al ciclo
  
  x0=x0-0.1; #-|-->Se varian x1 y x2 en un pequeño valor para poder empezar a hacer el 
  x2=x0+1;   #-|   el metodo, se disminuye x0 en 1 para que siempre muestre la raiz a  
  x1=x0+0.5; #-|   la derecha del numero dado
  
  cont=0;
  while( error > 10e-9 && cont <= 100 ){
    x0 = x1 - ( (f(x1) * (x1-x2)) / ((f(x1)-f(x2))) )#Definimos la formula iterativa 
    #dada en el parcial
    
    error = abs(x0-x1) #Calculamos el error para ver si hay mas itercaiones
    x2=x1 #Actualizamos
    x1=x0 #Actualizamos
    cont=cont+1
  }
  if (cont < 100)
    cat( "Raiz ",x0, " Iteraciones = ",cont, "\n")
  else
    cat ("No converge\n")
}
cat ("Metodo Iterativo\n")
se=seq(0,5,length=500)
plot(se,f(se),type="l",col="blue",lwd=3)
parteA(0)
parteA(2.2)