library(pracma)

A=matrix(c(4,-1,-1,-1,
           -1,4,-1,-1,
           -1,-1,4,-1,
           -1,-1,-1,4),nrow=4,byrow=4)

Ap=matrix(c(4,-1,-1,-1,
            -1.15,4,-1,-1,
            -1,-1,4,-1,
            -1,-1,-1,4),nrow=4,byrow=4)

b=c(-exp(1),5,6,0)

punto = function(A,Ap) {
  C = Ap-A  ## Calulamos el error en la matriz original
  cat ("Error en A\n\n")
  print(C)
  cat ("\n\n")
  SA = solve(A,b)    # Resolvemos la matriz para la matriz original
  SAp = solve (Ap,b) # Resolvemos la matriz para la matriz modificada
  C2 = SAp - SA      # Calculamos el error en las soluciones
  cat ("Error en Soluciones\n\n")
  print(C2)
  cat ("\n\n")
  Ea=(norm(C, type = "I"))/(norm(A,type ="I")) #Calculamos el error relativo de la matriz 
  Esol = (max(C2)) / (max(SAp))    #Calculamos el error relativo de las soluciones
  cond = norm(A, type = "I")* (norm( inv(A), type = "I")) #Calculamos el numero de 
  #condicion para la matriz
  cota = cond *Ea    #Calculamos la cota de error
  cat("La cota de error es: ",cota*100,"% \n\n")
  cat(round(Ea*100,digits = 2),"% de distorcion en la matriz produjo una distorcion de  ", 
      round(Esol*100,digits = 2) ,"% en la solución \n")
}
punto(A,Ap)
