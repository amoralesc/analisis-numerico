#INTERPOLACION USANDO LAGRANGE

library(pracma)

#PUNTOS ORIGINALES                                                 
x=c(6.7,6.0,5.0,4.3,3.5,2.8,2.3,1.7,1.8,2.5,3.0,3.6,4.2,4.5,5.2,5.6,5.3,5.2,5.1,4.9,4.7,
    4.8,5.2,5.6,6.0,6.3,7.2,7.3,7.4,7.3,7.8,8.4,8.8,8.9,9.0,9.1,9.2,9.6,10.1,10.8,11.5,11.7,
    11.4,10.7,11.41,11.8,12.6,13.0,13.6,13.9,14.0,13.7,13.2,12.9,12.8,12.7,12.5,12.2,12.1,

    12.0,11.9,11.8,11.6,11.5)

y=c(3.0,3.6,4.9,6.3,8.0,8.6,9.7,10.7,11.3,11.5,11.2,10.7,10.0,9.6,8.6,9.6,11.0,12.2,13.3,
    14.0,15.2,16.2,16.6,16.5,15.5,14.0,11.7,13.8,14.7,16.0,17.4,17.6,17.3,16.3,15.4,14.0,
    12.3,14.5,16.1,16.9,16.5,15.7,14.4,11.2,12.0,12.4,13.5,14.2,14.4,14.1,13.7,13.0,12.2,
    11.6,11.2,10.9,10.5,9.5,8.3,7.6,6.7,6.0,5.0,4.5)

#GRAFICAR LA MANO ORIGINAL
plot(x,y,pch=19,cex=1,col="blue",xlim=c(0,25),type="b")

#FUNCION INTERPOLATORIA DE LAGRANGE
lagrange <- function(x,y,a){ 
  n = length(x) 
  if(a < min(x) || max(x) < a) stop("No estÃ¡ interpolando")
  X = matrix(rep(x, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(y*Lnk)
}

#FUNCION PARA CALCULAR EL PUNTO A EVALUAR(A)
punto<-function(x){
  
  return((x[length(x)]))
  
}
puntoa<-function(x){
  
  suma<-0
  for(i in 1:length(x)){
    
    suma<- suma + x[i]
    
  }
  
  a<-suma/length(x)
  return(a)
}

#INTERPOLACION DE PUNTOS (a= punto medio del intervalo)
f<-lagrange(x[1:2],y[1:2],punto(x[1:2])) 
f1<-lagrange(x[2:3],y[2:3],punto(x[2:3]))
f2<-lagrange(x[3:5],y[3:5],punto(x[3:5]))
f3<-lagrange(x[5:6],y[5:6],punto(x[5:6]))
f4<-lagrange(x[6:7],y[6:7],punto(x[6:7]))
f5<-lagrange(x[7:8],y[7:8],punto(x[7:8]))
f6<-lagrange(x[8:10],y[8:10],punto(x[8:10]))
f7<-lagrange(x[10:11],y[10:11],punto(x[10:11]))
f8<-lagrange(x[11:13],y[11:13],punto(x[11:13]))
f9<-lagrange(x[13:15],y[13:15],punto(x[13:15]))
f10<-lagrange(x[15:16],y[15:16],punto(x[15:16]))
f11<-lagrange(x[16:17],y[16:17],punto(x[16:17]))
f12<-lagrange(x[17:18],y[17:18],punto(x[17:18]))
f13<-lagrange(x[18:22],y[18:22],punto(x[18:22]))
f14<-lagrange(x[22:23],y[22:23],punto(x[22:23]))
f15<-lagrange(x[23:24],y[23:24],punto(x[23:24]))
f16<-lagrange(x[24:26],y[24:26],punto(x[24:26]))
f17<-lagrange(x[26:27],y[26:27],punto(x[26:27]))
f18<-lagrange(x[27:28],y[27:28],punto(x[27:28]))
f19<-lagrange(x[28:29],y[28:29],punto(x[28:29]))
f20<-lagrange(x[29:31],y[29:31],punto(x[29:31]))
f21<-lagrange(x[31:32],y[31:32],punto(x[31:32]))
f22<-lagrange(x[32:33],y[32:33],punto(x[32:33]))
f23<-lagrange(x[33:37],y[33:37],punto(x[33:37]))
f24<-lagrange(x[37:39],y[37:39],punto(x[37:39]))
f25<-lagrange(x[39:40],y[39:40],punto(x[39:40]))
f26<-lagrange(x[40:41],y[40:41],punto(x[40:41]))
f27<-lagrange(x[41:42],y[41:42],punto(x[41:42]))
f28<-lagrange(x[42:44],y[42:44],punto(x[42:44]))
f29<-lagrange(x[44:46],y[44:46],punto(x[44:46]))
f30<-lagrange(x[46:48],y[46:48],punto(x[46:48]))
f31<-lagrange(x[48:49],y[48:49],punto(x[48:49]))
f32<-lagrange(x[49:50],y[49:50],punto(x[49:50]))
f33<-lagrange(x[50:51],y[50:51],punto(x[50:51]))
f34<-lagrange(x[51:52],y[51:52],punto(x[51:52]))
f35<-lagrange(x[52:54],y[52:54],punto(x[52:54]))
f36<-lagrange(x[54:57],y[54:57],punto(x[54:57]))
f37<-lagrange(x[57:58],y[57:58],punto(x[57:58]))
f38<-lagrange(x[58:62],y[58:62],punto(x[58:62]))
f39<-lagrange(x[62:64],y[62:64],punto(x[62:64]))



#PUNTOS X,Y REDUCIDOS E INTERPOLADOS
x1<-c(punto(x[1:2]),punto(x[2:3]),punto(x[3:5]),punto(x[5:6]),punto(x[6:7]),punto(x[7:8]),
      punto(x[8:10]),punto(x[10:11]),punto(x[11:13]),punto(x[13:15]),
      punto(x[15:16]),punto(x[16:17]),punto(x[17:18]),punto(x[18:22]),punto(x[22:23]),
      punto(x[23:24]),punto(x[24:26]),punto(x[26:27]),punto(x[27:28]),punto(x[28:29]),
      punto(x[29:31]),punto(x[31:32]),punto(x[32:33]),punto(x[33:37]),punto(x[37:39]),
      punto(x[39:40]),punto(x[40:41]),punto(x[41:42]),punto(x[42:44]),punto(x[44:46]),
      punto(x[46:48]),punto(x[48:49]),punto(x[49:50]),punto(x[50:51]),punto(x[51:52]),
      punto(x[52:54]),punto(x[54:57]),punto(x[57:58]),punto(x[58:62]),punto(x[62:64]))

y1<-c(f,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,
      f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,
      f30,f31,f32,f33,f34,f35,f36,f37,f38,f39)

#GRAFICAR MANO ORIGINAL Y MANO INTERPOLADA
plot(x,y, pch=19, cex=1, col = "red", type='o',asp=1,xlab="X", ylab="Y", main="Interpolación de la Mano")
#plot(x1,y1, pch=19, cex=1, col = "blue", type='b',asp=1,xlab="X", ylab="Y", main="Interpolación de la Mano")

points(x1,y1, pch=19, cex=1, col = "blue", asp=1,type='o')


