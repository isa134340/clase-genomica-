#Calcula la distancia de Hamming entre las personas  tú y quién quieras. ¿Cuántas diferencias hay en sus respuestas?
#a=mi distancia 
#b=distancia compañer@

#yo y julie 

x<- 1
while (x<23 ){ 
  if ( 1>0) { 
    sum((gustosG[11,2:14]) !=  gustosG[x,2:14]) -> upp
    print(upp)
    x<-x+1
    }
  }

#distancia de 3
#persona más similar conmigo JIMENA 
#Persona más diferente ANDRÉS 

#FUNCIONES
coef_binomial <- function(m, n) {
  factorial(n) / (factorial(m) * factorial(n - m))
}

binomial <- function(m, n, p) {
  coef_binomial(m, n) * (p^m) * ((1 - p)^(n - m))
}


#Supongamos que la probabilidad de que una persona disfrute de Kpop en la base de datos es p.
#Si seleccionamos  al azar 5 personas, ¿cuál es la probabilidad de que 3 sean fans de Kpop?

KPOP<- binomial( m= 3, n= 5, p= 0.27)

print(KPOP)


#Si seleccionamos  al azar, ¿cuál es la probabilidad de que  de ellas sea fan de ANIME ?

ANIM<- binomial(m=6, n=22, p= 0.54) 

print (ANIM)


#En nuestra base de datos, calculamos la probabilidad de consumir Chocolate (p).
#Si seleccionamos 10 personas, ¿cuál es la probabilidad de que  6 de ellas consuman Chocolate? 

CHOC<- binomial(m=6, n=10, p= 0.86)
print(CHOC)

#Si sabemos que la probabilidad de que una persona haga Jogging  y Nadar  está dada por la base de datos, y seleccionamos , ¿cuál es la probabilidad de que exactamente  de ellas realicen ambas actividades?
JJOGING<- binomial (m= 1, n= 22, p= 0.36)
print (JJOGING)
NADAR<- binomial (m=1, n=22, p=0.72)

print (NADAR)

#AMBOS:0.0006737591*3.891113e-11 =  2.621673e-14

#Seleccionamos  al azar 8 personas. ¿Cuál es la probabilidad de que   5 sean fanáticos de Rom-Coms ?



RCOMS <- binomial (m= 5, n=8, p =0.45 )

print(RCOMS)

# Si seleccionamos  al azar, 7 personas ¿cuál es la probabilidad de que  al menos una de ellas conduzca un auto?

AUTO<- binomial (m=1, n=7, p=0.40 )
print(AUTO)

#Si seleccionamos  al azar 5 personas , ¿cuál es la probabilidad de que  exactamente 3 consuman Alcohol?
ALCOH<- binomial (m=3, n= 5, p= 0.81)

print(ALCOH)
#Si seleccionamos  al azar 6 personas, ¿cuál es la probabilidad de que al menso 3 sean fans de los cómics ?
COMIC<- binomial(m=3, n=6, p= 0.45)
print(COMIC)

#Si seleccionamos  4 personas al azar, ¿cuál es la probabilidad de que  al menos 3. de ellas disfruten de bailar?
#significado de variables 
BAILE<- binomial(m=3, n=4, p= 0.68)
print(BAILE)

#m: número de éxitos.

#n: número de ensayos.

#p: probabilidad de éxito en un ensayo.

