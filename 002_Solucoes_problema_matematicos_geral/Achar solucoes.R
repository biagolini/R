# https://stackoverflow.com/questions/64198151/using-formulas-in-r
# Tutorial para função uniroot

# Parte 1. Informar a relação que se igual a zero
f <- function(x,radians,y) ((y/sqrt(x^2+y^2))-sin(radians)) 
# Parte 2. Criar uma função  que calcula qual seria o valor de y dado valores de x 

FindY <- function(x, radians, interval) {
  fy <- function(y) f(x, radians, y)
  uniroot(fy, interval)$root}

# Parte 3. Aplicar função. OBS angulo em radiano=(graus * pi) / (180) 

FindY(x=2, radians=pi/4, interval=c(-10000,10000))