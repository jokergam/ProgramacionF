 ! Area . f90 : Calcula el area de un circulo

 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

 Program Circle_area ! Comenzar programa

   Implicit None ! Declaracion de variables

   Real  :: radius , circum , area ! Declarar reales

   Real,PARAMETER  :: PI = 4.0 * atan(1.0) ! Declarar constante real

   Integer :: model_n = 1 ! Declare , assign Ints

   print*, "Escribe el radio del circulo:" ! hablar al usuario

   read*, radius ! leer radio

  circum = 2.0*PI*radius ! calcular circunferencia

  area = radius*radius*PI ! calcular

  print*, "Numero de programa =" , model_n ! Print program number

  print*, "Radio =" , radius ! imprimir radio

  print*, "El perimetro es =" , circum , "unidades" ! imprimir perimetro

  print*, "Area =" , area , "unidades cuadradas" ! imrpimir area
 End Program Circle_area ! Terminar programa
