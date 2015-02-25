 ! Volumen.f90 : Calcula el volumen del agua que se encuentra en un tanque esferico a una altura h del suelo
 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

 Program Volumen_esfera ! comenzar pograma

   Implicit None ! Declaracion de variables

   Real  :: radio , volumen , altura   ! Declarar reales

   Real,PARAMETER  :: PI = 4.0 * atan(1.0) ! Declar constaste

   Integer :: model_n = 2 ! Declarar entero con valor dado

   
   !Pedir datos
  
   print*, "Escribe el radio de la esfera (mayor o igual que cero):" ! hablar al usuario
   read*, radio
   print*, "Escribe la altura a la que esta el agua desde el piso o fondo del tanque esferico"   
   read*, altura

   IF (altura <= (2.0*radio)) THEN
        volumen = (PI/3.0)*(altura*altura)*((3.0*radio)-altura) ! calcular el volumen del agua en el tanque
   ELSE
       print*, "Verifica que los datos sean consistentes"
        STOP
   END IF
  


  print*, "Numero de programa =" , model_n ! Print program number
  print*, "El radio del tanque esferico es" , radio !radio
  print*, "La altura del agua respecto al fondo del tanque es" , altura !altura
  print*, "El volumen que se encuentra en el tanque esferico a esa altura es =" , volumen , "unidades cubicas" ! Imprimir el volumen

 End Program Volumen_esfera ! Terminar programa
