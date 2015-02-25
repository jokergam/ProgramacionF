 ! Subroutine . f90 : Muestra como se llama una subrutina
 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

 Subroutine g(x, y, ans1 , ans2 )

   Implicit None

   Real (8) :: x , y , ans1 , ans2 ! Declarar variables

   ans1 = sin (x*y) + 1. ! Usar funcion instrinseca

   ans2 = ans1**2

 End Subroutine g

 !

 Program Main ! Demos the CALL

   Implicit None

   Real *8 :: Xin =0.25 , Yin =2.0 , Gout1 , Gout2
   Integer :: model_n = 8 ! Declare , assign Ints
   call g( Xin , Yin , Gout1 , Gout2 ) ! Llamar la subrutina
   
    print*, "Numero de programa =" , model_n !Print program number

   write ( * , *) "Las respuestas son: " , Gout1 , Gout2

 End Program Main
