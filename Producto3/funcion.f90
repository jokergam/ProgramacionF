 ! Function . f90 : Llama a una funcion definida por el usuario

 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

 Real *8 Function f (x,y)

   Implicit None

   Real *8 :: x, y

   f = 1.0 + sin (x*y )

 End Function f

 

 Program Main

  Implicit None

  Real *8 :: Xin =0.25 , Yin =2. , c , f ! declarations ( also f)
  Integer :: model_n = 7 ! Declare , assign Ints
  c = f ( Xin , Yin )
  print*, "Numero de programa =" , model_n ! Print program number
  write ( * , * ) "f(Xin, Yin) = " , c

 End Program Main 
