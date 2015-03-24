!************************************************  
  !This program plots projectile motion of an object.  
  !The program requires user input for initial velocity   
  !and angle of the object.The algorithm uses a time   
  !step of 0.01 second i.e. it calculates object's  
  !location in the x and y plane every 0.01 second.  
  !**********By: Waleed Ishaque, 2013**************  
  program projectile_plot  
       implicit none  
       !Defining constants:  
       real, parameter :: pi = 4.0*atan(1.0) 
       real :: u, a, t, a_grados, my, mx, ft, Vx, Vy, FA  
       real, parameter :: g = 9.81  
       real:: x(150),y(150)  
          integer :: i 

       !where g is gravity, pi is "pi"   
       !u is object's initial velocity   
       !a is object's initial angle (grades)
       !t is time during the simulation   
       !x and y are arrays with 150 rows   
       !Seek user input   
       write(*,*) 'Ingresa el angulo de lanzamiento (Real)'   
       read *, a_grados   
       write(*,*) 'Ingresa la velocidad inical del proyectil en m/s (Real)'   
       read *, u   
       !Convertir angulo a radianes  
       a = (a_grados*pi)/180.0   
       !Calcular componentes de la velocidad en x (Vx) e y (Vy)
       IF (a_grados == 90.0) THEN
       Vx=0.0
       Vy=u
       ELSE IF (a_grados == 0.0) THEN
       Vx=0.0
       Vy=0.0
       ELSE
       Vx=u*cos(a)
       Vy=u*sin(a)
       END IF
        
   !Calcular el tiempo que el objeto esta en el aire, siendo el mismo
   !en las componentes x e y
   ft=(2.0*Vy)/g
 
  
   !calcular altura maxima
   my=(Vy**2)/19.62
   
   !Print results 
   print*, "Velocidad inicial:", u,"m/s"
   print*, "Angulo de lanzamiento (grados):", a_grados
   print*, "Tiempo total de vuelo:", ft, "s"
   print*, "La altura maxima es:", my,"m"
   
    
       !open .dat file and start writing on it using the algorithm   
       open(1, file='proj.dat')   
         
       
       do i=1,300 
            
            !displacement of object in x and y direction   
            t = (float(i)*0.1)   
            x(i) = Vx*t   
            y(i) = (Vy*t)-(4.905*t*t)   
            !write output in file "proj.dat" for plotting   
            write(1,*) x(i), y(i)   
            !kill the loop when the object hits the ground   
            IF (y(i)<0) exit  
       end do 
       close(1)   
       !close file 

   
  !Desplazamiento en la direccion x hasta que el objeto toca el suelo
   mx=x(i)
    print*, "El desplazamiento total en la direccion x es:", mx,"m"
  
   end program projectile_plot 
