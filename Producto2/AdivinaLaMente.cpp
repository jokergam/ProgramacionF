#include <iostream>
#include <unistd.h>
using namespace std;

int main()
{
   cout << "Hola! Tratare de adivinar un numero. Piensa en un numero entre 1 y 10" << endl; 
   sleep(5);
   cout << "Ahora multiplicalo por 9" << endl;
   sleep(5);
   cout << "Si el numero tiene 2 digitos, sumalos entre si. Si tu numero tiene un solo digito, sumale 0" << endl;
   sleep(5);
   cout << "Al numero restante sumale 4" << endl;
   sleep(10);
   cout << "Muy bien. El resultado es 13 ;D. Siguele rockeando con todo, animo." << endl;
   return 0;
}

