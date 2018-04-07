#include "helloWorld.h"

void main_hello(){
  //write( STDOUT_FILENO, " Prefork ", 9);

  int f = fork();

  int id = getpid();





  // for (int i = 0; i < 16; i++){
  //   if (f == 0) {
  //     break;
  //   } else {
  //     f = fork();
  //   }
  // }


   write( STDOUT_FILENO, " PID: ", 7);
   int num1 = f;
   char snum1[5];

  // convert 123 to string [buf]
   itoa(snum1, id);
  //
  // write(STDOUT_FILENO, "SPS: ", 5);
   write(STDOUT_FILENO, snum1, 5);
   write(STDOUT_FILENO, " ", 1);

  exit( EXIT_SUCCESS );

}
