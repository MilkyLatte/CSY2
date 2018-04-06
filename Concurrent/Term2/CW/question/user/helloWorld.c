#include "helloWorld.h"

void main_hello(){
  write( STDOUT_FILENO, " Prefork ", 9);
  fork();
  write( STDOUT_FILENO, " Hello ", 7);
  exit( EXIT_SUCCESS );
  write( STDOUT_FILENO, " pooop ", 7);

}
