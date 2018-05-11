#include "helloWorld.h"




int is_prime1( uint32_t x ) {
  if ( !( x & 1 ) || ( x < 2 ) ) {
    return ( x == 2 );
  }

  for( uint32_t d = 3; ( d * d ) <= x ; d += 2 ) {
    if( !( x % d ) ) {
      return 0;
    }
  }

  return 1;
}

void printing(int x){
  int num1 = x;
  char snum1[5];
  itoa(snum1, num1);
  write(STDOUT_FILENO, snum1, 5);
  write(STDOUT_FILENO, "\n", 1);
}
int reading(int pipe){
  int readCounter = -1;
  while (readCounter < 0){
    readCounter = pread(pipe);
  }
  return readCounter;
}
int writing(int pipe, int message){
  int writeCounter = 0;
  while(writeCounter == 0){
    writeCounter = pwrite(pipe, message, 0);
  }
  return writeCounter;
}

int pmod(int i, int n) {

    return (i % n + n) % n;
}


void thinking(int id){
  for( int i = 0; i < 10; i++ ) {
    write( STDOUT_FILENO, " P ", 3);
    char snum1[5];

    // convert 123 to string [buf]
    itoa(snum1, id);
    //
    // write(STDOUT_FILENO, "SPS: ", 5);
    write(STDOUT_FILENO, snum1, 5);
    write( STDOUT_FILENO, " thinking \n", 11);


    uint32_t lo = 1 <<  8;
    uint32_t hi = 1 << 16;

    for( uint32_t x = lo; x < hi; x++ ) {
      int r = is_prime1( x );
    }
  }
}

void eating(int id){
  for( int i = 0; i < 10; i++ ) {
    write( STDOUT_FILENO, " P ", 3);
    char snum1[5];

    // convert 123 to string [buf]
    itoa(snum1, id);
    //
    // write(STDOUT_FILENO, "SPS: ", 5);
    write(STDOUT_FILENO, snum1, 5);
    write( STDOUT_FILENO, " eating \n", 9);


    uint32_t lo = 1 <<  8;
    uint32_t hi = 1 <<16;

    for( uint32_t x = lo; x < hi; x++ ) {
      int r = is_prime1( x );
    }
  }
}

int children[16];


void waiter(){
  int chops[16];
  int toPhilosopher[16];
  int child[16];
  int n = 16;
  for (int i = 0; i < n; i++){
    child [i]= children[i];
    toPhilosopher[i] = pipe(child[i]);
  }
  for (int i = 0; i < n; i++){
    writing(toPhilosopher[i], child[(i+1)%n]);
  }
  for (int i = 0; i< n; i++){
    writing(toPhilosopher[i], child[pmod(i-1, n)]);
  }
  for (int i = 0; i < n; i++){
    chops[ i ] = semaph();
  }
  for (int i = 0; i < n; i++){
    writing(toPhilosopher[ i ], chops [ i ]);
  }
  for (int i = 0; i < n; i++){
    writing(toPhilosopher[ i ], chops [(i+1)%n]);
  }
  for (int i = 0; i < n; i++){
    writing(toPhilosopher[ i ], i);
  }


  while(1){

  }
}

void philosopher(){
  int n = 16;
  int fromWaiter = popen();
  int right = reading(fromWaiter);
  int left  = reading(fromWaiter);
  chop_t leftChop, rightChop;
  int philNumber;

  int toRight = pipe(right);
  int toLeft  = pipe(left);
  int fromRight = -1;
  int fromLeft = -1;

  while(fromLeft <= 0){
     fromLeft = popen();
  }
  while(fromRight <= 0){
     fromRight = popen();
  }

  leftChop.semaphore  = reading(fromWaiter);
  leftChop.status  = STATUS_DIRTY;
  rightChop.semaphore = reading(fromWaiter);
  rightChop.status  = STATUS_DIRTY;


  philNumber = reading(fromWaiter);

  // write( STDOUT_FILENO, "ID: ", 4);
  // printing(philNumber);
  // write( STDOUT_FILENO, "RIGHT CHOP: ", 12);
  // printing(rightChop.semaphore);
  // write( STDOUT_FILENO, "LEFT CHOP: ", 11);
  // printing(leftChop.semaphore);




  if(philNumber != n-1){
    lock(rightChop.semaphore);
  }
  if(philNumber == 0){
    lock(leftChop.semaphore);
  }
  if(philNumber == 0 || philNumber == n-1){
    int z = fromRight;
    fromRight = fromLeft;
    fromLeft = z;
  }
  // write( STDOUT_FILENO, "ID: ", 4);
  // printing(getpid());
  // write( STDOUT_FILENO, "R: ", 3);
  // printing(check(rightChop.semaphore));
  // write( STDOUT_FILENO, "L: ", 3);
  // printing(check(leftChop.semaphore));




  //
  // if(check(leftChop.semaphore) == getpid()){
  //   write( STDOUT_FILENO, "ID: ", 4);
  //   printing(philNumber);
  //   write( STDOUT_FILENO, "LEFT CHOP: ", 11);
  //   printing(leftChop.semaphore);
  // }
  // if(check(rightChop.semaphore) == getpid()){
  //   write( STDOUT_FILENO, "ID: ", 4);
  //   printing(philNumber);
  //   write( STDOUT_FILENO, "RIGHT CHOP: ", 12);
  //   printing(rightChop.semaphore);
  // }
  while(1){
    thinking(philNumber);
    bool gaveRight = false;
    bool gaveLeft  = false;
    bool NotTwoChopsticks = false;
    if(check(rightChop.semaphore) != getpid() || check(leftChop.semaphore) != getpid()){
      NotTwoChopsticks = true;
    }
    if(NotTwoChopsticks){
      while(1){
        pwrite(toRight,1,1);
        if(lock(rightChop.semaphore)){
          write( STDOUT_FILENO, "FROM RIGHT PH: ", 15);
          printing(philNumber);
          rightChop.status = STATUS_CLEAN;
         }
         if(check(rightChop.semaphore) == getpid() && check(leftChop.semaphore) == getpid()) break;

         pwrite(toLeft, 1,1);
         if(lock(leftChop.semaphore)){
           write( STDOUT_FILENO, "FROM LEFT PH: ", 14);
           printing(philNumber);
           leftChop.status = STATUS_CLEAN;
         }
         if(check(rightChop.semaphore) == getpid() && check(leftChop.semaphore) == getpid()) break;

         if(rightChop.status == STATUS_DIRTY){
           if(pread(fromRight) > -1){
             write( STDOUT_FILENO, "TO RIGHT PH: ", 13);
             printing(philNumber);
             sig(rightChop.semaphore);
             gaveRight = true;
             rightChop.status = STATUS_CLEAN;
           }
         }
         if(leftChop.status == STATUS_DIRTY){
            if(pread(fromLeft) > -1){
              write( STDOUT_FILENO, "TO LEFT PH: ", 12);
              printing(philNumber);
              sig(leftChop.semaphore);
              gaveLeft = true;
              leftChop.status = STATUS_CLEAN;
             }
           }
    }
  }

    if(check(rightChop.semaphore) == getpid() && check(leftChop.semaphore) == getpid()){
      eating(philNumber);
      rightChop.status = STATUS_DIRTY;
      leftChop.status  = STATUS_DIRTY;
    }

     while(1){
      sig(rightChop.semaphore);
      rightChop.status = STATUS_DIRTY;

      sig(leftChop.semaphore);
      leftChop.status  = STATUS_DIRTY;

      if(check(rightChop.semaphore) != -1 && check(leftChop.semaphore) != -1 ){
         write( STDOUT_FILENO, "HERE ", 5);
         printing(philNumber);
        break;
      }
    }
  }
}

void main_hello(){

  //write( STDOUT_FILENO, " Prefork ", 9);
  int id = getpid();
  int f = 2;
  int x = -1;

  if(f != 0) {
    for (int i = 0; i < 16; i++){
      if (f == 0) {
        break;
      } else {
        f = fork();
        x++;
        children[ i ] = f;
      }
    }
  }


  if(f == 0){
    philosopher();

  } else {
    waiter();
  }




  //  write( STDOUT_FILENO, " PID: ", 7);
  //  int num1 = f;
  //  char snum1[5];
  //
  // // convert 123 to string [buf]
  //  itoa(snum1, id);
  // //
  // // write(STDOUT_FILENO, "SPS: ", 16);
  //  write(STDOUT_FILENO, snum1, 5);
  //  write(STDOUT_FILENO, " ", 1);

  exit( EXIT_SUCCESS );

}
