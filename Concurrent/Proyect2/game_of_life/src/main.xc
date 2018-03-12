// COMS20001 - Cellular Automaton Farm - Initial Code Skeleton
// (using the XMOS i2c accelerometer demo code)

#include <platform.h>
#include <xs1.h>
#include <stdio.h>
#include "pgmIO.h"
#include "i2c.h"

#define  IMHT 64           //image height
#define  IMWD 64                 //image width

typedef unsigned char uchar;      //using uchar as shorthand

on tile[0]: port p_scl = XS1_PORT_1E;         //interface ports to orientation
on tile[0]: port p_sda = XS1_PORT_1F;

#define FXOS8700EQ_I2C_ADDR 0x1E  //register addresses for orientation
#define FXOS8700EQ_XYZ_DATA_CFG_REG 0x0E
#define FXOS8700EQ_CTRL_REG_1 0x2A
#define FXOS8700EQ_DR_STATUS 0x0
#define FXOS8700EQ_OUT_X_MSB 0x1
#define FXOS8700EQ_OUT_X_LSB 0x2
#define FXOS8700EQ_OUT_Y_MSB 0x3
#define FXOS8700EQ_OUT_Y_LSB 0x4
#define FXOS8700EQ_OUT_Z_MSB 0x5
#define FXOS8700EQ_OUT_Z_LSB 0x6

char infname[] = "64x64.pgm";     //put your input image path here
char outfname[] = "testout.pgm"; //put your output image path here
/////////////////////////////////////////////////////////////////////////////////////////
//
// Read Image from PGM file from path infname[] to channel c_out
//
/////////////////////////////////////////////////////////////////////////////////////////
void DataInStream(char infname[], chanend c_out)
{
  int res;
  uchar line[ IMWD ];
  printf( "DataInStream: Start...\n" );

  //Open PGM file
  res = _openinpgm( infname, IMWD, IMHT );
  if( res ) {
    printf( "DataInStream: Error openening %s\n.", infname );
    return;
  }

  //Read image line-by-line and send byte by byte to channel c_out
  for( int y = 0; y < IMHT; y++ ) {
    _readinline( line, IMWD );
    for( int x = 0; x < IMWD; x++ ) {
      c_out <: line[ x ];
//      printf( "-%4.1d ", line[ x ] ); //show image values
    }
//    printf( "\n" );
  }

  //Close PGM image file
  _closeinpgm();
  printf( "DataInStream: Done...\n" );
  return;
}

/////////////////////////////////////////////////////////////////////////////////////////
//
// Start your implementation by changing this function to implement the game of life
// by farming out parts of the image to worker threads who implement it...
// Currently the function just inverts the image
//
/////////////////////////////////////////////////////////////////////////////////////////
int pmod(int i, int n) {
    return (i % n + n) % n;
}


void distributor(chanend c_in, chanend c_out, chanend fromAcc, chanend to_farm [4]){
  uchar val;
  int map [IMWD][IMHT];
  int total2 [IMWD][IMHT];

  //Starting up and wait for tilting of the xCore-200 Explorer
  printf( "ProcessImage: Start, size = %dx%d\n", IMHT, IMWD );
  printf( "Waiting for Board Tilt...\n" );
  fromAcc :> int value;

  //Read in and do something with your image values..
  //This just inverts every pixel, but you should
  //change the image according to the "Game of Life"
  printf( "Processing...\n" );
  for( int y = 0; y < IMHT; y++ ) {   //go through all lines
    for( int x = 0; x < IMWD; x++ ) { //go through each pixel per line
      c_in :> val;
      map[x][y] = val;//read the pixel value
 //     c_out <: (uchar)( val ^ 0xFF ); //send some modified pixel out
    }
  }
  int i = 0;
  while (i < 10){
  int j = 0;
  int l = 0;

  for (int i = 0; i < 4; i++) {
      int miniMap[IMWD/4][IMHT];
      l = j;
      for (int z = l; z < (l + IMWD/4); z++) {

          for (int h = 0; h < IMHT; h++) {
//              if (z >= 62 ) printf("Z value is %d\n", z);
              if (z <= (IMHT-1)) miniMap [z % (IMWD/4)][h] = map[z][h];

          }
          j = z+1;
      }
      for(int p = 0; p < IMWD/4; p++) {
          for (int q = 0; q < IMHT; q++){
              to_farm[i] <: miniMap [p][q];
          }
      }
  }



  int z = IMHT/4;

  for (int x = 0; x < z; x++) {
      for (int y = 0; y < IMHT; y++) {
          to_farm[0] :> total2[x][y];
      }
  }

  for (int x = z; x < (z*2); x++) {
        for (int y = 0; y < IMHT; y++) {
            to_farm[1] :> total2[x][y];
        }
    }

  for (int x = (z*2); x < (z*3); x++) {
        for (int y = 0; y < IMHT; y++) {
            to_farm[2] :> total2[x][y];
        }
    }

  for (int x = (z*3); x < (z*4); x++) {
        for (int y = 0; y < IMHT; y++) {
            to_farm[3] :> total2[x][y];
        }
    }

  for (int i = 0; i < IMHT; i++){
      for (int j = 0; j < IMHT; j++){
          map[i][j] = total2[i][j];
      }
  }
  i++;
}







  printf( "\nOne processing round completed...\n" );







//  int x = 0;

//  while (x < 2) {
//
//   for( int y = 0; y < IMHT; y++ ) {
//       for( int x = 0; x < IMWD; x++ ) {
//           map[x][y] = total2[x][y];
////         printf( "-%4.1d ", map[x][y]); //show image values
//       }
////       printf( "\n" );
//     }
////   printf("\n");
//   x ++;
//  }
  for( int y = 0; y < IMHT; y++ ) {   //go through all lines
    for( int x = 0; x < IMWD; x++ ) { //go through each pixel per line
      uchar val = total2[x][y];
      c_out <: val; //send some modified pixel out
    }
  }



}
void barn(int id, chanend fromfarmer, chanend toLeft, chanend toRight){
    int edges[2][IMHT];

    while(1){


    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < IMHT; j++) {
            fromfarmer :> edges[i][j];
        }
    }
    printf("Finished receiving\n");
    for (int i = 0; i < IMHT; i++) {
        toLeft <: edges[0][i];

    }

    for (int i = 0; i < IMHT; i++) {
        toRight <: edges[1][i];
    }

    printf("Finished sending\n");
    }
}


void farmer(int id, chanend f_d, chanend toBarn, chanend r, chanend l){
    int map [(IMWD/4 + 2)][IMHT];
    int total2 [IMWD/4] [IMHT];
    while (1){
    //printf("I'm here at 1\n");
    for(int i = 1; i < ((IMWD/4)+1); i++) {
        for (int j = 0; j < IMHT; j++){
            f_d :> map[i][j];
        }
    }


    //printf("I'm here at 1\n");

    int send_to_barn [2][IMHT];

    for (int i = 0; i < IMHT; i++){
        send_to_barn [0][i] = map [1][i];
        send_to_barn [1][i] = map [(IMWD/4)][i];
    }

    printf("%d\n", id);

    //printf("I'm here at 2\n");
    for (int i = 0; i < 2; i++) {
            for (int j = 0; j < IMHT; j++) {
                toBarn <: send_to_barn [i][j];
            }
    }
    printf("I'm here at 3\n");

    for (int i = 0; i < IMHT; i++) {
            r :> map [(IMWD/4)+1][i];

    }

    for (int i = 0; i < IMHT; i++) {
        l :> map [0] [i];
    }


    printf("I'm here at 4\n");

    for (int y = 0; y < IMHT; y++) {
          for(int x = 1; x < ((IMWD/4) + 1) ; x++) {
              int total = 0;
              if (map [x-1][y] != 0) total ++;
              if (map [x-1][pmod(y-1, IMWD)] != 0) total ++;
              if (map [x-1][pmod(y+1, IMWD)] != 0) total ++;
              if (map [x][pmod(y-1, IMWD)] != 0) total ++;
              if (map [x][pmod(y+1, IMWD)] != 0) total ++;
              if (map [x+1] [pmod(y-1, IMWD)] != 0) total++;
              if (map [x+1] [y] != 0) total++;
              if (map [x+1] [pmod(y+1, IMWD)] != 0) total++;


              if (total < 2) {
                  total2[x-1][y] = 0;
              } else if(total == 2) {
                  total2[x-1][y] = map[x][y];
              } else if(total > 3) {
                  total2[x-1][y] = 0;
              } else if (total == 3) {
                  total2[x-1][y] = 255;
              }

          }

       }
    printf("I'm here at 5\n");




    for (int i = 0; i < (IMWD/4); i++) {
        for (int j = 0; j < IMHT; j++) {
            f_d <: total2[i][j];
        }
    }
    printf("I'm here at 6\n");
    }

}

/////////////////////////////////////////////////////////////////////////////////////////
//
// Write pixel stream from channel c_in to PGM image file
//
/////////////////////////////////////////////////////////////////////////////////////////
void DataOutStream(char outfname[], chanend c_in)
{
  int res;
  uchar line[ IMWD ];

  //Open PGM file
  printf( "DataOutStream: Start...\n" );
  res = _openoutpgm( outfname, IMWD, IMHT );
  if( res ) {
    printf( "DataOutStream: Error opening %s\n.", outfname );
    return;
  }

  //Compile each line of the image and write the image line-by-line
  for( int y = 0; y < IMHT; y++ ) {
    for( int x = 0; x < IMWD; x++ ) {
      c_in :> line[ x ];
//      printf( "-%4.1d ", line[ x ] ); //show image values
    }
//    printf( "\n" );
    _writeoutline( line, IMWD );
    //printf( "DataOutStream: Line written...\n" );
  }

  //Close the PGM image
  _closeoutpgm();
  printf( "DataOutStream: Done...\n" );
  return;
}

/////////////////////////////////////////////////////////////////////////////////////////
//
// Initialise and  read orientation, send first tilt event to channel
//
/////////////////////////////////////////////////////////////////////////////////////////
void orientation( client interface i2c_master_if i2c, chanend toDist) {
  i2c_regop_res_t result;
  char status_data = 0;
  int tilted = 0;

  // Configure FXOS8700EQ
  result = i2c.write_reg(FXOS8700EQ_I2C_ADDR, FXOS8700EQ_XYZ_DATA_CFG_REG, 0x01);
  if (result != I2C_REGOP_SUCCESS) {
    printf("I2C write reg failed\n");
  }
  
  // Enable FXOS8700EQ
  result = i2c.write_reg(FXOS8700EQ_I2C_ADDR, FXOS8700EQ_CTRL_REG_1, 0x01);
  if (result != I2C_REGOP_SUCCESS) {
    printf("I2C write reg failed\n");
  }

  //Probe the orientation x-axis forever
  while (1) {

    //check until new orientation data is available
    do {
      status_data = i2c.read_reg(FXOS8700EQ_I2C_ADDR, FXOS8700EQ_DR_STATUS, result);
    } while (!status_data & 0x08);

    //get new x-axis tilt value
    int x = read_acceleration(i2c, FXOS8700EQ_OUT_X_MSB);

    //send signal to distributor after first tilt
    if (!tilted) {
      if (x>30) {
        tilted = 1 - tilted;
        toDist <: 1;
      }
    }
  }
}

/////////////////////////////////////////////////////////////////////////////////////////
//
// Orchestrate concurrent system and start up all threads
//
/////////////////////////////////////////////////////////////////////////////////////////
int main(void) {

i2c_master_if i2c[1];               //interface to orientation


chan c_inIO, c_outIO, c_control;    //extend your channel definitions here
chan f_d[4];
chan c[4];
chan l[4];
chan r[4];
par {
    on tile [0]: i2c_master(i2c, 1, p_scl, p_sda, 10);   //server thread providing orientation data
    on tile [1]: orientation(i2c[0],c_control);        //client thread reading orientation data
    on tile [0]: DataInStream(infname, c_inIO);          //thread to read in a PGM image
    on tile [1]: DataOutStream(outfname, c_outIO);       //thread to write out a PGM image
    on tile [0]: distributor(c_inIO, c_outIO, c_control, f_d);//thread to coordinate work on image
    par (int i = 0; i < 4; i ++) {
        on tile [i%2]: farmer(i, f_d[i], c[i], l[(i+1)%4], r[(i + 3) %4]);
        on tile [i%2]: barn(i, c[i], l[i], r[i]);
    }
//
//    on tile[0]: farmer(1, f_d[0], c[0], l[1], r[3]);
//    on tile[1]: farmer(2, f_d[1], c[1], l[2], r[0]);
//    on tile[0]: farmer(3, f_d[2], c[2], l[3], r[1]);
//    on tile[1]: farmer(4, f_d[3], c[3], l[0], r[2]);
//    on tile[0]: barn(1, c[0], l[0], r[0]);
//    on tile[1]: barn(2, c[1], l[1], r[1]);
//    on tile[0]: barn(3, c[2], l[2], r[2]);
//    on tile[1]: barn(4, c[3], l[3], r[3]);

//    on tile [0]: farmer(f_d[0], c1,  c2);
//    on tile [0]: farmer(f_d[1], c2, c3);
//    on tile [0]: farmer(f_d[2], c3, c4);
//    on tile [0]: farmer(f_d[3], c4, c5);
//    on tile [0]: farmer(f_d[4], c5, c6);
//    on tile [0]: farmer(f_d[5], c6, c7);
//    on tile [0]: farmer(f_d[6], c7 , c8);
//    on tile [0]: farmer(f_d[7], c8, c1);

}

  return 0;
}
