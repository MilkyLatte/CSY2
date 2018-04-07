/* Copyright (C) 2017 Daniel Page <csdsp@bristol.ac.uk>
 *
 * Use of this source code is restricted per the CC BY-NC-ND license, a copy of
 * which can be found via http://creativecommons.org (and should be included as
 * LICENSE.txt within the associated archive or repository).
 */

#include "hilevel.h"
//int n = 4;
pcb_t pcb[ 32 ];
pipe_t pip[32];

int executing = 0;
uint32_t stack = 0x00001000;
int sps = 0;
bool rep = false; //replacable
int repIndex = 1; //index to be replaced

void scheduler( ctx_t* ctx ) {

  // memcpy( &pcb[ executing ].ctx, ctx, sizeof( ctx_t ) ); // preserve P_1
  // pcb[ executing ].status = STATUS_READY;                // update   P_1 status
  // memcpy( ctx, &pcb[ (executing + 1)%n ].ctx, sizeof( ctx_t ) ); // restore  P_2
  // pcb[ (executing + 1)%n  ].status = STATUS_EXECUTING;            // update   P_2 status
  // executing = (executing + 1)%n;

  // if(pcb[0].status == STATUS_READY){
  //   write(STDOUT_FILENO, " C READY ", 9);
  //
  // }

  int id = executing;

  for(int i = 0; i < sps; i++){
      if (i != executing && pcb[i].status == STATUS_READY) {
        pcb[ i ].age++;
      }
    }

  for(int i = 0; i < sps; i++){
    if(pcb[ i ].age + pcb[ i ].base > pcb[id].base + pcb[id].age && pcb[ i ].status == STATUS_READY){
      id = i;
    }
  }

  if (pcb[ id ].status == STATUS_READY) {
    memcpy( &pcb[ executing ].ctx, ctx, sizeof( ctx_t ) );
    if( pcb[ executing ].status == STATUS_EXECUTING){
      pcb[ executing ].status = STATUS_READY;          // update   P_1 status
    }
    memcpy( ctx, &pcb[ id ].ctx, sizeof( ctx_t ) ); // restore  P_2
    pcb[ id ].status = STATUS_EXECUTING;            // update   P_2 status
    pcb[ id ].age = 0;
    executing = id;
  }

//   int num = executing;
//   char snum[5];
//
// // convert 123 to string [buf]
//   itoa(snum, num);
//
//   write(STDOUT_FILENO, "executing: ", 11);
//   write(STDOUT_FILENO, snum, 5);
//   write(STDOUT_FILENO, " ", 1);

  int num1 = sps;
  char snum1[5];

//convert 123 to string [buf]
  itoa(snum1, num1);

  write(STDOUT_FILENO, "SPS: ", 5);
  write(STDOUT_FILENO, snum1, 5);
  write(STDOUT_FILENO, " ", 1);




  TIMER0->Timer1IntClr = 0x01;

  return;
}
extern void     main_console();
extern void     main_P1();
extern void     main_P3();
extern void     main_P4();
extern void     main_P5();
extern uint32_t tos_P;
extern uint32_t tos_Cons;
void hilevel_handler_rst(ctx_t* ctx) {
  /* Configure the mechanism for interrupt handling by
   *
   * - configuring timer st. it raises a (periodic) interrupt for each
   *   timer tick,
   * - configuring GIC st. the selected interrupts are forwarded to the
   *   processor via the IRQ interrupt signal, then
   * - enabling IRQ interrupts.
   */

  TIMER0->Timer1Load  = 0x00100000; // select period = 2^20 ticks ~= 1 sec
  TIMER0->Timer1Ctrl  = 0x00000002; // select 32-bit   timer
  TIMER0->Timer1Ctrl |= 0x00000040; // select periodic timer
  TIMER0->Timer1Ctrl |= 0x00000020; // enable          timer interrupt
  TIMER0->Timer1Ctrl |= 0x00000080; // enable          timer

  GICC0->PMR          = 0x000000F0; // unmask all            interrupts
  GICD0->ISENABLER1  |= 0x00000010; // enable timer          interrupt
  GICC0->CTLR         = 0x00000001; // enable GIC interface
  GICD0->CTLR         = 0x00000001; // enable GIC distributor


  memset( &pcb[ sps ], 0, sizeof( pcb_t ) );
  pcb[ sps ].pid      = sps;
  pcb[ sps ].status   = STATUS_READY;
  pcb[ sps ].ctx.cpsr = 0x50;
  pcb[ sps ].ctx.pc   = ( uint32_t )( &main_console );
  pcb[ sps ].ctx.sp   = ( uint32_t )( &tos_Cons);
  pcb[ sps ].base       = 3;
  pcb[ sps ].age        = 0;

  memcpy( ctx, &pcb[ sps ].ctx, sizeof( ctx_t ) );
  pcb[ sps ].status = STATUS_EXECUTING;
  executing = 0;
  sps++;


  int_enable_irq();

  return;
}

void hilevel_handler_irq(ctx_t* ctx) {
  // Step 2: read  the interrupt identifier so we know the source.

  uint32_t id = GICC0->IAR;

  // Step 4: handle the interrupt, then clear (or reset) the source.

  if( id == GIC_SOURCE_TIMER0 ) {
     scheduler(ctx);
  }

  // Step 5: write the interrupt identifier to signal we're done.

  GICC0->EOIR = id;

  return;
}



void hilevel_handler_svc(ctx_t* ctx, uint32_t id ) {
  switch( id ) {
    case 0x00 : { // 0x00 => yield()
      break;
    }

    case 0x01 : { // 0x01 => write( fd, x, n )
      int   fd = ( int   )( ctx->gpr[ 0 ] );
      char*  x = ( char* )( ctx->gpr[ 1 ] );
      int    n = ( int   )( ctx->gpr[ 2 ] );

      for( int i = 0; i < n; i++ ) {
        PL011_putc( UART0, *x++, true );
      }

      ctx->gpr[ 0 ] = n;
      break;
    }
     case 0x03 : { // 0x03 => fork()
       rep = false;
       for (int i = 0; i <= sps; i++){
         if (pcb [ i ].status == STATUS_TERMINATED && !rep){
           rep = true;
           repIndex = i;
         }
       }

       if(!rep){
         memset( &pcb[ sps ], 0, sizeof( pcb_t ) );
         ctx->gpr[ 0 ] = 0;
         memcpy( &pcb[ sps ].ctx, ctx, sizeof( ctx_t ));
         pcb[ sps ].pid      = sps;
         pcb[ sps ].status   = STATUS_READY;
         pcb[ sps ].ctx.sp   = ( uint32_t )(&tos_P - stack);
         pcb[ sps ].baseSP   = ( uint32_t )(&tos_P - stack);
         pcb[ sps ].parent   = executing;
         pcb[ sps ].base     = 1;
         pcb[ sps ].age      = 0;

         stack = stack + 0x00001000;
         sps++;
       } else {
         int x = pcb[repIndex].pid;
         ctx->gpr[ 0 ] = 0;
         memcpy( &pcb[ repIndex ].ctx, ctx, sizeof( ctx_t ));
         pcb[ repIndex ].pid        = x;
         pcb[ repIndex ].ctx.sp     = pcb[ repIndex ].baseSP;;
         pcb[ repIndex ].parent     = executing;
         pcb[ repIndex ].status     = STATUS_READY;
       }
       ctx->gpr[ 0 ] = pcb[executing].pid;


       break;
     }
      case 0x05:{ // exec
        char* x = ( char* )( ctx->gpr[ 0 ]);
        if (x == NULL) {
          break;
        }
        if (rep){// in the case there is an entry in the pcb that can be replaced
          pcb[ repIndex ].ctx.pc = ( uint32_t )(x);
          memcpy( &pcb[ executing ].ctx, ctx, sizeof( ctx_t ) );
          pcb[ executing ].status = STATUS_READY;          // update   P_1 status
          memcpy( ctx, &pcb[ repIndex ].ctx, sizeof( ctx_t ) ); // restore  P_2
          pcb[ repIndex ].status = STATUS_EXECUTING;            // update   P_2 status
          executing = repIndex;
        } else {
          pcb[ sps - 1 ].ctx.pc = ( uint32_t )(x);
          memcpy( &pcb[ executing ].ctx, ctx, sizeof( ctx_t ) );
          pcb[ executing ].status = STATUS_READY;          // update   P_1 status
          memcpy( ctx, &pcb[ sps - 1 ].ctx, sizeof( ctx_t ) ); // restore  P_2
          pcb[ sps - 1  ].status = STATUS_EXECUTING;            // update   P_2 status

          executing = sps - 1;
        }
        break;
       }
       case 0x04 : { // exit
         int num = executing;
         char snum[5];

       // convert 123 to string [buf]
         itoa(snum, num);
         int x = (int )(ctx->gpr[0]);
         //memcpy( &pcb[ executing ].ctx, ctx, sizeof( ctx_t ) );

         pcb[ executing ].status = STATUS_TERMINATED;

         write(STDOUT_FILENO, "exiting: ", 9);
         write(STDOUT_FILENO, snum, 5);
         write(STDOUT_FILENO, " ", 1);

         scheduler(ctx);

         // memcpy( ctx, &pcb[ 0 ].ctx, sizeof( ctx_t ) );
         // pcb[ 0 ].status = STATUS_EXECUTING;
         // executing = 0;
         //if (executing == sps-1){
         //   sps--;
         // }
         break;
       }
       case 0x06 : { // kill
         int id = (int)(ctx -> gpr[0]);
         int x  = (int)(ctx -> gpr[1]);
         pcb[ id ].status = STATUS_TERMINATED;
         // if (executing == sps-1){
         //   sps--;
         // }
         break;
       }
       case 0x08: {
         ctx->gpr[ 0 ] = pcb[executing].pid;
         break;
       }
      default   : { // 0x?? => unknown/unsupported
         break;
      }

      }

      return;
  }
