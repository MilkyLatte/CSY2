/* Copyright (C) 2017 Daniel Page <csdsp@bristol.ac.uk>
 *
 * Use of this source code is restricted per the CC BY-NC-ND license, a copy of
 * which can be found via http://creativecommons.org (and should be included as
 * LICENSE.txt within the associated archive or repository).
 */

#include "hilevel.h"
int n = 3;
pcb_t pcb[ 3 ];
int executing = 0;

void scheduler( ctx_t* ctx ) {

  // memcpy( &pcb[ executing ].ctx, ctx, sizeof( ctx_t ) ); // preserve P_1
  // pcb[ executing ].status = STATUS_READY;                // update   P_1 status
  // memcpy( ctx, &pcb[ (executing + 1)%n ].ctx, sizeof( ctx_t ) ); // restore  P_2
  // pcb[ (executing + 1)%n  ].status = STATUS_EXECUTING;            // update   P_2 status
  // executing = (executing + 1)%n;

  int id = executing;

  for(int i = 0; i < n; i++){
      if (i != executing) {
        pcb[ i ].p_age++;
      }
    }

  for(int i = 0; i < n; i++){
    if(pcb[ i ].p_age + pcb[ i ].p_base > pcb[id].p_base){
      id = i;
    }
  }

  memcpy( &pcb[ executing ].ctx, ctx, sizeof( ctx_t ) );
  pcb[ executing ].status = STATUS_READY;          // update   P_1 status
  memcpy( ctx, &pcb[ id ].ctx, sizeof( ctx_t ) ); // restore  P_2
  pcb[ id  ].status = STATUS_EXECUTING;            // update   P_2 status
  pcb [ id ].p_age = 0;
  executing = id;


  TIMER0->Timer1IntClr = 0x01;

  return;
}

extern void     main_P3();
extern void     main_P4();
extern void     main_P5();
extern uint32_t tos_P;
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


  memset( &pcb[ 0 ], 0, sizeof( pcb_t ) );
  pcb[ 0 ].pid      = 1;
  pcb[ 0 ].status   = STATUS_READY;
  pcb[ 0 ].ctx.cpsr = 0x50;
  pcb[ 0 ].ctx.pc   = ( uint32_t )( &main_P3 );
  pcb[ 0 ].ctx.sp   = ( uint32_t )(&tos_P - 0x00001000);
  pcb[ 0 ].p_base = 15;
  pcb[ 0 ].p_age  = 0;


  memset( &pcb[ 1 ], 0, sizeof( pcb_t ) );
  pcb[ 1 ].pid      = 2;
  pcb[ 1 ].status   = STATUS_READY;
  pcb[ 1 ].ctx.cpsr = 0x50;
  pcb[ 1 ].ctx.pc   = ( uint32_t )( &main_P4 );
  pcb[ 1 ].ctx.sp   = ( uint32_t )(&tos_P - 0x00001000);
  pcb[ 1 ].p_base = 10;
  pcb[ 1 ].p_age  = 0;

  memset( &pcb[ 2 ], 0, sizeof( pcb_t ) );
  pcb[ 2 ].pid      = 3;
  pcb[ 2 ].status   = STATUS_READY;
  pcb[ 2 ].ctx.cpsr = 0x50;
  pcb[ 2 ].ctx.pc   = ( uint32_t )( &main_P5 );
  pcb[ 2 ].ctx.sp   = ( uint32_t )( &tos_P - 0x00001000);
  pcb[ 2 ].p_base = 1;
  pcb[ 2 ].p_age  = 0;

  memcpy( ctx, &pcb[ 0 ].ctx, sizeof( ctx_t ) );
  pcb[ 0 ].status = STATUS_EXECUTING;
  executing = 0;

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

     }

    default   : { // 0x?? => unknown/unsupported
      break;
    }
  }
  return;
}
