#ifndef __HELLOWORLD_H
#define __HELLOWORLD_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "libc.h"

typedef enum {
  STATUS_CLEAN,
  STATUS_DIRTY,
} status_t;

typedef struct{
  int pipe;
  int server;
  int client;
}pipeinfo_t;

typedef struct{
  int phID;
  pipeinfo_t to, from;
  status_t status; //thinking, hungry, eating
}phil_t;

typedef struct{
  int semaphore;
  int rightOwner;
  int leftOwner;
  status_t status;
}chop_t;

#endif
