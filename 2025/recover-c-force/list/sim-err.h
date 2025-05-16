#ifndef SIM_ERR_H
#define SIM_ERR_H
#include <stdio.h>
#include <stdlib.h>

typedef const char * SimErr;
extern SimErr sim_err;

#define SIM_ERR(v) do { sim_err = (v); } while(0)
#define SIM_MER if (sim_err) { \
        fprintf(stderr, "%s\n", sim_err); \
        sim_err = NULL; } else
#define SIM_ASK SIM_MER{}
#define SIM_PUN if (sim_err) { \
        fprintf(stderr, "%s\n", sim_err); \
        exit(-1); }
#endif
