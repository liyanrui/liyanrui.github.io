#ifndef SIM_ERR_H
#define SIM_ERR_H
#include <stdio.h>
#include <stdlib.h>

typedef const char * SimErr;
#if __STDC_VERSION__ >= 201112L
        extern _Thread_local SimErr sim_err;
#else
        extern SimErr sim_err;
        #warning "sim_err is not safe for multithreading."
#endif

#define SIM_ERR(v) do { sim_err = (v); } while(0)
#define SIM_MER if (sim_err) { \
        fprintf(stderr, "%s\n", sim_err); \
        sim_err = NULL; } else
#define SIM_PUN if (sim_err) { \
        fprintf(stderr, "%s\n", sim_err); \
        exit(-1); }
#endif
