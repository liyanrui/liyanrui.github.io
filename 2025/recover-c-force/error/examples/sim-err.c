#include "sim-err.h"

#if __STDC_VERSION__ >= 201112L
        _Thread_local SimErr sim_err = NULL;
#else
        SimErr sim_err = NULL;
#endif
