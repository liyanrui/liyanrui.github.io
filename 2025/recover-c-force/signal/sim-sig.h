#ifndef SIM_SIG_H
#define SIM_SIG_H
#include <sim-list.h>

typedef const char * SimSig;
typedef SimVal *(*SimRouter)(SimSig sig, SimVal *v);

bool sim_sig_is(SimSig a, SimSig b);
void sim_bus_connect(SimList *bus, SimSig sig, SimRouter router);
void sim_bus_emit(SimList *bus, SimSig sig, SimVal *value);
void sim_bus_schedule(SimList *bus);
SimArray *sim_bus_wait(SimList *bus, SimSig sig);
void sim_bus_del(SimList *bus, SimSig sig);
void sim_bus_free(SimList *bus);
#endif
