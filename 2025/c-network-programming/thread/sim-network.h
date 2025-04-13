#ifndef SIM_NETWORK_H
#define SIM_NETWORK_H
#include <sim-str.h>

typedef struct sim_client SimClient;
SimClient *sim_client(const char *host, const char *port);
void sim_client_free(SimClient *client);

void sim_client_send(SimClient *self, SimStr *msg);
SimStr *sim_client_receive(SimClient *self);

typedef struct sim_server SimServer;
SimServer *sim_server(const char *host, const char *port);
SimServer *sim_server_copy(SimServer *server);
void sim_server_free(SimServer *server);

void sim_server_run(SimServer *self);
void sim_server_close(SimServer *server);

void sim_server_send(SimServer *self, SimStr *msg);
SimStr *sim_server_receive(SimServer *self);

bool sim_client_safe(SimClient *self);
bool sim_server_safe(SimServer *self);

#endif







