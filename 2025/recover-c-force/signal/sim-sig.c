#include "sim-sig.h"

typedef struct {
        bool active;
        SimSig sig;
        SimRouter router;
        SimVal *input;
        SimVal *output;
} SimDevice;

bool sim_sig_is(SimSig a, SimSig b) {
        return (strcmp(a, b) == 0) ? true : false;
}

void sim_bus_connect(SimList *bus, SimSig sig, SimRouter router) {
        if (!bus) {
                SIM_ERR3("invalid bus!");
                return;
        }
        if (!router) {
                SIM_ERR3("invalid router!");
                return;
        }
        SimDevice *dev = malloc(sizeof(SimDevice));
        if (!dev) {
                SIM_ERR3("failed to allocate memory for device!");
                return;
        }
        *dev = (SimDevice){false, sig, router, NULL, NULL};
        sim_list_suffix(bus, dev, SimDevice *);
}

void sim_bus_emit(SimList *bus, SimSig sig, SimVal *value) {
        if (!bus) {
                SIM_ERR3("invalid bus!");
                return;
        }
        for (SimLink *it = bus->head; it; it = it->next) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                if (sim_sig_is(dev->sig, sig)) {
                        if (dev->active || dev->input) {
                                SIM_ERR3("device busy!");
                                continue;
                        }
                        dev->input = value;
                        dev->active = true; /* 激活设备 */
                }
        }
}

void sim_bus_schedule(SimList *bus) {
        if (!bus) {
                SIM_ERR3("invalide bus!");
                return;
        }
        for (SimLink *it = bus->head; it; it = it->next) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                if (dev->active) {
                        dev->output = dev->router(dev->sig, dev->input);
                }
        }
}

SimArray *sim_bus_wait(SimList *bus, SimSig sig) {
        if (!bus) {
                SIM_ERR3("invalide bus!");
                return NULL;
        }
        SimArray *values = sim_array(SimVal *);
        for (SimLink *it = bus->head; it; it = it->next) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                if (!dev->active) continue;
                if (sim_sig_is(dev->sig, sig)) {
                        if (dev->output) {
                                sim_array_add(values, dev->output, SimVal *);
                                dev->output = NULL;
                        }
                        /* 设备重置，等待下一次被激活 */
                        dev->input = NULL;
                        dev->active = false;
                }
        }
        return values;
}

void sim_bus_del(SimList *bus, SimSig sig) {
        if (!bus) {
                SIM_ERR3("invalid bus!");
                return;
        }
        SimLink *it = bus->head;
        while (it) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                if (sim_sig_is(dev->sig, sig)) {
                        free(dev);
                        SimLink *next = it->next;
                        sim_list_del(bus, it);
                        it = next;
                        continue;
                }
                it = it->next;
        }
}

void sim_bus_free(SimList *bus) {
        if (!bus) return;
        for (SimLink *it = bus->head; it; it = it->next) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                free(dev);
        }
        sim_list_free(bus);
}
