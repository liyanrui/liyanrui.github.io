#include "foo.h"
int main(void) {
        SimList *bus = sim_list();
        sim_bus_connect(bus, "hi", foo_router);
        sim_bus_emit(bus, "hi", NULL);
        sim_bus_schedule(bus);
        sim_bus_free(bus);
        return 0;
}
