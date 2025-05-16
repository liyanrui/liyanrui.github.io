#include "foo.h"

int main(void) {
        SimList *bus = sim_list();
        sim_bus_connect(bus, "hi", foo_router );
        sim_bus_emit(bus, "hi", NULL);
        sim_bus_schedule(bus);
        SimArray *results = sim_bus_wait(bus, "hi");
        for (size_t i = 0; i < results->n; i++) {
                SimVal *v = sim_array_raw(results, i, SimVal *);
                printf("%d\n", sim_val_raw(v, int));
                sim_val_free(v);
        }
        sim_array_free(results);
        sim_bus_free(bus);
        return 0;
}
