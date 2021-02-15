// simple integer energy system, ECS style

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "energy.h"

int main(void) {
    struct ents *el = ent_init();
    ent_create(el, '@', "Bob", player_update, 2, 5);
    ent_create(el, 'T', "Tom", monst_update, 2, 4);
    ent_create(el, 'T', "Tim", monst_update, 2, 3);
    // should instead be infinite game loop until victory/death/entropy
    for (int i = 0; i < 4; i++)
        ecs_energy(el);
    return 0;
}

void ecs_energy(struct ents *el) {
    unsigned int min           = UINT_MAX;
    static unsigned long clock = 0;
    for (unsigned int eid = 0; eid < ENT_COUNT; eid++)
        if ((el->mask[eid] & COMP_ENERGY) == COMP_ENERGY)
            if (el->energy[eid].cost < min) min = el->energy[eid].cost;
    clock += min;
    for (unsigned int eid = 0; eid < ENT_COUNT; eid++) {
        if ((el->mask[eid] & COMP_ENERGY) == COMP_ENERGY) {
            if (el->energy[eid].cost == min) {
                unsigned int cost = (el->energy[eid].fn)(el, eid, clock);
                assert(cost > 0 && cost < UINT_MAX);
                el->energy[eid].cost = cost;
            } else {
                // not their turn... make it closer to being so
                el->energy[eid].cost -= min;
            }
        }
    }
}

unsigned int ent_create(struct ents *el, char ch, char *name, ent_update eup,
                        int x, int y) {
    unsigned int eid    = ent_getid(el);
    el->mask[eid]       = COMP_DISPLAY | COMP_ENERGY | COMP_LOCATION;
    el->display[eid].ch = ch;
    strncpy(el->display[eid].name, name, DISPLAY_NAME_MAX);
    el->energy[eid].cost = ENERGY_MOVE_NOW;
    el->energy[eid].fn   = eup;
    el->location[eid].x  = x;
    el->location[eid].y  = y;
    return eid;
}

inline void ent_delete(struct ents *el, unsigned int eid) {
    el->mask[eid] = COMP_NONE;
}

unsigned int ent_getid(struct ents *el) {
    for (unsigned int eid = 0; eid < ENT_COUNT; eid++)
        if (el->mask[eid] == COMP_NONE) return eid;
    abort(); // whoops entity list was too small
}

struct ents *ent_init(void) {
    struct ents *el;
    if ((el = calloc(1, sizeof(struct ents))) == NULL) oom();
    return el;
}

unsigned int monst_update(struct ents *el, unsigned int eid,
                          unsigned long clock) {
    // debug, presumably display would be handled by some other system
    fprintf(stderr, "monster update id=%u clock=%-4lu |%s|\n", eid, clock,
            el->display[eid].name);
    // this would likely vary depending on action taken, etc
    return 5;
}

unsigned int player_update(struct ents *el, unsigned int eid,
                           unsigned long clock) {
    fprintf(stderr, "player  update id=%u clock=%-4lu |%s|\n", eid, clock,
            el->display[eid].name);
    return 10;
}
