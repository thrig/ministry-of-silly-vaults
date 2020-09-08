#ifndef oom
#define oom() exit(-1)
#endif

#define ENT_COUNT 8U

#define DISPLAY_NAME_MAX 30U

// forward decl due to function pointer typedef taking this type
struct ents;

struct display {
    char ch;
    char name[DISPLAY_NAME_MAX + 1];
};

typedef unsigned int (*ent_update)(struct ents *el, unsigned int eid,
                                   unsigned long clock);

#define ENERGY_MOVE_NOW 0U

struct energy {
    unsigned int cost;
    ent_update fn;
};

struct location {
    int x;
    int y;
};

enum {
    COMP_NONE     = 0,
    COMP_DISPLAY  = 1 << 0,
    COMP_ENERGY   = 1 << 1,
    COMP_LOCATION = 1 << 2
};

struct ents {
    int mask[ENT_COUNT];
    struct display display[ENT_COUNT];
    struct energy energy[ENT_COUNT];
    struct location location[ENT_COUNT];
};

struct ents *ent_init(void) {
    struct ents *el;
    if ((el = calloc(1, sizeof(struct ents))) == NULL) oom();
    return el;
}

void ecs_energy(struct ents *el);
unsigned int ent_getid(struct ents *el);
void ent_delete(struct ents *el, unsigned int eid);
unsigned int ent_create(struct ents *el, char ch, char *name, ent_update eup,
                        int x, int y);
unsigned int monst_update(struct ents *el, unsigned int eid,
                          unsigned long clock);
unsigned int player_update(struct ents *el, unsigned int eid,
                           unsigned long clock);
