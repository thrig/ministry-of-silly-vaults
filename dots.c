// dots - draw dots somewhat randomly in a terminal. this code suits a
// larger terminal (e.g. 170x58 or such). there are things to TWEAK

#include <sys/ioctl.h>

#include <err.h>
#include <math.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct {
    double x;
    double y;
} vector_t;

typedef struct {
    vector_t accel;
    vector_t velo;
    vector_t position;
} thingy_t;

typedef struct {
    uint8_t rr;
    uint8_t gg;
    uint8_t bb;
} color_t;

void poked(int sig);

int main(int argc, char *argv[]) {
#ifdef __OpenBSD__
    if (pledge("stdio tty", NULL) == -1) err(1, "pledge failed");
#else
    srand(time(NULL)); // BAD
#endif

    struct winsize w;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) == -1) err(1, "ioctl failed");

    // TWEAK how much to fill up the terminal. may need more fill on a
    // smaller terminal
    int iters = w.ws_col * w.ws_row / 4;

    thingy_t *q = malloc(sizeof(thingy_t));
    if (q == NULL) err(1, "malloc failed");
    q->accel.x = q->accel.y = q->velo.x = q->velo.y = 0.0;
    // start somewhere
    q->position.x = rand() % w.ws_col;
    q->position.y = rand() % w.ws_row;

    color_t *c = malloc(sizeof(color_t));
    if (c == NULL) err(1, "malloc failed");
    c->rr = c->gg = c->bb = 0;

    // ANSI or XTerm Control Sequences - https://invisible-island.net/xterm/
    dprintf(STDOUT_FILENO,
            "\033[?1049h\033[1;1H\033[2J\033[?25l\033[>2p\033[m");

    signal(SIGINT, poked);

    // TWEAK
    //while (1) {
    for (int i = 0; i < iters; i++) {
        q->velo.x += q->accel.x;
        q->velo.y += q->accel.y;
        q->position.x += q->velo.x;
        q->position.y += q->velo.y;

        // keep it inbounds (may need to round downwards?)
        q->position.x = fmod(q->position.x, w.ws_col);
        q->position.y = fmod(q->position.y, w.ws_row);

        dprintf(STDOUT_FILENO, "\033[%.f;%.fH\033[38;2;%u;%u;%um%c\033[m",
                q->position.y + 1, q->position.x + 1, c->rr, c->gg, c->bb,
                (rand() > RAND_MAX / 2) ? '.' : ' ');
        c->rr = (c->rr + 1) % 256;
        c->gg = (c->gg + 1) % 256;
        c->bb = (c->bb + 1) % 256;

        // TWEAK lower acceleration makes the track the thingy takes
        // more visible
        double angle = rand() / (double) RAND_MAX * M_PI * 2;
        q->accel.x   = cos(angle) / 5.5;
        q->accel.y   = sin(angle) / 5.5;
    }

    exit(1);
}

void poked(int sig) {
    signal(SIGINT, SIG_DFL);
    dprintf(STDOUT_FILENO, "\033[m\033[?25h\033[?1049l");
    raise(SIGINT);
}
