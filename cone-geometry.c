// cone-geometry - animate a cone in the terminal
//
// two rays (segments?) are drawn out along the edges of the cone and a
// line is created between the endpoints of those segments (rays?). then
// additional raysegments are drawn from the origin out to each point
// along the connecting line. this however will leave some gaps for
// various angles. another method might be to draw lines between each
// point along the two segmentrays, but this leads to more gaps than the
// raycasting method
//
// it might make more sense to use a FOV algorithm but restrict it to
// only certain cells as defined by the rays...

#include <locale.h>
#include <math.h>
#include <ncurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define MSEC_IN_SEC 1000000

typedef void (*line_callback)(int, int, void *);

// TODO need better name for this
struct point {
    int x;
    int y;
    chtype ch;
};

struct timespec delay;

double deg2rad(int degrees);
int rad2deg(double rad);

void cone(int x, int y, double dir, double width, unsigned int len, chtype ch);
void cone_each_ray(int x, int y, void *ptr);
void cone_ray(int x, int y, void *ptr);
void line(int x0, int y0, int x1, int y1, line_callback cb, void *ptr);

int main(int argc, char *argv[]) {
    char *astr;

    setlocale(LC_ALL, "");
    initscr();
    curs_set(FALSE);
    cbreak();
    noecho();
    nonl();

    delay.tv_nsec = 50 * MSEC_IN_SEC;

    for (int angle = 0; angle < 365; angle += 5) {
        // causes flicker, which may or may not be desired
        //clearok(stdscr, TRUE);
        cone(40, 12, deg2rad(angle), deg2rad(20), 12, ACS_DIAMOND);
        asprintf(&astr, "%d", angle);
        mvaddstr(0, 0, astr);
        refresh();
        nanosleep(&delay, NULL);
        //getch();
        cone(40, 12, deg2rad(angle), deg2rad(20), 12, ' ');
    }

    //cone(40, 12, deg2rad(135), deg2rad(20), 12, ACS_DIAMOND);
    //getch();
    curs_set(TRUE);
    endwin();
    return 0;
}

// x == col, y == row (but terminal is upsidedown comp unit circle)
void cone(int x, int y, double dir, double width, unsigned int len, chtype ch) {
    struct point origin;
    double nx1, ny1, nx2, ny2;

    origin.x  = x;
    origin.y  = y;
    origin.ch = ch;

    // rays that outline the cone from the origin
    nx1 = x + cos(dir - width) * len;
    ny1 = y + sin(dir - width) * len;
    nx2 = x + cos(dir + width) * len;
    ny2 = y + sin(dir + width) * len;

    // draw a line between the endpoints of the two raws that outline
    // the cone, then cone_ray from origin to each of the points on that
    // line. downside: the ray end is a line, not what probably instead
    // should be an arc
    line(nx1, ny1, nx2, ny2, cone_each_ray, (void *) &origin);
}

void cone_each_ray(int x, int y, void *ptr) {
    struct point *origin = ptr;
    line(origin->x, origin->y, x, y, cone_ray, (void *) &origin->ch);
}

void cone_ray(int x, int y, void *ptr) {
    chtype *ch = ptr;
    move(y, x);
    addch(*ch);
}

double deg2rad(int degrees) { return degrees * (M_PI / 180.0); }

// Bresenham's line algorithm (from Rosetta Code)
void line(int x0, int y0, int x1, int y1, line_callback cb, void *ptr) {
    int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
    int dy = abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
    int err = (dx > dy ? dx : -dy) / 2, e2;
    while (1) {
        cb(x0, y0, ptr);
        if (x0 == x1 && y0 == y1) break;
        e2 = err;
        if (e2 > -dx) {
            err -= dy;
            x0 += sx;
        }
        if (e2 < dy) {
            err += dx;
            y0 += sy;
        }
    }
}

int rad2deg(double rad) { return rad * (180 / M_PI); }
