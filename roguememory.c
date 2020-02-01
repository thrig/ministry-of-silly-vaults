/* can we build a level map from memory of the stack? */

#include <err.h>
#include <locale.h>
#include <ncurses.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RM_COLS 80
#define RM_ROWS 24
#define MAPSIZE RM_ROWS *RM_COLS

void stack(void *ptr, unsigned char *const map, size_t need);
void stack2(void *ptr, unsigned char *const map, size_t need);

int main(void) {
    unsigned char *map = malloc(MAPSIZE);
    if (!map) err(1, "malloc failed");

    setlocale(LC_ALL, "");
    initscr();
    curs_set(FALSE);
    cbreak();
    noecho();
    nonl();

    // map is somewhere on the heap, &map is here on the stack
    stack((void *) &map, map, MAPSIZE);

    int y = 0, x = 0;
    for (int i = 0; i < MAPSIZE; i++) {
        mvwaddch(stdscr, y, x, map[i]);
        if (++x >= RM_COLS) {
            x = 0;
            y++;
        }
    }

    refresh();
    getch();
    curs_set(TRUE);
    endwin();
    return 0;
}

void stack(void *ptr, unsigned char *const map, size_t need) {
    const unsigned char here = 'x';
    const unsigned char *const start = (const unsigned char *const) ptr;
    // stack grows top down (I hope) so start will have a higher address
    // than anything recursed into here
    if (start - &here > need) {
        volatile char buf[5];
        for (size_t i = 0; i < need; i++)
            map[i] = *(start - i) ? '.' : '#';
        return;
    }
    if (arc4random() > 2147483648) {
        stack2(ptr, map, need);
    } else {
        stack(ptr, map, need);
    }
}

void stack2(void *ptr, unsigned char *const map, size_t need) {
    stack(ptr, map, need);
    volatile char buf[50];
}
