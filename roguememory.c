/* can we build a level map from memory? */

#include <assert.h>
#include <ctype.h>
#include <locale.h>
#include <ncurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RM_ROWS 24
#define RM_COLS 80
#define MAPSIZE RM_ROWS *RM_COLS

#define REIFY_MAX 256
const unsigned char reify[REIFY_MAX] = {
    '#', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', ' ',  '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',',
    '-', '.', '/',  '0', '1', '2', '3', '4', '5', '6',  '7', '8', '9', ':', ';',
    '<', '=', '>',  '?', '@', 'A', 'B', 'C', 'D', 'E',  'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M',  'N', 'O', 'P', 'Q', 'R', 'S', 'T',  'U', 'V', 'W', 'X', 'Y',
    'Z', '[', '\\', ']', '^', '_', '`', 'a', 'b', 'c',  'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k',  'l', 'm', 'n', 'o', 'p', 'q', 'r',  's', 't', 'u', 'v', 'w',
    'x', 'y', 'z',  '{', '|', '}', '~', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.', '.', '.',  '.', '.', '.', '.', '.', '.', '.',  '.', '.', '.', '.', '.',
    '.'};

int main(void) {
    /* if you want to fiddle around with the map */
    // unsigned char *ptr = malloc(MAPSIZE);
    // assert(ptr);
    // memcpy(ptr, (void *) main, MAPSIZE);
    const unsigned char *ptr = (unsigned char *) main;

    setlocale(LC_ALL, "");
    initscr();
    curs_set(FALSE);
    cbreak();
    noecho();
    nonl();

    int y = 0, x = 0;
    for (int i = 0; i < MAPSIZE; i++) {
        mvwaddch(stdscr, y, x, reify[ptr[i]]);
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
