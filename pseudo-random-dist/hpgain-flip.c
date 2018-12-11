#include <stdio.h>
#include <stdlib.h>

int seed;
#define RN (((seed = seed*11109+13849) & 0x7fff) >> 1)
int rnd(int range) {
    return range == 0 ? 0 : abs(RN) % range;
}

int main(void) {
    for (int t = 0; t < 10000; t++) {
        seed = (int) arc4random();
        int hp = 12;
        for (int level = 0; level < 8; level++) {
            for (int flip = 0; flip < 5; flip++)
                hp += RN & 1 ? 2 : 1;
        }
        printf("%d\n", hp);
    }
    return 0;
}
