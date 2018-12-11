#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define TRIALS 10000

#define PRD_COLS 7
uint32_t odds[PRD_COLS] = { 128849018, 730144440, 1245540515,
    1589137899, 1846835937, 2534030704, 3564822855 };

int main(void) {
    int counter = 0, hits = 0;
    for (int t = 0; t < TRIALS; t++) {
        if (arc4random() <= odds[counter]) {
            hits++;
            counter = 0;
        } else {
            counter = counter >= PRD_COLS ? 0 : counter + 1;
        }
    }
    printf("%g\n", hits / (double) TRIALS);
    return 0;
}
