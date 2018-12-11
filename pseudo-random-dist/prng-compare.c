#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define TRIALS 10000

#define PRD_COLS 7
uint32_t odds[PRD_COLS] = { 128849018, 730144440, 1245540515,
    1589137899, 1846835937, 2534030704, 3564822855 };

int main(void) {
    for (int r = 0; r < 10000; r++) {
        int counter = 0, prd_hits = 0, reg_hits = 0;
        for (int t = 0; t < TRIALS; t++) {
            uint32_t v = arc4random();
            if (arc4random() <= odds[counter]) {
                prd_hits++;
                counter = 0;
            } else {
                counter = counter >= PRD_COLS ? 0 : counter + 1;
            }
            if (v <= 1055273465) reg_hits++;
        }
        printf("%g %g\n", prd_hits / (double) TRIALS,
               reg_hits / (double) TRIALS);
    }
    exit(0);
}
