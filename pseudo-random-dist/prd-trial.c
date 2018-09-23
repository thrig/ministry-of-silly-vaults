/* test what sort of odds the PRD generates (prd.h will likely require
 * tweaks to get what you want) */

#include <stdio.h>
#include <stdlib.h>

#include "prd.h"

#define RUNS   1000
#define TRIALS 100000

int main(void)
{
    for (int r = 0; r < RUNS; r++) {
        unsigned int offset = 0;
        unsigned int hits = 0;
        for (int t = 0; t < TRIALS; t++) {
            if (arc4random() <= PRD_VALUE(offset)) {
                hits++;
                offset = 0;
            } else {
                offset = PRD_NEXT(offset);
            }
        }
        printf("%g\n", hits / (double) TRIALS);
        /* here one might re-seed the RNG, if need be */
    }
    exit(0);
}
