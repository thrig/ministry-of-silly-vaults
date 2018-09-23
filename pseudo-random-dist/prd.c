/* compare PRD with more typical <= 25% check */

#include <stdio.h>
#include <stdlib.h>

#include "prd.h"

#define RUNS 1000
#define TRIALS 10000

int main(void)
{
    uint32_t v;
    for (int r = 0; r < RUNS; r++) {
        int offset = 0;
        unsigned int prd_run = 0;
        unsigned int reg_run = 0;
        unsigned int prd_max = 0;
        unsigned int reg_max = 0;
        for (int t = 0; t < TRIALS; t++) {
            v = arc4random();
            if (v <= PRD_VALUE(offset)) {
                if (prd_run > prd_max)
                    prd_max = prd_run;
                prd_run = 0;
                offset = 0;
            } else {
                offset = PRD_NEXT(offset);
                prd_run++;
            }
            if (v <= 1073741824) {
                if (reg_run > reg_max)
                    reg_max = reg_run;
                reg_run = 0;
            } else {
                reg_run++;
            }
        }
        printf("%u %u\n", prd_max, reg_max);
    }
    exit(0);
}
