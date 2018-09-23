/* PRD table lookup implementation. assumes a 32-bit RNG */

#ifndef PRD_H
#define PRD_H

#include <stdint.h>

#define PRD_COLS 7

/* 3% 17% 29% 37% 43% 59% 83% -- works out to ~25% or so */
uint32_t prd_table[1][PRD_COLS] = {
    {128849018, 730144440, 1245540515, 1589137899, 1846835937, 2534030704, 3564822855}
};

#define PRD_NEXT(col)   (col >= PRD_COLS ? 0 : col+1)
#define PRD_VALUE(col)  prd_table[0][col]

#endif
