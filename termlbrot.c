/* Mandelbrot, only at the resolution and graphics capabilities of a terminal */

#include <err.h>
#include <complex.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// TWEAK binary floor/wall offers too low a resolution, hence ~ for water
// and , for rubble (colors might also help)
#define FLOOR_TILES 8
const char Floor_Tiles[FLOOR_TILES] =
    { '~', '.', '.', '.', '.', ',', ',', '#' };

unsigned int **make_matrixui(unsigned int rows, unsigned int cols);

int main(void)
{
    // TWEAK area to iterate over
    const long double run_real[2] = { -1.0, 0 };
    const long double run_imag[2] = { -1.5, 0.5 };
    // TWEAK how big the terminal or output canvas is (and thus how much
    // resolution the fractal is displayed at)
    const unsigned int rows = 39;
    const unsigned int cols = 78;
    // TODO can these autoscale depending on how tight the graph is?
    const unsigned int iterations = 1000;
    const long double threshold = 120;

    long double range_real, range_imag;
    long double cur_row, cur_col;
    long double complex coord, zeta;

    unsigned int **field = make_matrixui(rows, cols);
    long index;
    unsigned int stability;
    unsigned int min_stab = UINT_MAX;
    unsigned int max_stab = 0;

    // TODO there's nothing that links the run ratio to the terminal ratio
    range_real = run_real[1] - run_real[0];
    range_imag = run_imag[1] - run_imag[0];

    for (int r = 0; r < rows; r++) {
        cur_row = run_real[0] + range_real / (long double) rows *r;
        for (int c = 0; c < cols; c++) {
            cur_col = run_imag[0] + range_imag / (long double) cols *c;
            zeta = 0.0 + 0.0 * I;
            // TWEAK rotate by flipping these
            coord = cur_col + cur_row * I;
            stability = 0;
            for (unsigned int i = 0; i < iterations; i++) {
                // TWEAK the equation, if need be
                zeta = zeta * zeta + coord;
                stability++;
                if (!isfinite(creal(zeta)) || !isfinite(cimag(zeta))
                    || creal(zeta) > threshold || cimag(zeta) > threshold) {
                    break;
                }
            }
            if (stability < min_stab)
                min_stab = stability;
            if (stability > max_stab)
                max_stab = stability;

            field[r][c] = stability;
        }
    }

    // TWEAK some other equation might favor more open space vs. not
    long double slope =
        (0.0 - (FLOOR_TILES - 1)) / (long double) (max_stab - min_stab);
    long double intercept = 0.0 - slope * (long double) max_stab;

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            index = lrintl(slope * (long double) field[r][c] + intercept);
            if (index < 0)
                index = 0;
            else if (index >= FLOOR_TILES - 1)
                index = FLOOR_TILES - 1;
            putchar(Floor_Tiles[index]);
        }
        putchar('\n');
    }

    exit(EXIT_SUCCESS);
}

unsigned int **make_matrixui(unsigned int rows, unsigned int cols)
{
    unsigned int i;
    unsigned int **matrix;
    if ((matrix = malloc(rows * sizeof(unsigned int *))) == NULL)
        err(1, "could not malloc matrix rows");
    if ((matrix[0] =
         (unsigned int *) malloc(rows * cols * sizeof(unsigned int))) == NULL)
        err(1, "could not malloc matrix");
    for (i = 1; i < rows; i++) {
        matrix[i] = matrix[0] + i * cols;
    }
    return matrix;
}
