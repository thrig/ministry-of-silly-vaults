/* Mandelbrot, only at the resolution and graphics capabilities of a terminal */

#include <sys/ioctl.h>
#include <sys/ttycom.h>

#include <err.h>
#include <complex.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// TWEAK binary floor/wall offers too low a resolution, hence ~ for water
// and , for rubble (colors might also help)
#define FLOOR_TILES 8
const char *Floor_Tiles[FLOOR_TILES] =
    { "\033[34m~", "\033[33m.", "\033[37m.", "\033[31m.", "\033[35m.", "\033[32m,", "\033[32m+", "\033[35m#" };

unsigned int **make_matrixui(unsigned int rows, unsigned int cols);

int main(void)
{
    // TWEAK how big the terminal or output canvas is (and thus how much
    // resolution the fractal is displayed at)
    struct winsize w;
    unsigned int rows = 23;
    unsigned int cols = 80;

    // TWEAK area to iterate over
    long double run_real[2] = { -0.9, -0.7 };
    long double run_imag[2] = { -1.0, 0.5 };

    long double randval;

    // TWEAK autoscale depending on how tight the graph is?
    // lowering them will a) save CPU and b) increase open space
    const unsigned int iterations = 100;
    const long double threshold = 1.5;

    long double range_real, range_imag;
    long double cur_row, cur_col;
    long double complex coord, zeta;

    unsigned int **field;
    long index;
    unsigned int stability;
    unsigned int min_stab = UINT_MAX;
    unsigned int max_stab = 0;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) == -1) {
        warn("could not obtain terminal size from STDOUT");
    } else {
        cols = w.ws_col;
        rows = w.ws_row - 2;
    }

    field = make_matrixui(rows, cols);

    randval = 0.0 - arc4random() / (long double)UINT32_MAX;
    printf("I0=%Lf", randval);
    run_imag[0] = randval;

    randval = 0.0 - arc4random() / (long double)UINT32_MAX;
    printf(" R0=%Lf", randval);
    run_real[0] = randval;

    randval = arc4random() / (long double)UINT32_MAX;

    // TWEAK divide factor is how much to zoom in by
    run_real[1] = run_real[0] + randval / 4;
    printf(" R1=%Lf\n", run_real[1]);

    run_imag[1] = run_imag[0] + rows * (run_real[1]-run_real[0]) / (long double) cols * 3;

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
                // TWEAK the equation, if need be (Julia sets set some
                // constant for coord and zeta is the r/c value)
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
            printf("%s", Floor_Tiles[index]);
        }
        putchar('\n');
    }
    printf("\033[0m");

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
