/* Five degree accuracy, trig function verses lookup table.
 *
 * On linux, may require something like
 *   CFLAGS="-std=c99 -lm -lrt -D_XOPEN_SOURCE -D_POSIX_C_SOURCE=199309" make five-degrees
 *
 * to compile (what is with all the defines and such complications?!).
 * Also, Mac OS X only in 10.12 got clock_gettime, or so I've heard.
 */

#include <sys/resource.h>

#include <err.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <time.h>
#include <unistd.h>

#define DEG2RAD(d)  (M_PI * (double) (d) / 180.)

// NOTE assumes positive degree, 0..359
#define NEAREST_DEGREE(d)  (((d) + 2) / 5 * 5 % 360)
#define INDEXOF(degree)    ((degree) / 5)

#define NSEC_IN_SEC 1000000000
#define MSEC_IN_SEC    1000000

#define MAX_ERROR 0.0001

#define TEST_REPITITIONS 100000000

// though for additional speed might use ints at a precision of 1000 or
// something, so 0.173 would be 173, etc.
double lookup[72] =
    { 0.000000, 0.087156, 0.173648, 0.258819, 0.342020, 0.422618, 0.500000,
    0.573576, 0.642788, 0.707107, 0.766044, 0.819152, 0.866025, 0.906308,
    0.939693, 0.965926, 0.984808, 0.996195, 1.000000, 0.996195, 0.984808,
    0.965926, 0.939693, 0.906308, 0.866025, 0.819152, 0.766044, 0.707107,
    0.642788, 0.573576, 0.500000, 0.422618, 0.342020, 0.258819, 0.173648,
    0.087156, 0.000000, -0.087156, -0.173648, -0.258819, -0.342020, -0.422618,
    -0.500000, -0.573576, -0.642788, -0.707107, -0.766044, -0.819152,
    -0.866025, -0.906308, -0.939693, -0.965926, -0.984808, -0.996195, -1.000000,
    -0.996195, -0.984808, -0.965926, -0.939693, -0.906308, -0.866025, -0.819152,
    -0.766044, -0.707107, -0.642788, -0.573576, -0.500000, -0.422618, -0.342020,
    -0.258819, -0.173648, -0.087156
};

void bylookup(void);
void bysin(void);
void deltaof(void (*thecall) (), const char *name);

void sinof(int degree);

int main(int argc, char *argv[])
{
    double a, b;

    // how lookup table above was generated
    //for (int i = 0; i < 360; i += 5) sinof(i);

    // double check that both methods get (something like the) same answer...
    for (int d = 0; d < 360; d++) {
        a = sin(DEG2RAD(NEAREST_DEGREE(d)));
        b = lookup[INDEXOF(NEAREST_DEGREE(d))];
        if (fabs(a - b) > MAX_ERROR) {
            printf("not ok - degree %d (%f vs %f)\n", d, a, b);
            exit(1);
        }
    }

    deltaof(bysin, "sin");
    deltaof(bylookup, "lookup");

    exit(EXIT_SUCCESS);
}

void bylookup(void)
{
    double a;
    for (int r = 0; r < TEST_REPITITIONS; r++) {
        a = lookup[INDEXOF(NEAREST_DEGREE(r))];
    }
}

void bysin(void)
{
    double a;
    for (int r = 0; r < TEST_REPITITIONS; r++) {
        a = sin(DEG2RAD(NEAREST_DEGREE(r)));
    }
}

void deltaof(void (*thecall) (), const char *name)
{
    long double delta_t, delta_user, delta_sys;
    struct rusage use_before, use_after;
    struct timespec before, after;

    if (getrusage(RUSAGE_SELF, &use_before) == -1)
        err(EX_OSERR, "getrusage() failed");
    if (clock_gettime(CLOCK_REALTIME, &before) == -1)
        err(EX_OSERR, "clock_gettime() failed");

    thecall();

    if (clock_gettime(CLOCK_REALTIME, &after) == -1)
        err(EX_OSERR, "clock_gettime() failed");
    if (getrusage(RUSAGE_SELF, &use_after) == -1)
        err(EX_OSERR, "getrusage() failed");

    delta_t =
        (after.tv_sec - before.tv_sec) + (after.tv_nsec -
                                          before.tv_nsec) /
        (long double) NSEC_IN_SEC;


    delta_user =
        (use_after.ru_utime.tv_sec - use_before.ru_utime.tv_sec) +
        (use_after.ru_utime.tv_usec -
         use_before.ru_utime.tv_usec) / (long double) MSEC_IN_SEC;
    delta_sys =
        (use_after.ru_stime.tv_sec - use_before.ru_stime.tv_sec) +
        (use_after.ru_stime.tv_usec -
         use_before.ru_stime.tv_usec) / (long double) MSEC_IN_SEC;

    fprintf(stderr, "%s\t%.6Lf real %.6Lf user %.6Lf system\n", name, delta_t,
            delta_user, delta_sys);
}

void sinof(int degree)
{
    printf("%f, ", sin(DEG2RAD(degree)));
}
