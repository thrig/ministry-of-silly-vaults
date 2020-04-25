/* sin-map - sin-map.lisp only in C and with aalib */

#include <err.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <aalib.h>

int main(int argc, char *argv[]) {
    if (!aa_parseoptions(NULL, NULL, &argc, argv) || argc != 1) {
        printf("%s", aa_help);
        exit(1);
    }

    aa_context *context;
    context = aa_autoinit(&aa_defparams);
    if (context == NULL) err(1, "aa_autoinit failed");

    //aa_palette palette;
    //unsigned char *framebuffer;
    //framebuffer = aa_image(context);

    // KLUGE use -width and -height to match desired image size
    // so -width $COLUMNS -height $LINES to match terminal size
    int iwidth  = aa_imgwidth(context);
    int iheight = aa_imgheight(context);

    // trig noise
    double cratio = M_PI / iwidth;
    double rratio = M_PI / iheight;
    // TWEAK these move the trig function inputs around
    int off1      = arc4random_uniform(iwidth);
    int off2      = arc4random_uniform(iwidth);
    // TWEAK should probably be small integers
    int mul1      = 1 + arc4random_uniform((int) sqrt(iheight));
    int mul2      = 1 + arc4random_uniform((int) sqrt(iwidth));
    for (int r = 0; r < iheight; r++) {
        for (int c = 0; c < iwidth; c++) {
            double val = 1 +
                         0.25 * sin((double) (r - off1) * rratio *
                                    (1 + arc4random_uniform(2))) +
                         0.25 * cos((double) (c - off2) * cratio *
                                    (1 + arc4random_uniform(3))) +
                         0.25 * sin((double) (r - off2) * rratio * mul1) +
                         0.25 * cos((double) (c - off1) * cratio * mul2);
            // TWEAK 0..255 range the ~ 0..2 val should be mapped into
            aa_putpixel(context, c, r, (int) (24 * val));
        }
    }

    aa_render(context, &aa_defrenderparams, 0, 0, aa_scrwidth(context),
              aa_scrheight(context));
    aa_flush(context);
    aa_close(context);
    return 0;
}
