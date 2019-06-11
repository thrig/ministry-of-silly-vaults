/* a minimal terminal C shim for Common LISP via CFFI */

#include <sys/ioctl.h>
#include <sys/types.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>

struct termios orig, raw;

void rawterm(void)
{
    tcgetattr(STDIN_FILENO, &orig);
    raw = orig;
    cfmakeraw(&raw);
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
    /* SBCL buffers so must use FINISH-OUTPUT over there */
    //setvbuf(stdout, (char *) NULL, _IONBF, (size_t) 0);
}

int readkey(void)
{
    int k;
    read(STDIN_FILENO, &k, (size_t) 1);
    return k;
}

void restore(void)
{
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig);
}

void termsize(struct winsize *ws)
{
    ioctl(STDOUT_FILENO, TIOCGWINSZ, ws);
}
