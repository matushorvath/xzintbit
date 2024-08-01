#include <stdlib.h>
#include <stdio.h>

#ifdef _WIN32
#   include <fcntl.h>
#   include <io.h>
#else
#   include <termios.h>
#   include <unistd.h>
#endif // _WIN32

#include "terminal.h"

#define TERM_SHOW_CURSOR    "\x1b[?25h"
#define TERM_RESET_ATTR     "\x1b[0m"

#define STRLEN(s) (sizeof(s) / sizeof(s[0]) - 1)

#ifdef _WIN32

void restore_terminal(void) {
    if (_isatty(_fileno(stdout))) {
        write(_fileno(stdout), TERM_SHOW_CURSOR, STRLEN(TERM_SHOW_CURSOR));
        write(_fileno(stdout), TERM_RESET_ATTR, STRLEN(TERM_RESET_ATTR));
    }
}

void init_terminal(void) {
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
    _setmode(_fileno(stderr), _O_BINARY);

    atexit(&restore_terminal);
}

#else // _WIN32

struct termios orig_attr;

void restore_terminal(void) {
    if (isatty(STDOUT_FILENO)) {
        (void)!write(STDOUT_FILENO, TERM_SHOW_CURSOR, STRLEN(TERM_SHOW_CURSOR));
        (void)!write(STDOUT_FILENO, TERM_RESET_ATTR, STRLEN(TERM_RESET_ATTR));
    }

    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_attr);
}

void init_terminal(void) {
    tcgetattr(STDIN_FILENO, &orig_attr);
    atexit(&restore_terminal);

    struct termios attr = orig_attr;
    attr.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    attr.c_oflag &= ~(OPOST);
    attr.c_cflag |= (CS8);
    attr.c_lflag &= ~(ECHO | ICANON | IEXTEN); // keep ISIG for Ctrl+C, Ctrl+Z
    attr.c_cc[VMIN] = 0;
    attr.c_cc[VTIME] = 1;

    tcsetattr(STDIN_FILENO, TCSAFLUSH, &attr);
}

#endif // _WIN32
