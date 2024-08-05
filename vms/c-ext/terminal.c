#include <stdlib.h>
#include <stdio.h>

#ifdef _WIN32
#   include <fcntl.h>
#   include <io.h>
#else
#   include <termios.h>
#   include <unistd.h>
#   include <poll.h>
#   include <errno.h>
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

void init_terminal(bool extended) {
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
    _setmode(_fileno(stderr), _O_BINARY);

    if (extended) {
        atexit(&restore_terminal);
    }
}

int read_sync(void) {
    # TODO sync/async read on Windows
}

int read_async(void) {
    # TODO sync/async read on Windows
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

void init_terminal(bool extended) {
    if (extended) {
        tcgetattr(STDIN_FILENO, &orig_attr);
        atexit(&restore_terminal);

        struct termios attr = orig_attr;
        attr.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
        attr.c_oflag &= ~(OPOST);
        attr.c_cflag |= (CS8);
        attr.c_lflag &= ~(ECHO | ICANON | IEXTEN); // keep ISIG for Ctrl+C, Ctrl+Z

        tcsetattr(STDIN_FILENO, TCSAFLUSH, &attr);
    }
}

int read_sync(void) {
    int ch = getc(stdin);
    return ch == EOF ? READ_EOF : ch;
}

int read_async(void) {
    // is there data to read?
    struct pollfd fd = {};
    fd.fd = STDIN_FILENO;
    fd.events = POLLIN;

    int res = poll(&fd, 1, 0);
    if (res < 0) {
        fprintf(stderr, "error while waiting for input: %i", errno);
        exit(1);
    }

    if (res == 0 || (fd.revents & POLLIN) == 0 ) {
        if ((fd.revents & POLLHUP) != 0) {
            // end of input
            return READ_EOF;
        } else {
            // no data to read
            return READ_NO_DATA;
        }
    }

    // read the data
    char ch = -1;
    ssize_t size = read(STDIN_FILENO, &ch, 1);
    if (size < 0) {
        fprintf(stderr, "error while reading input: %i", errno);
        exit(1);
    }

    if (size == 0) {
        return READ_EOF;
    }

    return ch;
}

#endif // _WIN32
