#ifdef _WIN32

#include <stdio.h>
#include <fcntl.h>
#include <io.h>

#include "terminal.h"

void init_terminal() {
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
    _setmode(_fileno(stderr), _O_BINARY);
}

#else // _WIN32

#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

#include "terminal.h"

struct termios orig_attr;

void disableRawMode() {
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_attr);
}

void init_terminal() {
    tcgetattr(STDIN_FILENO, &orig_attr);
    atexit(&disableRawMode);

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
