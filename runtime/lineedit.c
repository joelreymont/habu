/*
 * Line editing with readline-style features
 * - Arrow keys for cursor movement
 * - Ctrl-A: beginning of line
 * - Ctrl-E: end of line
 * - Backspace/Delete
 * - Arrow up/down for history (future)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <ctype.h>

#define MAX_LINE 1024
#define MAX_HISTORY 100

static struct termios orig_termios;
static int raw_mode_enabled = 0;

/* Terminal raw mode */
static void disable_raw_mode(void) {
    if (raw_mode_enabled) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
        raw_mode_enabled = 0;
    }
}

static int enable_raw_mode(void) {
    if (!isatty(STDIN_FILENO)) return -1;

    if (tcgetattr(STDIN_FILENO, &orig_termios) == -1) return -1;
    atexit(disable_raw_mode);

    struct termios raw = orig_termios;
    raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
    raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_cc[VMIN] = 1;
    raw.c_cc[VTIME] = 0;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) return -1;
    raw_mode_enabled = 1;
    return 0;
}

/* Line editor state */
typedef struct {
    char buf[MAX_LINE];
    int len;
    int pos;
} line_state;

static void refresh_line(const char *prompt, line_state *ls) {
    /* Move cursor to beginning, clear line, print prompt and line */
    char seq[64];

    /* Clear line */
    snprintf(seq, 64, "\r\x1b[K");
    write(STDOUT_FILENO, seq, strlen(seq));

    /* Print prompt and current line */
    write(STDOUT_FILENO, prompt, strlen(prompt));
    write(STDOUT_FILENO, ls->buf, ls->len);

    /* Move cursor to position */
    if (ls->pos != ls->len) {
        snprintf(seq, 64, "\r\x1b[%dC", (int)(strlen(prompt) + ls->pos));
        write(STDOUT_FILENO, seq, strlen(seq));
    }
}

static void move_left(line_state *ls) {
    if (ls->pos > 0) {
        ls->pos--;
    }
}

static void move_right(line_state *ls) {
    if (ls->pos < ls->len) {
        ls->pos++;
    }
}

static void move_home(line_state *ls) {
    ls->pos = 0;
}

static void move_end(line_state *ls) {
    ls->pos = ls->len;
}

static void insert_char(line_state *ls, char c) {
    if (ls->len < MAX_LINE - 1) {
        if (ls->pos < ls->len) {
            /* Insert in middle - shift right */
            memmove(ls->buf + ls->pos + 1, ls->buf + ls->pos, ls->len - ls->pos);
        }
        ls->buf[ls->pos] = c;
        ls->pos++;
        ls->len++;
        ls->buf[ls->len] = '\0';
    }
}

static void delete_char(line_state *ls) {
    if (ls->pos > 0 && ls->len > 0) {
        memmove(ls->buf + ls->pos - 1, ls->buf + ls->pos, ls->len - ls->pos);
        ls->pos--;
        ls->len--;
        ls->buf[ls->len] = '\0';
    }
}

/* Read a line with editing */
char* lineedit_readline(const char *prompt) {
    if (!raw_mode_enabled) {
        if (enable_raw_mode() == -1) {
            /* Fallback to simple fgets */
            static char fallback_buf[MAX_LINE];
            printf("%s", prompt);
            fflush(stdout);
            if (fgets(fallback_buf, MAX_LINE, stdin) == NULL) {
                return NULL;
            }
            /* Remove newline */
            size_t len = strlen(fallback_buf);
            if (len > 0 && fallback_buf[len-1] == '\n') {
                fallback_buf[len-1] = '\0';
            }
            return fallback_buf;
        }
    }

    line_state ls;
    memset(&ls, 0, sizeof(ls));

    /* Print prompt */
    write(STDOUT_FILENO, prompt, strlen(prompt));

    while (1) {
        char c;
        int nread = read(STDIN_FILENO, &c, 1);
        if (nread <= 0) {
            return NULL;
        }

        if (c == 13) { /* Enter */
            write(STDOUT_FILENO, "\r\n", 2);
            static char result[MAX_LINE];
            memcpy(result, ls.buf, ls.len);
            result[ls.len] = '\0';
            return result;
        }

        if (c == 127 || c == 8) { /* Backspace/Delete */
            delete_char(&ls);
            refresh_line(prompt, &ls);
            continue;
        }

        if (c == 27) { /* Escape sequence */
            char seq[3];
            if (read(STDIN_FILENO, &seq[0], 1) != 1) continue;
            if (read(STDIN_FILENO, &seq[1], 1) != 1) continue;

            if (seq[0] == '[') {
                if (seq[1] >= '0' && seq[1] <= '9') {
                    /* Extended escape */
                    if (read(STDIN_FILENO, &seq[2], 1) != 1) continue;
                    if (seq[1] == '3' && seq[2] == '~') {
                        /* Delete key */
                        if (ls.pos < ls.len) {
                            memmove(ls.buf + ls.pos, ls.buf + ls.pos + 1, ls.len - ls.pos - 1);
                            ls.len--;
                            ls.buf[ls.len] = '\0';
                        }
                    }
                } else {
                    switch (seq[1]) {
                        case 'A': /* Up arrow - history (TODO) */
                            break;
                        case 'B': /* Down arrow - history (TODO) */
                            break;
                        case 'C': /* Right arrow */
                            move_right(&ls);
                            break;
                        case 'D': /* Left arrow */
                            move_left(&ls);
                            break;
                        case 'H': /* Home */
                            move_home(&ls);
                            break;
                        case 'F': /* End */
                            move_end(&ls);
                            break;
                    }
                }
                refresh_line(prompt, &ls);
            }
            continue;
        }

        if (c == 1) { /* Ctrl-A */
            move_home(&ls);
            refresh_line(prompt, &ls);
            continue;
        }

        if (c == 5) { /* Ctrl-E */
            move_end(&ls);
            refresh_line(prompt, &ls);
            continue;
        }

        if (c == 4) { /* Ctrl-D */
            if (ls.len == 0) {
                return NULL;
            }
            continue;
        }

        if (c >= 32 && c < 127) { /* Printable character */
            insert_char(&ls, c);
            refresh_line(prompt, &ls);
        }
    }
}
