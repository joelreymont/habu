#include "habu.h"
#include "object.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Simple REPL for testing */
int main(void) {
    habu_init(4 * 1024 * 1024);

    printf("Habu REPL - Press Ctrl-D or Ctrl-C to exit\n");
    printf("Features: Arrow keys, Ctrl-A (home), Ctrl-E (end), History (up/down)\n\n");

    while (1) {
        char *line = lineedit_readline("habu> ");

        if (line == NULL) {
            printf("\nBye!\n");
            break;
        }

        if (strlen(line) == 0) {
            free(line);
            continue;
        }

        /* Try to read and evaluate */
        habu_value_t str = habu_make_string(line, strlen(line));
        habu_value_t expr = habu_read_from_string(str);

        /* Evaluate the expression */
        habu_value_t result = habu_eval(expr);

        /* Print result */
        printf("=> ");
        habu_println_value(result);

        free(line);
    }

    lineedit_cleanup();
    habu_shutdown();
    return 0;
}
