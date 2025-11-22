/* Minimal runtime implementation for bootstrap testing */

#include <string.h>
#include "habu-minimal.h"

/* Simple cons cell allocation */
typedef struct cons_cell {
    habu_value_t car;
    habu_value_t cdr;
} cons_cell_t;

/* Ensure 16-byte alignment so low 4 bits are clear */
static cons_cell_t cells[10000] __attribute__((aligned(16)));
static int cell_count = 0;

habu_value_t habu_cons(habu_value_t car, habu_value_t cdr) {
    if (cell_count >= 10000) return HABU_NIL;
    cells[cell_count].car = car;
    cells[cell_count].cdr = cdr;
    habu_value_t ptr = (habu_value_t)&cells[cell_count];
    cell_count++;
    /* Tag as cons - just OR with 1, don't clear bits */
    /* The array should be naturally aligned */
    return ptr | 1;
}

habu_value_t habu_car(habu_value_t cons) {
    if (!HABU_IS_CONS(cons)) return HABU_NIL;
    cons_cell_t *cell = (cons_cell_t *)(cons & ~0xFLL);
    return cell->car;
}

habu_value_t habu_cdr(habu_value_t cons) {
    if (!HABU_IS_CONS(cons)) return HABU_NIL;
    cons_cell_t *cell = (cons_cell_t *)(cons & ~0xFLL);
    return cell->cdr;
}

/* Simple symbol table */
typedef struct symbol {
    const char *name;
    habu_value_t value;
} symbol_t;

/* Ensure 16-byte alignment so low 4 bits are clear */
static symbol_t symbols[1000] __attribute__((aligned(16)));
static int symbol_count = 0;

/* Simple string pool for symbol names */
static char string_pool[100000];
static int string_pool_pos = 0;

habu_value_t habu_intern(const char *name) {
    /* Look up existing symbol */
    for (int i = 0; i < symbol_count; i++) {
        if (strcmp(symbols[i].name, name) == 0) {
            return symbols[i].value;
        }
    }

    /* Create new symbol */
    if (symbol_count >= 1000) return HABU_NIL;

    /* Copy string to pool */
    int len = strlen(name);
    if (string_pool_pos + len + 1 > 100000) return HABU_NIL;
    char *name_copy = &string_pool[string_pool_pos];
    strcpy(name_copy, name);
    string_pool_pos += len + 1;

    symbols[symbol_count].name = name_copy;
    /* Tag as symbol - just OR with 2, don't clear bits */
    symbols[symbol_count].value = ((habu_value_t)(&symbols[symbol_count])) | 2;
    symbol_count++;
    return symbols[symbol_count - 1].value;
}
