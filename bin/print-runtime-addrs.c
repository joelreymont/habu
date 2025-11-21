#include <stdio.h>
#include "../runtime/habu.h"

int main(void) {
    /* Memory allocation */
    printf("HABU_CONS_ADDR=0x%lx\n", (unsigned long)(void*)habu_cons);
    printf("HABU_MAKE_VECTOR_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_vector);
    printf("HABU_MAKE_STRING_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_string);
    printf("HABU_MAKE_SYMBOL_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_symbol);

    /* List accessors */
    printf("HABU_CAR_ADDR=0x%lx\n", (unsigned long)(void*)habu_car);
    printf("HABU_CDR_ADDR=0x%lx\n", (unsigned long)(void*)habu_cdr);
    printf("HABU_SET_CAR_ADDR=0x%lx\n", (unsigned long)(void*)habu_set_car);
    printf("HABU_SET_CDR_ADDR=0x%lx\n", (unsigned long)(void*)habu_set_cdr);

    /* Vector operations */
    printf("HABU_VECTOR_REF_ADDR=0x%lx\n", (unsigned long)(void*)habu_vector_ref);
    printf("HABU_VECTOR_SET_ADDR=0x%lx\n", (unsigned long)(void*)habu_vector_set);

    /* String operations */
    printf("HABU_STRING_REF_ADDR=0x%lx\n", (unsigned long)(void*)habu_string_ref);
    printf("HABU_STRING_LENGTH_RAW_ADDR=0x%lx\n", (unsigned long)(void*)habu_string_length_raw);
    printf("HABU_STRING_CONCAT_ADDR=0x%lx\n", (unsigned long)(void*)habu_string_concat);
    printf("HABU_STRING_SUBSTRING_ADDR=0x%lx\n", (unsigned long)(void*)habu_string_substring);
    printf("HABU_FIXNUM_TO_STRING_ADDR=0x%lx\n", (unsigned long)(void*)habu_fixnum_to_string);
    printf("HABU_MAKE_STRING_FROM_VECTOR_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_string_from_vector);

    /* Symbol operations */
    printf("HABU_MAKE_SYMBOL_FROM_STRING_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_symbol_from_string);
    printf("HABU_SYMBOL_NAME_ADDR=0x%lx\n", (unsigned long)(void*)habu_symbol_name);

    /* Closure operations */
    printf("HABU_MAKE_CLOSURE_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_closure);
    printf("HABU_CLOSURE_CODE_ADDR=0x%lx\n", (unsigned long)(void*)habu_closure_code);
    printf("HABU_CLOSURE_ENV_ADDR=0x%lx\n", (unsigned long)(void*)habu_closure_env);

    /* Type operations */
    printf("HABU_GET_TAG_ADDR=0x%lx\n", (unsigned long)(void*)habu_get_tag);

    /* I/O operations */
    printf("HABU_PRINT_ADDR=0x%lx\n", (unsigned long)(void*)habu_print);
    printf("HABU_WRITE_BYTE_ADDR=0x%lx\n", (unsigned long)(void*)habu_write_byte);
    printf("HABU_READ_BYTE_ADDR=0x%lx\n", (unsigned long)(void*)habu_read_byte);
    printf("HABU_FGETS_LINE_ADDR=0x%lx\n", (unsigned long)(void*)habu_fgets_line);

    return 0;
}
