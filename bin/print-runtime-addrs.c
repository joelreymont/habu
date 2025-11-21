#include <stdio.h>
#include "../runtime/habu.h"

int main(void) {
    printf("HABU_CONS_ADDR=0x%lx\n", (unsigned long)(void*)habu_cons);
    printf("HABU_CAR_ADDR=0x%lx\n", (unsigned long)(void*)habu_car);
    printf("HABU_CDR_ADDR=0x%lx\n", (unsigned long)(void*)habu_cdr);
    return 0;
}
