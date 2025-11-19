#include "habu.h"
#include "object.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(void) {
    habu_init(4 * 1024 * 1024);
    
    habu_value_t result = ({
habu_value_t INPUT = ({char* s = (char*)habu_make_string("42", 2); s ? habu_make_string(s, strlen(s)) : NIL;});
habu_print_value(habu_make_string("Input string created", 20));
});
    
    if (is_fixnum(result)) {
        printf("Result: %lld\n", (long long)value_to_fixnum(result));
    } else {
        printf("Result: 0x%llx (tagged pointer)\n", (unsigned long long)result);
    }
    
    habu_shutdown();
    return 0;
}
