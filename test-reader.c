#include "habu.h"
#include "object.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

habu_value_t TEST_READER();

habu_value_t TEST_READER() {
    return ({
    habu_value_t INPUT = habu_make_string("42", 2);
    ({
      habu_value_t RESULT = READ_FROM_STRING(INPUT);
      habu_print_value(RESULT);
      });
    });
}

int main(void) {
    habu_init(4 * 1024 * 1024);
    
    habu_value_t result = TEST_READER();
    
    if (is_fixnum(result)) {
        printf("Result: %lld\n", (long long)value_to_fixnum(result));
    } else {
        printf("Result: 0x%llx (tagged pointer)\n", (unsigned long long)result);
    }
    
    habu_shutdown();
    return 0;
}
