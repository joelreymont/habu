#include "habu.h"
#include "object.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

habu_value_t TEST_NUM();

habu_value_t TEST_NUM() {
    return ({
    habu_value_t STR = ({char* s = (char*)habu_make_string("42", 2); s ? habu_make_string(s, strlen(s)) : NIL;});
    ({
      habu_value_t LEN = fixnum_to_value(habu_string_length_raw(STR));
      ({
  habu_print_value(habu_make_string("Length: ", 8));
  habu_print_value(LEN);
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Char 0: ", 8));
  habu_print_value(habu_string_ref(STR, value_to_fixnum(fixnum_to_value(0))));
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Char 1: ", 8));
  habu_print_value(habu_string_ref(STR, value_to_fixnum(fixnum_to_value(1))));
  ({printf("\n"); NIL;});
});
      });
    });
}

int main(void) {
    habu_init(4 * 1024 * 1024);
    
    habu_value_t result = TEST_NUM();
    
    if (is_fixnum(result)) {
        printf("Result: %lld\n", (long long)value_to_fixnum(result));
    } else {
        printf("Result: 0x%llx (tagged pointer)\n", (unsigned long long)result);
    }
    
    habu_shutdown();
    return 0;
}
