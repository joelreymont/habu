#include "habu.h"
#include "object.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

habu_value_t REPL_LOOP();
habu_value_t REPL_LOOP_BODY();

habu_value_t REPL_LOOP() {
    return ({
  habu_print_value(habu_make_string("Habu REPL - Written in Lisp!", 28));
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Press Ctrl-D to exit", 20));
  ({printf("\n"); NIL;});
  ({printf("\n"); NIL;});
  REPL_LOOP_BODY();
});
}

habu_value_t REPL_LOOP_BODY() {
    return ({
  habu_print_value(habu_make_string("habu> ", 6));
  ({
      habu_value_t LINE = (habu_value_t)habu_fgets_line();
      (is_nil(LINE) ? ({
  ({printf("\n"); NIL;});
  habu_print_value(habu_make_string("Bye!", 4));
  ({printf("\n"); NIL;});
}) : ({
  (is_nil((value_to_fixnum(fixnum_to_value((char*)LINE ? strlen((char*)LINE) : 0)) > value_to_fixnum(fixnum_to_value(0)) ? fixnum_to_value(1) : NIL)) ? NIL : ({
          habu_value_t INPUT_STR = ({char* s = (char*)LINE; s ? habu_make_string(s, strlen(s)) : NIL;});
          ({
            habu_value_t EXPR = habu_read_from_string(INPUT_STR);
            ({
              habu_value_t RESULT = habu_eval(EXPR);
              ({
  habu_print_value(RESULT);
  ({printf("\n"); NIL;});
});
              });
            });
          }));
  REPL_LOOP_BODY();
}));
      });
});
}

int main(void) {
    habu_init(4 * 1024 * 1024);
    
    habu_value_t result = REPL_LOOP();
    
    if (is_fixnum(result)) {
        printf("Result: %lld\n", (long long)value_to_fixnum(result));
    } else {
        printf("Result: 0x%llx (tagged pointer)\n", (unsigned long long)result);
    }
    
    habu_shutdown();
    return 0;
}
