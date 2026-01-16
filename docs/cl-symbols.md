# ANSI Common Lisp Symbol Audit

978 external symbols in COMMON-LISP package per ANSI INCITS 226-1994.

Status: ✓ implemented | ⚠ partial | ✗ missing

## Lambda List Keywords (8)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| &allow-other-keys | ⚠ | compile.zig | Parsed but not fully enforced |
| &aux | ✗ | - | Not implemented |
| &body | ✓ | compile.zig:427 | Macro lambda lists |
| &environment | ✗ | - | Not implemented |
| &key | ✓ | compile.zig:429 | Keyword args |
| &optional | ✓ | compile.zig:428 | Optional args |
| &rest | ✓ | compile.zig:426 | Rest args |
| &whole | ✗ | - | Not implemented |

## Special Variables (65)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| * | ✓ | repl.zig | Previous result |
| ** | ✓ | repl.zig | 2nd previous result |
| *** | ✓ | repl.zig | 3rd previous result |
| *break-on-signals* | ✗ | - | |
| *compile-file-pathname* | ✗ | - | |
| *compile-file-truename* | ✗ | - | |
| *compile-print* | ✗ | - | |
| *compile-verbose* | ✗ | - | |
| *debug-io* | ✗ | - | |
| *debugger-hook* | ✗ | - | |
| *default-pathname-defaults* | ✗ | - | |
| *error-output* | ✗ | - | |
| *features* | ✓ | compile.zig | Reader conditionals |
| *gensym-counter* | ✓ | vm.zig | Gensym support |
| *load-pathname* | ✗ | - | |
| *load-print* | ✗ | - | |
| *load-truename* | ✗ | - | |
| *load-verbose* | ✗ | - | |
| *macroexpand-hook* | ✗ | - | |
| *modules* | ✗ | - | |
| *package* | ✓ | heap.zig | Current package |
| *print-array* | ✗ | - | |
| *print-base* | ✗ | - | |
| *print-case* | ✗ | - | |
| *print-circle* | ✗ | - | |
| *print-escape* | ✗ | - | |
| *print-gensym* | ✗ | - | |
| *print-length* | ✗ | - | |
| *print-level* | ✗ | - | |
| *print-lines* | ✗ | - | |
| *print-miser-width* | ✗ | - | |
| *print-pprint-dispatch* | ✗ | - | |
| *print-pretty* | ✗ | - | |
| *print-radix* | ✗ | - | |
| *print-readably* | ✗ | - | |
| *print-right-margin* | ✗ | - | |
| *query-io* | ✗ | - | |
| *random-state* | ✗ | - | Using seed fn |
| *read-base* | ✗ | - | |
| *read-default-float-format* | ✗ | - | |
| *read-eval* | ✗ | - | #. disabled |
| *read-suppress* | ✗ | - | |
| *readtable* | ✓ | vm.zig | Runtime readtable |
| *standard-input* | ✓ | vm.zig | stdin stream |
| *standard-output* | ✓ | vm.zig | stdout stream |
| *terminal-io* | ✗ | - | |
| *trace-output* | ✗ | - | |
| + | ✓ | repl.zig | Previous form |
| ++ | ✓ | repl.zig | 2nd previous form |
| +++ | ✓ | repl.zig | 3rd previous form |
| - | ✓ | repl.zig | Current form |
| / | ✓ | repl.zig | Previous values |
| // | ✓ | repl.zig | 2nd previous values |
| /// | ✓ | repl.zig | 3rd previous values |

## Constants (43)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| array-dimension-limit | ✗ | - | |
| array-rank-limit | ✗ | - | |
| array-total-size-limit | ✗ | - | |
| boole-1 | ✗ | - | |
| boole-2 | ✗ | - | |
| boole-and | ✗ | - | |
| boole-andc1 | ✗ | - | |
| boole-andc2 | ✗ | - | |
| boole-c1 | ✗ | - | |
| boole-c2 | ✗ | - | |
| boole-clr | ✗ | - | |
| boole-eqv | ✗ | - | |
| boole-ior | ✗ | - | |
| boole-nand | ✗ | - | |
| boole-nor | ✗ | - | |
| boole-orc1 | ✗ | - | |
| boole-orc2 | ✗ | - | |
| boole-set | ✗ | - | |
| boole-xor | ✗ | - | |
| call-arguments-limit | ✗ | - | |
| char-code-limit | ✗ | - | |
| double-float-epsilon | ✗ | - | |
| double-float-negative-epsilon | ✗ | - | |
| internal-time-units-per-second | ✗ | - | |
| lambda-list-keywords | ✗ | - | |
| lambda-parameters-limit | ✗ | - | |
| least-negative-double-float | ✗ | - | |
| least-negative-long-float | ✗ | - | |
| least-negative-normalized-double-float | ✗ | - | |
| least-negative-normalized-long-float | ✗ | - | |
| least-negative-normalized-short-float | ✗ | - | |
| least-negative-normalized-single-float | ✗ | - | |
| least-negative-short-float | ✗ | - | |
| least-negative-single-float | ✗ | - | |
| least-positive-double-float | ✗ | - | |
| least-positive-long-float | ✗ | - | |
| least-positive-normalized-double-float | ✗ | - | |
| least-positive-normalized-long-float | ✗ | - | |
| least-positive-normalized-short-float | ✗ | - | |
| least-positive-normalized-single-float | ✗ | - | |
| least-positive-short-float | ✗ | - | |
| least-positive-single-float | ✗ | - | |
| long-float-epsilon | ✗ | - | |
| long-float-negative-epsilon | ✗ | - | |
| most-negative-double-float | ✗ | - | |
| most-negative-fixnum | ✗ | - | |
| most-negative-long-float | ✗ | - | |
| most-negative-short-float | ✗ | - | |
| most-negative-single-float | ✗ | - | |
| most-positive-double-float | ✗ | - | |
| most-positive-fixnum | ✗ | - | |
| most-positive-long-float | ✗ | - | |
| most-positive-short-float | ✗ | - | |
| most-positive-single-float | ✗ | - | |
| multiple-values-limit | ✗ | - | |
| nil | ✓ | value.zig | Constant |
| pi | ✓ | arith.zig | 3.14159... |
| short-float-epsilon | ✗ | - | |
| short-float-negative-epsilon | ✗ | - | |
| single-float-epsilon | ✗ | - | |
| single-float-negative-epsilon | ✗ | - | |
| t | ✓ | value.zig | True constant |

## Special Operators (25)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| block | ✓ | compile.zig:99 | Named exit |
| catch | ✓ | compile.zig:103 | Dynamic catch |
| eval-when | ✓ | compile.zig:136 | Compile-time control |
| flet | ✓ | compile.zig:64 | Local functions |
| function | ✓ | compile.zig:82 | Function reference |
| go | ✓ | compile.zig:112 | Tagbody jump |
| if | ✓ | compile.zig:54 | Conditional |
| labels | ✓ | compile.zig:65 | Recursive local fns |
| let | ✓ | compile.zig:60 | Parallel binding |
| let* | ✓ | compile.zig:61 | Sequential binding |
| load-time-value | ✗ | - | |
| locally | ✗ | - | |
| macrolet | ✗ | - | |
| multiple-value-call | ✓ | compile.zig:116 | MV function call |
| multiple-value-prog1 | ✗ | - | |
| progn | ✓ | compile.zig:73 | Sequencing |
| progv | ✓ | vm.zig | Dynamic binding |
| quote | ✓ | compile.zig:78 | Literal data |
| return-from | ✓ | compile.zig:100 | Block exit |
| setq | ✓ | compile.zig:70 | Variable assignment |
| symbol-macrolet | ✗ | - | |
| tagbody | ✓ | compile.zig:111 | Go targets |
| the | ✓ | compile.zig:96 | Type assertion |
| throw | ✓ | compile.zig:104 | Dynamic throw |
| unwind-protect | ✓ | compile.zig:101 | Cleanup forms |

## Macros (86)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| and | ✓ | compile.zig:56 | Short-circuit and |
| assert | ✓ | stdlib.habu | Assertion |
| case | ✓ | stdlib.habu:26 | Symbol dispatch |
| ccase | ✓ | stdlib.habu | Continuable case |
| check-type | ✓ | stdlib.habu | Type check |
| cond | ✓ | compile.zig:55 | Multi-branch cond |
| ctypecase | ✓ | stdlib.habu | Continuable typecase |
| decf | ✓ | stdlib.habu | Decrement place |
| declaim | ✗ | - | Global declaration |
| defclass | ✓ | compile.zig:129 | CLOS class |
| defconstant | ✗ | - | Use defvar |
| defgeneric | ✓ | compile.zig:132 | Generic function |
| define-compiler-macro | ✗ | - | |
| define-condition | ✓ | stdlib.habu | Condition class |
| define-method-combination | ✗ | - | |
| define-modify-macro | ✗ | - | |
| define-setf-expander | ✗ | - | |
| define-symbol-macro | ✗ | - | |
| defmacro | ✓ | compile.zig:89 | Macro definition |
| defmethod | ✓ | compile.zig:133 | CLOS method |
| defpackage | ✓ | compile.zig:120 | Package definition |
| defparameter | ✗ | - | Use defvar |
| defsetf | ✗ | - | |
| defstruct | ✓ | stdlib.habu:2739 | Structure |
| deftype | ✓ | compile.zig | Type definition |
| defun | ✓ | compile.zig:69 | Function definition |
| defvar | ✓ | compile.zig:68 | Variable definition |
| destructuring-bind | ✓ | stdlib.habu:504 | Pattern destructure |
| do | ✓ | stdlib.habu:61 | Parallel iteration |
| do* | ✓ | stdlib.habu:113 | Sequential iteration |
| do-all-symbols | ✗ | - | |
| do-external-symbols | ✗ | - | |
| do-symbols | ✗ | - | |
| dolist | ✓ | stdlib.habu | List iteration |
| dotimes | ✓ | stdlib.habu | Count iteration |
| ecase | ✓ | stdlib.habu | Error case |
| etypecase | ✓ | stdlib.habu | Error typecase |
| formatter | ✗ | - | |
| handler-bind | ✓ | compile.zig:107 | Handler binding |
| handler-case | ✓ | compile.zig:104 | Handler dispatch |
| ignore-errors | ✓ | stdlib.habu:55 | Error suppression |
| in-package | ✓ | compile.zig:121 | Package switch |
| incf | ✓ | stdlib.habu | Increment place |
| lambda | ✓ | compile.zig:65 | Anonymous function |
| loop | ✓ | stdlib.habu:2536 | Loop facility |
| loop-finish | ⚠ | stdlib.habu | In loop impl |
| multiple-value-bind | ✓ | compile.zig:115 | MV binding |
| multiple-value-list | ✓ | compile.zig:117 | MV to list |
| multiple-value-setq | ✓ | stdlib.habu | MV assignment |
| nth-value | ✓ | stdlib.habu | Nth value select |
| or | ✓ | compile.zig:57 | Short-circuit or |
| pop | ✓ | stdlib.habu | List pop |
| pprint-exit-if-list-exhausted | ✗ | - | |
| pprint-logical-block | ✗ | - | |
| pprint-pop | ✗ | - | |
| print-unreadable-object | ✗ | - | |
| prog | ✓ | stdlib.habu | Prog form |
| prog* | ✗ | - | |
| prog1 | ✓ | stdlib.habu | Return first |
| prog2 | ✓ | stdlib.habu | Return second |
| psetf | ✓ | stdlib.habu | Parallel setf |
| psetq | ✓ | stdlib.habu | Parallel setq |
| push | ✓ | stdlib.habu | List push |
| pushnew | ✓ | stdlib.habu | Conditional push |
| remf | ✓ | stdlib.habu | Remove property |
| restart-bind | ✗ | - | |
| restart-case | ✓ | compile.zig:108 | Restart establish |
| return | ✓ | stdlib.habu | Block return |
| rotatef | ✓ | stdlib.habu | Rotate places |
| setf | ✓ | stdlib.habu:878 | Generalized set |
| shiftf | ✓ | stdlib.habu | Shift places |
| step | ✗ | - | |
| time | ✗ | - | |
| trace | ✗ | - | |
| typecase | ✓ | stdlib.habu | Type dispatch |
| unless | ✓ | stdlib.habu | Negated when |
| untrace | ✗ | - | |
| when | ✓ | stdlib.habu | Conditional |
| with-accessors | ✗ | - | |
| with-compilation-unit | ✗ | - | |
| with-condition-restarts | ✗ | - | |
| with-hash-table-iterator | ✓ | stdlib.habu | Hash iteration |
| with-input-from-string | ✓ | stdlib.habu | String input |
| with-open-file | ✓ | stdlib.habu | File scope |
| with-open-stream | ✗ | - | |
| with-output-to-string | ✓ | stdlib.habu | String output |
| with-package-iterator | ✗ | - | |
| with-simple-restart | ✗ | - | |
| with-slots | ✗ | - | |
| with-standard-io-syntax | ✗ | - | |

## Functions - Arithmetic (79)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| * | ✓ | compile.zig:141 | Multiply |
| + | ✓ | compile.zig:139 | Add |
| - | ✓ | compile.zig:140 | Subtract |
| / | ✓ | compile.zig:142 | Divide |
| /= | ✓ | arith.zig | Not equal |
| 1+ | ✓ | stdlib.habu:37 | Increment |
| 1- | ✓ | stdlib.habu:39 | Decrement |
| < | ✓ | compile.zig:153 | Less than |
| <= | ✓ | compile.zig:155 | Less or equal |
| = | ✓ | compile.zig:157 | Numeric equal |
| > | ✓ | compile.zig:154 | Greater than |
| >= | ✓ | compile.zig:156 | Greater or equal |
| abs | ✓ | compile.zig:248 | Absolute value |
| acos | ✓ | compile.zig:261 | Arc cosine |
| acosh | ✓ | compile.zig:267 | Hyperbolic arc cos |
| arithmetic-error-operands | ✗ | - | |
| arithmetic-error-operation | ✗ | - | |
| ash | ✓ | compile.zig:343 | Arithmetic shift |
| asin | ✓ | compile.zig:260 | Arc sine |
| asinh | ✓ | compile.zig:266 | Hyperbolic arc sin |
| atan | ✓ | compile.zig:262 | Arc tangent |
| atanh | ✓ | compile.zig:268 | Hyperbolic arc tan |
| boole | ✗ | - | |
| byte | ✗ | - | |
| byte-position | ✗ | - | |
| byte-size | ✗ | - | |
| ceiling | ✓ | compile.zig:272 | Ceiling |
| cis | ✓ | stdlib.habu:1765 | cos+i*sin |
| complex | ✓ | compile.zig:380 | Make complex |
| conjugate | ✗ | - | |
| cos | ✓ | compile.zig:258 | Cosine |
| cosh | ✓ | compile.zig:264 | Hyperbolic cos |
| decode-float | ✗ | - | |
| denominator | ✓ | compile.zig:384 | Rational denom |
| deposit-field | ✗ | - | |
| dpb | ✗ | - | |
| evenp | ✓ | compile.zig:252 | Even predicate |
| exp | ✓ | compile.zig:269 | Exponential |
| expt | ✓ | stdlib.habu | Power |
| fceiling | ✗ | - | |
| ffloor | ✗ | - | |
| float | ✓ | arith.zig | To float |
| float-digits | ✗ | - | |
| float-precision | ✗ | - | |
| float-radix | ✗ | - | |
| float-sign | ✗ | - | |
| floor | ✓ | compile.zig:271 | Floor |
| fround | ✗ | - | |
| ftruncate | ✗ | - | |
| gcd | ✓ | stdlib.habu | Greatest common divisor |
| imagpart | ✓ | compile.zig:382 | Complex imaginary |
| integer-decode-float | ✗ | - | |
| integer-length | ✓ | compile.zig:351 | Bit length |
| isqrt | ✓ | stdlib.habu:63 | Integer sqrt |
| lcm | ✓ | stdlib.habu | Least common multiple |
| ldb | ✗ | - | |
| ldb-test | ✗ | - | |
| log | ✓ | compile.zig:270 | Logarithm |
| logand | ✓ | compile.zig:339 | Bitwise and |
| logandc1 | ✓ | compile.zig:346 | And complement 1 |
| logandc2 | ✓ | compile.zig:347 | And complement 2 |
| logbitp | ✓ | compile.zig:349 | Bit predicate |
| logcount | ✓ | compile.zig:350 | Bit count |
| logeqv | ✓ | compile.zig:348 | Bitwise equiv |
| logior | ✓ | compile.zig:340 | Bitwise or |
| lognand | ✓ | compile.zig:344 | Bitwise nand |
| lognor | ✓ | compile.zig:345 | Bitwise nor |
| lognot | ✓ | compile.zig:342 | Bitwise not |
| logorc1 | ✓ | arith.zig | Or complement 1 |
| logorc2 | ✓ | arith.zig | Or complement 2 |
| logtest | ✓ | arith.zig | Bit test |
| logxor | ✓ | compile.zig:341 | Bitwise xor |
| make-random-state | ✗ | - | |
| mask-field | ✗ | - | |
| max | ✓ | stdlib.habu | Maximum |
| min | ✓ | stdlib.habu | Minimum |
| minusp | ✓ | compile.zig:251 | Negative predicate |
| mod | ✓ | compile.zig:143 | Modulo |
| numerator | ✓ | compile.zig:383 | Rational numer |
| oddp | ✓ | compile.zig:253 | Odd predicate |
| parse-integer | ✓ | compile.zig:335 | String to int |
| phase | ✓ | stdlib.habu:1768 | Complex phase |
| plusp | ✓ | compile.zig:250 | Positive predicate |
| random | ✓ | compile.zig:318 | Random number |
| random-state-p | ✗ | - | |
| rational | ✓ | compile.zig:385 | To rational |
| rationalize | ✓ | compile.zig:386 | Approx rational |
| realpart | ✓ | compile.zig:381 | Complex real |
| rem | ✓ | compile.zig:148 | Remainder |
| round | ✓ | compile.zig:273 | Round |
| scale-float | ✗ | - | |
| signum | ✓ | stdlib.habu:41 | Sign |
| sin | ✓ | compile.zig:257 | Sine |
| sinh | ✓ | compile.zig:263 | Hyperbolic sin |
| sqrt | ✓ | compile.zig:256 | Square root |
| tan | ✓ | compile.zig:259 | Tangent |
| tanh | ✓ | compile.zig:265 | Hyperbolic tan |
| truncate | ✓ | compile.zig:146 | Truncate |
| upgraded-complex-part-type | ✗ | - | |
| zerop | ✓ | compile.zig:249 | Zero predicate |

## Functions - Characters (30)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| alpha-char-p | ✓ | compile.zig:326 | Alpha predicate |
| alphanumericp | ✓ | stdlib.habu | Alphanum predicate |
| both-case-p | ✓ | stdlib.habu | Case predicate |
| char-code | ✓ | compile.zig:215 | To code point |
| char-downcase | ✓ | compile.zig:324 | To lowercase |
| char-equal | ✓ | stdlib.habu | Case-insensitive = |
| char-greaterp | ✓ | stdlib.habu | Case-insensitive > |
| char-int | ✗ | - | |
| char-lessp | ✓ | stdlib.habu | Case-insensitive < |
| char-name | ✗ | - | |
| char-not-equal | ✗ | - | |
| char-not-greaterp | ✓ | stdlib.habu | Case-insensitive <= |
| char-not-lessp | ✓ | stdlib.habu | Case-insensitive >= |
| char-upcase | ✓ | compile.zig:323 | To uppercase |
| char/= | ✗ | - | |
| char< | ✓ | compile.zig:218 | Char less |
| char<= | ✗ | - | |
| char= | ✓ | compile.zig:217 | Char equal |
| char> | ✓ | compile.zig:219 | Char greater |
| char>= | ✗ | - | |
| character | ✓ | compile.zig:244 | Coerce to char |
| characterp | ✓ | compile.zig:209 | Char predicate |
| code-char | ✓ | compile.zig:216 | From code point |
| digit-char | ✗ | - | |
| digit-char-p | ✓ | compile.zig:325 | Digit predicate |
| graphic-char-p | ✗ | - | |
| lower-case-p | ✓ | stdlib.habu | Lowercase pred |
| name-char | ✗ | - | |
| standard-char-p | ✓ | stdlib.habu | Standard predicate |
| upper-case-p | ✓ | stdlib.habu | Uppercase pred |

## Functions - Conses/Lists (77)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| acons | ✓ | stdlib.habu | Add to alist |
| adjoin | ✓ | stdlib.habu | Set adjoin |
| append | ✓ | compile.zig:183 | List append |
| assoc | ✓ | compile.zig:190 | Alist lookup |
| assoc-if | ✗ | - | |
| assoc-if-not | ✗ | - | |
| atom | ✓ | compile.zig:212 | Atom predicate |
| butlast | ✓ | stdlib.habu | All but last |
| caaaar | ✗ | - | |
| caaadr | ✗ | - | |
| caaar | ✗ | - | |
| caadar | ✗ | - | |
| caaddr | ✗ | - | |
| caadr | ✗ | - | |
| caar | ✓ | compile.zig:166 | Car of car |
| cadaar | ✗ | - | |
| cadadr | ✗ | - | |
| cadar | ✓ | compile.zig:173 | Car of cdr of car |
| caddar | ✗ | - | |
| cadddr | ✓ | stdlib.habu | 4th via cdr |
| caddr | ✓ | compile.zig:174 | Third |
| cadr | ✓ | compile.zig:167 | Second |
| car | ✓ | compile.zig:161 | First of cons |
| cdaaar | ✗ | - | |
| cdaadr | ✗ | - | |
| cdaar | ✗ | - | |
| cdadar | ✗ | - | |
| cdaddr | ✗ | - | |
| cdadr | ✓ | compile.zig:176 | Cdr of cadr |
| cdar | ✓ | compile.zig:168 | Cdr of car |
| cddaar | ✗ | - | |
| cddadr | ✗ | - | |
| cddar | ✓ | compile.zig:177 | Cdr of cdar |
| cdddar | ✗ | - | |
| cddddr | ✓ | stdlib.habu | 4th cdr |
| cdddr | ✓ | compile.zig:178 | Cdr of cddr |
| cddr | ✓ | compile.zig:169 | Cdr of cdr |
| cdr | ✓ | compile.zig:162 | Rest of cons |
| cons | ✓ | compile.zig:160 | Make cons |
| consp | ✓ | compile.zig:200 | Cons predicate |
| copy-alist | ✓ | stdlib.habu | Copy alist |
| copy-list | ✓ | stdlib.habu | Copy list |
| copy-tree | ✓ | stdlib.habu | Deep copy |
| eighth | ✓ | stdlib.habu | 8th element |
| endp | ✓ | stdlib.habu | End predicate |
| fifth | ✓ | stdlib.habu | 5th element |
| first | ✓ | compile.zig:163 | First element |
| fourth | ✓ | compile.zig:182 | 4th element |
| get-properties | ✗ | - | |
| getf | ✓ | stdlib.habu | Plist lookup |
| intersection | ✓ | stdlib.habu | Set intersection |
| last | ✓ | compile.zig:188 | Last cons |
| ldiff | ✓ | stdlib.habu | List difference |
| list | ✓ | compile.zig:195 | Make list |
| list* | ✓ | stdlib.habu | Make dotted list |
| list-length | ✓ | stdlib.habu | Proper length |
| listp | ✓ | compile.zig:211 | List predicate |
| make-list | ✓ | stdlib.habu | Create list |
| mapc | ✓ | stdlib.habu:459 | Map for effect |
| mapcan | ✓ | stdlib.habu:1092 | Nconc map |
| mapcar | ✓ | stdlib.habu:456 | Map to list |
| mapcon | ✓ | stdlib.habu:1098 | Nconc cons map |
| mapl | ✗ | - | |
| maplist | ✓ | stdlib.habu:467 | Map over tails |
| member | ✓ | compile.zig:189 | List member |
| member-if | ✓ | stdlib.habu | Member by pred |
| member-if-not | ✓ | stdlib.habu | Member not pred |
| nbutlast | ✗ | - | |
| nconc | ✓ | stdlib.habu | Destructive append |
| nintersection | ✗ | - | |
| ninth | ✓ | stdlib.habu | 9th element |
| nreconc | ✓ | stdlib.habu | Destructive revappend |
| nreverse | ✓ | stdlib.habu | Destructive reverse |
| nset-difference | ✗ | - | |
| nset-exclusive-or | ✗ | - | |
| nsublis | ✗ | - | |
| nsubst | ✗ | - | |
| nsubst-if | ✗ | - | |
| nsubst-if-not | ✗ | - | |
| nth | ✓ | compile.zig:186 | Nth element |
| nthcdr | ✓ | compile.zig:187 | Nth cdr |
| null | ✓ | compile.zig:207 | Null predicate |
| nunion | ✗ | - | |
| pairlis | ✓ | stdlib.habu | Make alist |
| pop | ✓ | stdlib.habu | Pop from list |
| push | ✓ | stdlib.habu | Push to list |
| pushnew | ✓ | stdlib.habu | Conditional push |
| rassoc | ✓ | stdlib.habu | Reverse assoc |
| rassoc-if | ✓ | stdlib.habu | Reverse assoc-if |
| rassoc-if-not | ✗ | - | |
| rest | ✓ | compile.zig:164 | Rest of list |
| revappend | ✓ | stdlib.habu | Reverse and append |
| reverse | ✓ | compile.zig:185 | Reverse list |
| rplaca | ✓ | compile.zig:196 | Set car |
| rplacd | ✓ | compile.zig:197 | Set cdr |
| second | ✓ | compile.zig:180 | 2nd element |
| set-difference | ✓ | stdlib.habu | Set difference |
| set-exclusive-or | ✓ | stdlib.habu | Set xor |
| seventh | ✓ | stdlib.habu | 7th element |
| sixth | ✓ | stdlib.habu | 6th element |
| sublis | ✓ | stdlib.habu | Substitute alist |
| subsetp | ✓ | stdlib.habu | Subset predicate |
| subst | ✓ | stdlib.habu | Substitute |
| subst-if | ✓ | stdlib.habu | Substitute by pred |
| subst-if-not | ✓ | stdlib.habu | Substitute not pred |
| tailp | ✓ | stdlib.habu | Tail predicate |
| tenth | ✓ | stdlib.habu | 10th element |
| third | ✓ | compile.zig:181 | 3rd element |
| tree-equal | ✓ | stdlib.habu | Tree equality |
| union | ✓ | stdlib.habu | Set union |

## Functions - Arrays (38)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| adjust-array | ✗ | - | |
| adjustable-array-p | ✗ | - | |
| aref | ✓ | compile.zig:276 | Array element |
| array-dimension | ✗ | - | |
| array-dimensions | ✗ | - | |
| array-displacement | ✗ | - | |
| array-element-type | ✗ | - | |
| array-has-fill-pointer-p | ✗ | - | |
| array-in-bounds-p | ✗ | - | |
| array-rank | ✗ | - | |
| array-row-major-index | ✗ | - | |
| array-total-size | ✗ | - | |
| arrayp | ✓ | stdlib.habu | Array predicate |
| bit | ✗ | - | |
| bit-and | ✗ | - | |
| bit-andc1 | ✗ | - | |
| bit-andc2 | ✗ | - | |
| bit-eqv | ✗ | - | |
| bit-ior | ✗ | - | |
| bit-nand | ✗ | - | |
| bit-nor | ✗ | - | |
| bit-not | ✗ | - | |
| bit-orc1 | ✗ | - | |
| bit-orc2 | ✗ | - | |
| bit-vector-p | ✓ | stdlib.habu | Bit-vector pred |
| bit-xor | ✗ | - | |
| fill-pointer | ✗ | - | |
| make-array | ✓ | compile.zig:298 | Create array |
| row-major-aref | ✗ | - | |
| sbit | ✗ | - | |
| simple-bit-vector-p | ✗ | - | |
| simple-vector-p | ✓ | stdlib.habu | Simple-vector pred |
| svref | ✓ | compile.zig:277 | Simple-vector ref |
| upgraded-array-element-type | ✗ | - | |
| vector | ✓ | compile.zig:297 | Make vector |
| vector-pop | ✗ | - | |
| vector-push | ✗ | - | |
| vector-push-extend | ✗ | - | |
| vectorp | ✓ | compile.zig:204 | Vector predicate |

## Functions - Strings (28)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| char | ✓ | compile.zig:301 | String char |
| make-string | ✓ | compile.zig:358 | Create string |
| nstring-capitalize | ✗ | - | |
| nstring-downcase | ✗ | - | |
| nstring-upcase | ✗ | - | |
| schar | ✓ | compile.zig:302 | Simple-string char |
| simple-string-p | ✓ | stdlib.habu | Simple-string pred |
| string | ✓ | compile.zig:243 | To string |
| string-capitalize | ✓ | stdlib.habu | Capitalize |
| string-downcase | ✓ | compile.zig:362 | Downcase |
| string-equal | ✓ | stdlib.habu | Case-insensitive = |
| string-greaterp | ✓ | stdlib.habu | Case-insensitive > |
| string-left-trim | ✓ | stdlib.habu | Left trim |
| string-lessp | ✓ | stdlib.habu | Case-insensitive < |
| string-not-equal | ✓ | stdlib.habu | Case-insensitive /= |
| string-not-greaterp | ✓ | stdlib.habu | Case-insensitive <= |
| string-not-lessp | ✓ | stdlib.habu | Case-insensitive >= |
| string-right-trim | ✓ | stdlib.habu | Right trim |
| string-trim | ✓ | stdlib.habu | Both trim |
| string-upcase | ✓ | compile.zig:361 | Upcase |
| string/= | ✗ | - | Use string-not-equal |
| string< | ✓ | compile.zig:306 | String less |
| string<= | ✓ | compile.zig:308 | String less-eq |
| string= | ✓ | compile.zig:305 | String equal |
| string> | ✓ | compile.zig:307 | String greater |
| string>= | ✓ | compile.zig:309 | String greater-eq |
| stringp | ✓ | compile.zig:203 | String predicate |

## Functions - Sequences (56)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| concatenate | ✓ | compile.zig:363 | Concat sequences |
| copy-seq | ✓ | stdlib.habu | Copy sequence |
| count | ✓ | compile.zig:193 | Count element |
| count-if | ✓ | stdlib.habu | Count by pred |
| count-if-not | ✗ | - | |
| delete | ✓ | stdlib.habu | Destructive remove |
| delete-duplicates | ✓ | stdlib.habu | Delete dups |
| delete-if | ✓ | stdlib.habu | Delete by pred |
| delete-if-not | ✓ | stdlib.habu | Delete not pred |
| elt | ✓ | stdlib.habu:417 | Sequence element |
| every | ✓ | stdlib.habu | All satisfy |
| fill | ✗ | - | |
| find | ✓ | compile.zig:191 | Find element |
| find-if | ✓ | stdlib.habu | Find by pred |
| find-if-not | ✗ | - | |
| length | ✓ | compile.zig:184 | Sequence length |
| map | ✗ | - | |
| map-into | ✗ | - | |
| merge | ✗ | - | |
| mismatch | ✗ | - | |
| notany | ✓ | stdlib.habu | None satisfy |
| notevery | ✓ | stdlib.habu | Not all satisfy |
| nsubstitute | ✗ | - | |
| nsubstitute-if | ✗ | - | |
| nsubstitute-if-not | ✗ | - | |
| position | ✓ | compile.zig:192 | Find position |
| position-if | ✓ | stdlib.habu | Position by pred |
| position-if-not | ✗ | - | |
| reduce | ✓ | stdlib.habu:152 | Reduce sequence |
| remove | ✓ | compile.zig:194 | Remove element |
| remove-duplicates | ✓ | stdlib.habu | Remove dups |
| remove-if | ✓ | stdlib.habu | Remove by pred |
| remove-if-not | ✓ | stdlib.habu | Remove not pred |
| replace | ✓ | stdlib.habu | Replace elements |
| search | ✓ | stdlib.habu | Search subsequence |
| some | ✓ | stdlib.habu | Any satisfy |
| sort | ✓ | stdlib.habu | Sort sequence |
| stable-sort | ✓ | stdlib.habu | Stable sort |
| subseq | ✓ | compile.zig:311 | Subsequence |
| substitute | ✓ | stdlib.habu | Substitute element |
| substitute-if | ✓ | stdlib.habu | Substitute by pred |
| substitute-if-not | ✓ | stdlib.habu | Substitute not pred |

## Functions - Hash Tables (17)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| clrhash | ✓ | compile.zig:372 | Clear hash table |
| gethash | ✓ | compile.zig:368 | Get hash entry |
| hash-table-count | ✓ | compile.zig:371 | Entry count |
| hash-table-p | ✓ | compile.zig:374 | Hash-table pred |
| hash-table-rehash-size | ✗ | - | |
| hash-table-rehash-threshold | ✗ | - | |
| hash-table-size | ✗ | - | |
| hash-table-test | ✓ | compile.zig:373 | Test function |
| make-hash-table | ✓ | compile.zig:367 | Create hash table |
| maphash | ✗ | - | |
| remhash | ✓ | compile.zig:370 | Remove entry |
| sxhash | ✓ | compile.zig:377 | Hash value |

## Functions - Filenames (26)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| *default-pathname-defaults* | ✗ | - | |
| directory | ✗ | - | |
| directory-namestring | ✗ | - | |
| enough-namestring | ✗ | - | |
| file-author | ✗ | - | |
| file-error-pathname | ✗ | - | |
| file-namestring | ✗ | - | |
| file-write-date | ✗ | - | |
| host-namestring | ✗ | - | |
| logical-pathname | ✗ | - | |
| logical-pathname-translations | ✗ | - | |
| make-pathname | ✗ | - | |
| merge-pathnames | ✗ | - | |
| namestring | ✗ | - | |
| parse-namestring | ✗ | - | |
| pathname | ✗ | - | |
| pathname-device | ✗ | - | |
| pathname-directory | ✗ | - | |
| pathname-host | ✗ | - | |
| pathname-match-p | ✗ | - | |
| pathname-name | ✗ | - | |
| pathname-type | ✗ | - | |
| pathname-version | ✗ | - | |
| pathnamep | ✗ | - | |
| probe-file | ✗ | - | |
| translate-logical-pathname | ✗ | - | |
| translate-pathname | ✗ | - | |
| truename | ✗ | - | |
| wild-pathname-p | ✗ | - | |

## Functions - Files/Streams (47)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| broadcast-stream-streams | ✗ | - | |
| clear-input | ✗ | - | |
| clear-output | ✗ | - | |
| close | ✓ | compile.zig:285 | Close stream |
| concatenated-stream-streams | ✗ | - | |
| delete-file | ✗ | - | |
| echo-stream-input-stream | ✗ | - | |
| echo-stream-output-stream | ✗ | - | |
| ensure-directories-exist | ✗ | - | |
| file-length | ✓ | compile.zig:291 | File length |
| file-position | ✓ | compile.zig:290 | File position |
| file-string-length | ✗ | - | |
| finish-output | ✓ | compile.zig:292 | Finish output |
| force-output | ✓ | compile.zig:293 | Force output |
| fresh-line | ✗ | - | |
| get-output-stream-string | ✓ | compile.zig:393 | Get string |
| input-stream-p | ✓ | compile.zig:389 | Input pred |
| interactive-stream-p | ✗ | - | |
| listen | ✗ | - | |
| make-broadcast-stream | ✗ | - | |
| make-concatenated-stream | ✗ | - | |
| make-echo-stream | ✗ | - | |
| make-string-input-stream | ✓ | compile.zig:391 | String input |
| make-string-output-stream | ✓ | compile.zig:392 | String output |
| make-synonym-stream | ✗ | - | |
| make-two-way-stream | ✗ | - | |
| open | ✓ | compile.zig:283 | Open file |
| open-stream-p | ✗ | - | |
| output-stream-p | ✓ | compile.zig:390 | Output pred |
| peek-char | ✓ | compile.zig:221 | Peek char |
| read-byte | ✓ | compile.zig:288 | Read byte |
| read-char | ✓ | compile.zig:220 | Read char |
| read-char-no-hang | ✗ | - | |
| read-line | ✓ | compile.zig:286 | Read line |
| read-sequence | ✗ | - | |
| rename-file | ✗ | - | |
| stream-element-type | ✗ | - | |
| stream-error-stream | ✗ | - | |
| stream-external-format | ✗ | - | |
| streamp | ✓ | compile.zig:388 | Stream pred |
| synonym-stream-symbol | ✗ | - | |
| two-way-stream-input-stream | ✗ | - | |
| two-way-stream-output-stream | ✗ | - | |
| unread-char | ✓ | compile.zig:222 | Unread char |
| write-byte | ✓ | compile.zig:289 | Write byte |
| write-char | ✓ | compile.zig:317 | Write char |
| write-line | ✓ | compile.zig:287 | Write line |
| write-sequence | ✗ | - | |
| write-string | ✓ | stdlib.habu | Write string |
| y-or-n-p | ✗ | - | |
| yes-or-no-p | ✗ | - | |

## Functions - Printer (25)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| copy-pprint-dispatch | ✗ | - | |
| format | ✓ | compile.zig:320 | Format output |
| pprint | ✗ | - | |
| pprint-dispatch | ✗ | - | |
| pprint-fill | ✗ | - | |
| pprint-indent | ✗ | - | |
| pprint-linear | ✗ | - | |
| pprint-newline | ✗ | - | |
| pprint-tab | ✗ | - | |
| pprint-tabular | ✗ | - | |
| prin1 | ✓ | vm.zig | Print readably |
| prin1-to-string | ✗ | - | |
| princ | ✓ | compile.zig:315 | Print aesthetic |
| princ-to-string | ✗ | - | |
| print | ✓ | compile.zig:314 | Print with newline |
| print-object | ✗ | - | |
| set-pprint-dispatch | ✗ | - | |
| terpri | ✓ | compile.zig:316 | Output newline |
| write | ✗ | - | |
| write-to-string | ✓ | compile.zig:336 | To string |

## Functions - Reader (19)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| copy-readtable | ✗ | - | |
| get-dispatch-macro-character | ✓ | compile.zig:332 | Get dispatch fn |
| get-macro-character | ✓ | compile.zig:330 | Get macro fn |
| make-dispatch-macro-character | ✗ | - | |
| read | ✓ | compile.zig:223 | Read S-expr |
| read-delimited-list | ✗ | - | |
| read-from-string | ✓ | compile.zig:224 | Read from string |
| read-preserving-whitespace | ✗ | - | |
| readtable-case | ✗ | - | |
| readtablep | ✓ | stdlib.habu | Readtable pred |
| set-dispatch-macro-character | ✓ | compile.zig:331 | Set dispatch fn |
| set-macro-character | ✓ | compile.zig:329 | Set macro fn |
| set-syntax-from-char | ✗ | - | |

## Functions - System/Environment (43)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| apropos | ✗ | - | |
| apropos-list | ✗ | - | |
| compile | ✗ | - | |
| compile-file | ✗ | - | |
| compile-file-pathname | ✗ | - | |
| compiled-function-p | ✓ | stdlib.habu | Compiled pred |
| compiler-macro-function | ✗ | - | |
| constantp | ✓ | stdlib.habu | Constant pred |
| decode-universal-time | ✗ | - | |
| describe | ✗ | - | |
| describe-object | ✗ | - | |
| disassemble | ✗ | - | |
| documentation | ✗ | - | |
| dribble | ✗ | - | |
| ed | ✗ | - | |
| encode-universal-time | ✗ | - | |
| eval | ✓ | compile.zig:226 | Evaluate form |
| get-decoded-time | ✗ | - | |
| get-internal-real-time | ✗ | - | |
| get-internal-run-time | ✗ | - | |
| get-universal-time | ✗ | - | |
| inspect | ✗ | - | |
| lisp-implementation-type | ✗ | - | |
| lisp-implementation-version | ✗ | - | |
| load | ✓ | compile.zig:225 | Load file |
| long-site-name | ✗ | - | |
| machine-instance | ✗ | - | |
| machine-type | ✗ | - | |
| machine-version | ✗ | - | |
| macro-function | ✗ | - | |
| macroexpand | ✓ | compile.zig:90 | Expand macro |
| macroexpand-1 | ✗ | - | |
| provide | ✗ | - | |
| require | ✗ | - | |
| room | ✗ | - | |
| short-site-name | ✗ | - | |
| sleep | ✗ | - | |
| software-type | ✗ | - | |
| software-version | ✗ | - | |
| special-operator-p | ✗ | - | |
| user-homedir-pathname | ✗ | - | |

## Functions - Symbols (17)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| boundp | ✓ | compile.zig:230 | Bound pred |
| copy-symbol | ✗ | - | |
| fboundp | ✓ | compile.zig:231 | Fbound pred |
| gensym | ✓ | compile.zig:227 | Generate symbol |
| gentemp | ✗ | - | |
| get | ✓ | compile.zig:238 | Property get |
| keywordp | ✓ | compile.zig:206 | Keyword pred |
| make-symbol | ✗ | - | |
| makunbound | ✗ | - | |
| remprop | ✓ | compile.zig:240 | Remove property |
| set | ✗ | - | |
| symbol-function | ✓ | compile.zig:233 | Get function |
| symbol-name | ✓ | compile.zig:237 | Get name |
| symbol-package | ✗ | - | |
| symbol-plist | ✓ | stdlib.habu | Get plist |
| symbol-value | ✓ | compile.zig:232 | Get value |
| symbolp | ✓ | compile.zig:201 | Symbol pred |

## Functions - Packages (22)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| delete-package | ✗ | - | |
| export | ✓ | compile.zig:122 | Export symbols |
| find-all-symbols | ✗ | - | |
| find-package | ✓ | package.zig | Find package |
| find-symbol | ✗ | - | |
| import | ✗ | - | |
| in-package | ✓ | compile.zig:121 | Switch package |
| intern | ✓ | compile.zig:236 | Intern symbol |
| list-all-packages | ✗ | - | |
| make-package | ✗ | - | |
| package-error-package | ✗ | - | |
| package-name | ✗ | - | |
| package-nicknames | ✗ | - | |
| package-shadowing-symbols | ✗ | - | |
| package-use-list | ✗ | - | |
| package-used-by-list | ✗ | - | |
| packagep | ✗ | - | |
| rename-package | ✗ | - | |
| shadow | ✗ | - | |
| shadowing-import | ✗ | - | |
| unexport | ✗ | - | |
| unintern | ✗ | - | |
| unuse-package | ✗ | - | |
| use-package | ✓ | compile.zig:123 | Use package |

## Functions - CLOS (25)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| add-method | ✗ | - | |
| allocate-instance | ✗ | - | |
| call-next-method | ✗ | - | Need for :around |
| change-class | ✗ | - | |
| class-name | ✗ | - | |
| class-of | ✗ | - | |
| compute-applicable-methods | ✗ | - | |
| ensure-generic-function | ✗ | - | |
| find-class | ✗ | - | |
| find-method | ✗ | - | |
| function-keywords | ✗ | - | |
| initialize-instance | ✗ | - | |
| make-instance | ✓ | compile.zig:130 | Create instance |
| make-instances-obsolete | ✗ | - | |
| make-load-form | ✗ | - | |
| make-load-form-saving-slots | ✗ | - | |
| method-qualifiers | ✗ | - | |
| next-method-p | ✗ | - | |
| no-applicable-method | ✗ | - | |
| no-next-method | ✗ | - | |
| reinitialize-instance | ✗ | - | |
| remove-method | ✗ | - | |
| shared-initialize | ✗ | - | |
| slot-boundp | ✗ | - | |
| slot-exists-p | ✗ | - | |
| slot-makunbound | ✗ | - | |
| slot-missing | ✗ | - | |
| slot-unbound | ✗ | - | |
| slot-value | ✓ | compile.zig:131 | Access slot |
| update-instance-for-different-class | ✗ | - | |
| update-instance-for-redefined-class | ✗ | - | |

## Functions - Conditions (25)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| abort | ✗ | - | Standard restart |
| assert | ✓ | stdlib.habu | Assertion |
| break | ✗ | - | |
| cell-error-name | ✗ | - | |
| cerror | ✓ | stdlib.habu:1041 | Continuable error |
| compute-restarts | ✗ | - | |
| continue | ✗ | - | Standard restart |
| error | ✓ | compile.zig:241 | Signal error |
| find-restart | ✓ | compile.zig:110 | Find restart |
| invoke-debugger | ✗ | - | |
| invoke-restart | ✓ | compile.zig:109 | Invoke restart |
| invoke-restart-interactively | ✗ | - | |
| muffle-warning | ✗ | - | Standard restart |
| package-error-package | ✗ | - | |
| print-not-readable-object | ✗ | - | |
| restart-name | ✗ | - | |
| signal | ✓ | compile.zig:105 | Signal condition |
| simple-condition-format-arguments | ✗ | - | |
| simple-condition-format-control | ✗ | - | |
| store-value | ✗ | - | Standard restart |
| type-error-datum | ✗ | - | |
| type-error-expected-type | ✗ | - | |
| unbound-slot-instance | ✗ | - | |
| use-value | ✗ | - | Standard restart |
| warn | ✓ | stdlib.habu | Issue warning |

## Functions - Types (15)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| coerce | ✓ | stdlib.habu | Type coercion |
| compiled-function-p | ✓ | stdlib.habu | Compiled pred |
| subtypep | ✗ | - | |
| type-of | ✓ | primitives/type.zig | Get type |
| typep | ✓ | compile.zig:234 | Type predicate |

## Type Specifiers (50)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| and | ✗ | - | Compound type |
| array | ✓ | objects.zig | Array type |
| atom | ✓ | compile.zig | Atom type |
| base-char | ✗ | - | |
| base-string | ✗ | - | |
| bignum | ✓ | objects.zig | Large integer |
| bit | ✗ | - | |
| bit-vector | ✓ | stdlib.habu | Bit vector |
| boolean | ✗ | - | |
| broadcast-stream | ✗ | - | |
| built-in-class | ✗ | - | |
| cell-error | ✗ | - | |
| character | ✓ | compile.zig | Character type |
| class | ✗ | - | |
| compiled-function | ✓ | stdlib.habu | Compiled fn type |
| complex | ✓ | compile.zig | Complex type |
| concatenated-stream | ✗ | - | |
| condition | ✗ | - | |
| cons | ✓ | compile.zig | Cons type |
| control-error | ✗ | - | |
| division-by-zero | ✗ | - | |
| double-float | ✗ | - | |
| echo-stream | ✗ | - | |
| end-of-file | ✗ | - | |
| eql | ✗ | - | Singleton type |
| error | ✗ | - | Error condition |
| extended-char | ✗ | - | |
| file-error | ✗ | - | |
| file-stream | ✗ | - | |
| fixnum | ✓ | compile.zig | Small integer |
| float | ✓ | compile.zig | Float type |
| floating-point-inexact | ✗ | - | |
| floating-point-invalid-operation | ✗ | - | |
| floating-point-overflow | ✗ | - | |
| floating-point-underflow | ✗ | - | |
| function | ✓ | compile.zig | Function type |
| generic-function | ✗ | - | |
| hash-table | ✓ | compile.zig | Hash table type |
| integer | ✓ | compile.zig | Integer type |
| keyword | ✓ | compile.zig | Keyword type |
| list | ✓ | compile.zig | List type |
| logical-pathname | ✗ | - | |
| long-float | ✗ | - | |
| member | ✗ | - | Enumeration type |
| method | ✗ | - | |
| method-combination | ✗ | - | |
| mod | ✗ | - | |
| nil | ✓ | compile.zig | Nil type |
| not | ✗ | - | Complement type |
| null | ✓ | compile.zig | Null type |
| number | ✓ | compile.zig | Number type |
| or | ⚠ | compile.zig | Union type (ty_or) |
| package | ✗ | - | |
| package-error | ✗ | - | |
| parse-error | ✗ | - | |
| pathname | ⚠ | objects.zig | Type exists, ops missing |
| print-not-readable | ✗ | - | |
| program-error | ✗ | - | |
| random-state | ✗ | - | |
| ratio | ✓ | objects.zig | Rational type |
| rational | ✓ | compile.zig | Rational type |
| reader-error | ✗ | - | |
| readtable | ✓ | vm.zig | Readtable type |
| real | ✗ | - | Real number type |
| restart | ✗ | - | |
| satisfies | ✗ | - | Predicate type |
| sequence | ✗ | - | |
| serious-condition | ✗ | - | |
| short-float | ✗ | - | |
| signed-byte | ✗ | - | |
| simple-array | ✗ | - | |
| simple-base-string | ✗ | - | |
| simple-bit-vector | ✗ | - | |
| simple-condition | ✓ | stdlib.habu | Simple condition |
| simple-error | ✓ | stdlib.habu | Simple error |
| simple-string | ✓ | stdlib.habu | Simple string |
| simple-type-error | ✗ | - | |
| simple-vector | ✓ | stdlib.habu | Simple vector |
| simple-warning | ✗ | - | |
| single-float | ✗ | - | |
| standard-char | ✗ | - | |
| standard-class | ✗ | - | |
| standard-generic-function | ✗ | - | |
| standard-method | ✗ | - | |
| standard-object | ✗ | - | |
| storage-condition | ✗ | - | |
| stream | ✓ | compile.zig | Stream type |
| stream-error | ✗ | - | |
| string | ✓ | compile.zig | String type |
| string-stream | ✓ | compile.zig | String stream |
| structure-class | ✗ | - | |
| structure-object | ✗ | - | |
| style-warning | ✗ | - | |
| symbol | ✓ | compile.zig | Symbol type |
| synonym-stream | ✗ | - | |
| t | ✓ | compile.zig | Universal type |
| two-way-stream | ✗ | - | |
| type-error | ✗ | - | |
| unbound-slot | ✗ | - | |
| unbound-variable | ✗ | - | |
| undefined-function | ✗ | - | |
| unsigned-byte | ✗ | - | |
| values | ✗ | - | Multiple values type |
| vector | ✓ | compile.zig | Vector type |
| warning | ✗ | - | |

---

## Summary

| Category | Total | Implemented | Partial | Missing |
|----------|-------|-------------|---------|---------|
| Lambda Keywords | 8 | 4 | 1 | 3 |
| Special Variables | 65 | 18 | 0 | 47 |
| Constants | 43 | 3 | 0 | 40 |
| Special Operators | 25 | 19 | 0 | 6 |
| Macros | 86 | 53 | 1 | 32 |
| Arithmetic | 79 | 64 | 0 | 15 |
| Characters | 30 | 20 | 0 | 10 |
| Conses/Lists | 77 | 67 | 0 | 10 |
| Arrays | 38 | 8 | 0 | 30 |
| Strings | 28 | 23 | 0 | 5 |
| Sequences | 56 | 36 | 0 | 20 |
| Hash Tables | 17 | 11 | 0 | 6 |
| Filenames | 26 | 0 | 0 | 26 |
| Files/Streams | 47 | 24 | 0 | 23 |
| Printer | 25 | 7 | 0 | 18 |
| Reader | 19 | 10 | 0 | 9 |
| System/Environment | 43 | 5 | 0 | 38 |
| Symbols | 17 | 12 | 0 | 5 |
| Packages | 22 | 6 | 0 | 16 |
| CLOS | 25 | 2 | 0 | 23 |
| Conditions | 25 | 9 | 0 | 16 |
| Types | 15 | 4 | 0 | 11 |
| Type Specifiers | 50 | 28 | 2 | 20 |

**Total: 978 symbols**
- **Implemented: ~523 (53%)**
- **Partial: ~4 (0.4%)**
- **Missing: ~451 (46%)**

---

## Priority Implementation Order

### Phase 1: High Priority
1. LOOP enhancements (append/nconc/minimize/maximize, hash iteration, conditionals)
2. CLOS method combinations (:before/:after/:around, call-next-method)
3. Condition system (define-condition, standard restarts, break)
4. Pathname system (make-pathname, accessors, #P reader)

### Phase 2: Core Gaps
5. Type system (subtypep, compound types)
6. Declarations (declare, declaim)
7. Print control (*print-* variables)
8. Reader macros (#S, #A, #*, runtime readtable)

### Phase 3: Infrastructure
9. MOP (class-of, slot-boundp, etc.)
10. Compiler (compile, compile-file)
11. Package system (do-symbols, unintern)
12. Stream types (broadcast, echo, etc.)

### Phase 4: Polish
13. Time/environment functions
14. Documentation strings
15. Pretty printer (XP algorithm)
16. Remaining constants
