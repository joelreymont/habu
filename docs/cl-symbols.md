# ANSI Common Lisp Symbol Audit

978 external symbols in COMMON-LISP package per ANSI INCITS 226-1994.

Status: ✓ implemented (653, 67%) | ⚠ partial (29, 3%) | ✗ missing (278, 28%)
Note: 960 symbols documented, 18 symbols pending audit

## Lambda List Keywords (8)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| &allow-other-keys | ⚠ | compile.zig | Parsed but not fully enforced |
| &aux | ✓ | compile.zig:453 | Aux bindings |
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
| *print-array* | ✓ | vm.zig:590, io.zig | Control array printing |
| *print-base* | ✓ | vm.zig:571, io.zig | Number output radix |
| *print-case* | ✓ | vm.zig:559, io.zig | Symbol case control |
| *print-circle* | ✗ | - | |
| *print-escape* | ✓ | vm.zig:551, io.zig:17 | Control escape sequences |
| *print-gensym* | ✓ | vm.zig:583, io.zig | Gensym prefix control |
| *print-length* | ✓ | vm.zig:534, io.zig:41 | Max elements to print |
| *print-level* | ✓ | vm.zig:543, io.zig:45 | Max nesting depth |
| *print-lines* | ✗ | - | |
| *print-miser-width* | ✗ | - | |
| *print-pprint-dispatch* | ✗ | - | |
| *print-pretty* | ✗ | - | |
| *print-radix* | ✓ | vm.zig:577, io.zig | Radix indicator control |
| *print-readably* | ✓ | vm.zig:565, io.zig:37 | Readable output control |
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
| array-dimension-limit | ✓ | stdlib.habu:20 | 2^62-1 |
| array-rank-limit | ✓ | stdlib.habu:21 | 65536 |
| array-total-size-limit | ✓ | stdlib.habu:22 | 2^62-1 |
| boole-1 | ✓ | stdlib.habu:67 | 2 |
| boole-2 | ✓ | stdlib.habu:68 | 3 |
| boole-and | ✓ | stdlib.habu:71 | 6 |
| boole-andc1 | ✓ | stdlib.habu:77 | 12 |
| boole-andc2 | ✓ | stdlib.habu:78 | 13 |
| boole-c1 | ✓ | stdlib.habu:69 | 4 |
| boole-c2 | ✓ | stdlib.habu:70 | 5 |
| boole-clr | ✓ | stdlib.habu:65 | 0 |
| boole-eqv | ✓ | stdlib.habu:74 | 9 |
| boole-ior | ✓ | stdlib.habu:72 | 7 |
| boole-nand | ✓ | stdlib.habu:75 | 10 |
| boole-nor | ✓ | stdlib.habu:76 | 11 |
| boole-orc1 | ✓ | stdlib.habu:79 | 14 |
| boole-orc2 | ✓ | stdlib.habu:80 | 15 |
| boole-set | ✓ | stdlib.habu:66 | 1 |
| boole-xor | ✓ | stdlib.habu:73 | 8 |
| call-arguments-limit | ✓ | stdlib.habu:23 | 65536 |
| char-code-limit | ✓ | stdlib.habu:24 | 1114112 (Unicode) |
| double-float-epsilon | ✓ | stdlib.habu:35 | 2.22e-16 |
| double-float-negative-epsilon | ✓ | stdlib.habu:36 | 1.11e-16 |
| internal-time-units-per-second | ✓ | stdlib.habu:86 | 1000000 (µs) |
| lambda-list-keywords | ✓ | stdlib.habu:83 | List of &-keywords |
| lambda-parameters-limit | ✓ | stdlib.habu:25 | 65536 |
| least-negative-double-float | ✓ | stdlib.habu:32 | -2.225e-308 |
| least-negative-long-float | ✓ | stdlib.habu:58 | Same as double |
| least-negative-normalized-double-float | ✓ | stdlib.habu:34 | -2.225e-308 |
| least-negative-normalized-long-float | ✓ | stdlib.habu:60 | Same as double |
| least-negative-normalized-short-float | ✓ | stdlib.habu:52 | Same as double |
| least-negative-normalized-single-float | ✓ | stdlib.habu:44 | Same as double |
| least-negative-short-float | ✓ | stdlib.habu:50 | Same as double |
| least-negative-single-float | ✓ | stdlib.habu:42 | Same as double |
| least-positive-double-float | ✓ | stdlib.habu:31 | 2.225e-308 |
| least-positive-long-float | ✓ | stdlib.habu:57 | Same as double |
| least-positive-normalized-double-float | ✓ | stdlib.habu:33 | 2.225e-308 |
| least-positive-normalized-long-float | ✓ | stdlib.habu:59 | Same as double |
| least-positive-normalized-short-float | ✓ | stdlib.habu:51 | Same as double |
| least-positive-normalized-single-float | ✓ | stdlib.habu:43 | Same as double |
| least-positive-short-float | ✓ | stdlib.habu:49 | Same as double |
| least-positive-single-float | ✓ | stdlib.habu:41 | Same as double |
| long-float-epsilon | ✓ | stdlib.habu:61 | Same as double |
| long-float-negative-epsilon | ✓ | stdlib.habu:62 | Same as double |
| most-negative-double-float | ✓ | stdlib.habu:30 | -1.797e308 |
| most-negative-fixnum | ✓ | stdlib.habu:19 | -2^62 |
| most-negative-long-float | ✓ | stdlib.habu:56 | Same as double |
| most-negative-short-float | ✓ | stdlib.habu:48 | Same as double |
| most-negative-single-float | ✓ | stdlib.habu:40 | Same as double |
| most-positive-double-float | ✓ | stdlib.habu:29 | 1.797e308 |
| most-positive-fixnum | ✓ | stdlib.habu:18 | 2^62-1 |
| most-positive-long-float | ✓ | stdlib.habu:55 | Same as double |
| most-positive-short-float | ✓ | stdlib.habu:47 | Same as double |
| most-positive-single-float | ✓ | stdlib.habu:39 | Same as double |
| multiple-values-limit | ✓ | stdlib.habu:26 | 256 |
| nil | ✓ | value.zig | Constant |
| pi | ✓ | stdlib.habu:14 | 3.14159... |
| short-float-epsilon | ✓ | stdlib.habu:53 | Same as double |
| short-float-negative-epsilon | ✓ | stdlib.habu:54 | Same as double |
| single-float-epsilon | ✓ | stdlib.habu:45 | Same as double |
| single-float-negative-epsilon | ✓ | stdlib.habu:46 | Same as double |
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
| locally | ✓ | stdlib.habu | Local declarations |
| macrolet | ✗ | - | |
| multiple-value-call | ✓ | compile.zig:116 | MV function call |
| multiple-value-prog1 | ✓ | stdlib.habu | MV return first |
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
| declaim | ✓ | compile.zig | Global declaration |
| defclass | ✓ | compile.zig:129 | CLOS class |
| defconstant | ✓ | stdlib.habu | Constant definition |
| defgeneric | ✓ | compile.zig:132 | Generic function |
| define-compiler-macro | ✗ | stdlib.habu | Compiler macro (stub) |
| define-condition | ✓ | stdlib.habu | Condition class |
| define-method-combination | ✗ | stdlib.habu | Method combination (stub) |
| define-modify-macro | ✓ | stdlib.habu | Read-modify-write macro |
| define-setf-expander | ✗ | stdlib.habu | Setf expander (stub) |
| define-symbol-macro | ✗ | stdlib.habu | Symbol macro (stub) |
| defmacro | ✓ | compile.zig:89 | Macro definition |
| defmethod | ✓ | compile.zig:133 | CLOS method |
| defpackage | ✓ | compile.zig:120 | Package definition |
| defparameter | ✓ | stdlib.habu | Parameter definition |
| defsetf | ✓ | stdlib.habu | Setf expansion |
| defstruct | ✓ | stdlib.habu:2739 | Structure |
| deftype | ✓ | compile.zig | Type definition |
| defun | ✓ | compile.zig:69 | Function definition |
| defvar | ✓ | compile.zig:68 | Variable definition |
| destructuring-bind | ✓ | stdlib.habu:504 | Pattern destructure |
| do | ✓ | stdlib.habu:61 | Parallel iteration |
| do* | ✓ | stdlib.habu:113 | Sequential iteration |
| do-all-symbols | ✓ | stdlib.habu | All symbol iteration (stub) |
| do-external-symbols | ✓ | stdlib.habu | External symbol iteration (stub) |
| do-symbols | ✓ | stdlib.habu | Symbol iteration (stub) |
| dolist | ✓ | stdlib.habu | List iteration |
| dotimes | ✓ | stdlib.habu | Count iteration |
| ecase | ✓ | stdlib.habu | Error case |
| etypecase | ✓ | stdlib.habu | Error typecase |
| formatter | ✓ | stdlib.habu | Format compiler |
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
| pprint-exit-if-list-exhausted | ✗ | stdlib.habu | Pprint exit (stub) |
| pprint-logical-block | ✗ | stdlib.habu | Pprint block (stub) |
| pprint-pop | ✗ | stdlib.habu | Pprint pop (stub) |
| print-unreadable-object | ✓ | stdlib.habu | Print #<...> notation |
| prog | ✓ | stdlib.habu | Prog form |
| prog* | ✓ | stdlib.habu | Prog with let* |
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
| step | ✓ | stdlib.habu | Single-step (stub) |
| time | ✓ | stdlib.habu | Time execution |
| trace | ✓ | stdlib.habu | Trace calls (stub) |
| typecase | ✓ | stdlib.habu | Type dispatch |
| unless | ✓ | stdlib.habu | Negated when |
| untrace | ✓ | stdlib.habu | Untrace (stub) |
| when | ✓ | stdlib.habu | Conditional |
| with-accessors | ⚠ | stdlib.habu | Accessor bindings (no setf) |
| with-compilation-unit | ✓ | stdlib.habu | Compilation unit (stub) |
| with-condition-restarts | ✓ | stdlib.habu | Condition restarts (stub) |
| with-hash-table-iterator | ✓ | stdlib.habu | Hash iteration |
| with-input-from-string | ✓ | stdlib.habu | String input |
| with-open-file | ✓ | stdlib.habu | File scope |
| with-open-stream | ✓ | stdlib.habu | Generic stream wrapper |
| with-output-to-string | ✓ | stdlib.habu | String output |
| with-package-iterator | ✓ | stdlib.habu | Package iterator (stub) |
| with-simple-restart | ✓ | stdlib.habu | Simple restart |
| with-slots | ⚠ | stdlib.habu | Slot bindings (no setf) |
| with-standard-io-syntax | ✓ | stdlib.habu | Standard I/O env |

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
| arithmetic-error-operands | ⚠ | stdlib.habu | Error operands (stub) |
| arithmetic-error-operation | ⚠ | stdlib.habu | Error operation (stub) |
| ash | ✓ | compile.zig:343 | Arithmetic shift |
| asin | ✓ | compile.zig:260 | Arc sine |
| asinh | ✓ | compile.zig:266 | Hyperbolic arc sin |
| atan | ✓ | compile.zig:262 | Arc tangent |
| atanh | ✓ | compile.zig:268 | Hyperbolic arc tan |
| boole | ✓ | stdlib.habu | Boolean ops on integers |
| byte | ✓ | stdlib.habu | Byte specifier |
| byte-position | ✓ | stdlib.habu | Get byte position |
| byte-size | ✓ | stdlib.habu | Get byte size |
| ceiling | ✓ | compile.zig:272 | Ceiling |
| cis | ✓ | stdlib.habu:1765 | cos+i*sin |
| complex | ✓ | compile.zig:380 | Make complex |
| conjugate | ✓ | stdlib.habu | Complex conjugate |
| cos | ✓ | compile.zig:258 | Cosine |
| cosh | ✓ | compile.zig:264 | Hyperbolic cos |
| decode-float | ⚠ | stdlib.habu | Decompose float (stub) |
| denominator | ✓ | compile.zig:384 | Rational denom |
| deposit-field | ✓ | stdlib.habu | Deposit field |
| dpb | ✓ | stdlib.habu | Deposit byte |
| evenp | ✓ | compile.zig:252 | Even predicate |
| exp | ✓ | compile.zig:269 | Exponential |
| expt | ✓ | stdlib.habu | Power |
| fceiling | ✓ | stdlib.habu | Ceiling as float |
| ffloor | ✓ | stdlib.habu | Floor as float |
| float | ✓ | arith.zig | To float |
| float-digits | ✓ | stdlib.habu | Float digits |
| float-precision | ✓ | stdlib.habu | Float precision |
| float-radix | ✓ | stdlib.habu | Float radix |
| float-sign | ✓ | stdlib.habu | Float sign |
| floor | ✓ | compile.zig:271 | Floor |
| fround | ✓ | stdlib.habu | Round as float |
| ftruncate | ✓ | stdlib.habu | Truncate as float |
| gcd | ✓ | stdlib.habu | Greatest common divisor |
| imagpart | ✓ | compile.zig:382 | Complex imaginary |
| integer-decode-float | ⚠ | stdlib.habu | Decompose to ints (stub) |
| integer-length | ✓ | compile.zig:351 | Bit length |
| isqrt | ✓ | stdlib.habu:63 | Integer sqrt |
| lcm | ✓ | stdlib.habu | Least common multiple |
| ldb | ✓ | stdlib.habu | Load byte |
| ldb-test | ✓ | stdlib.habu | Test byte |
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
| mask-field | ✓ | stdlib.habu | Extract field |
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
| random-state-p | ✓ | stdlib.habu | Random state pred |
| rational | ✓ | compile.zig:385 | To rational |
| rationalize | ✓ | compile.zig:386 | Approx rational |
| realpart | ✓ | compile.zig:381 | Complex real |
| rem | ✓ | compile.zig:148 | Remainder |
| round | ✓ | compile.zig:273 | Round |
| scale-float | ✓ | stdlib.habu | Scale float |
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
| char-int | ✓ | stdlib.habu | Char to int |
| char-lessp | ✓ | stdlib.habu | Case-insensitive < |
| char-name | ✓ | stdlib.habu | Char to name |
| char-not-equal | ✓ | stdlib.habu | Case-insensitive /= |
| char-not-greaterp | ✓ | stdlib.habu | Case-insensitive <= |
| char-not-lessp | ✓ | stdlib.habu | Case-insensitive >= |
| char-upcase | ✓ | compile.zig:323 | To uppercase |
| char/= | ✓ | stdlib.habu | Char not equal |
| char< | ✓ | compile.zig:218 | Char less |
| char<= | ✓ | stdlib.habu | Char less or equal |
| char= | ✓ | compile.zig:217 | Char equal |
| char> | ✓ | compile.zig:219 | Char greater |
| char>= | ✓ | stdlib.habu | Char greater or equal |
| character | ✓ | compile.zig:244 | Coerce to char |
| characterp | ✓ | compile.zig:209 | Char predicate |
| code-char | ✓ | compile.zig:216 | From code point |
| digit-char | ✓ | stdlib.habu | Digit to char |
| digit-char-p | ✓ | compile.zig:325 | Digit predicate |
| graphic-char-p | ✓ | stdlib.habu | Graphic char predicate |
| lower-case-p | ✓ | stdlib.habu | Lowercase pred |
| name-char | ✓ | stdlib.habu | Name to char |
| standard-char-p | ✓ | stdlib.habu | Standard predicate |
| upper-case-p | ✓ | stdlib.habu | Uppercase pred |

## Functions - Conses/Lists (77)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| acons | ✓ | stdlib.habu | Add to alist |
| adjoin | ✓ | stdlib.habu | Set adjoin |
| append | ✓ | compile.zig:183 | List append |
| assoc | ✓ | compile.zig:190 | Alist lookup |
| assoc-if | ✓ | stdlib.habu | Alist lookup if |
| assoc-if-not | ✓ | stdlib.habu | Alist lookup if not |
| atom | ✓ | compile.zig:212 | Atom predicate |
| butlast | ✓ | stdlib.habu | All but last |
| caaaar | ✓ | stdlib.habu | 4-level car |
| caaadr | ✓ | stdlib.habu | 4-level car |
| caaar | ✓ | stdlib.habu | 3-level car |
| caadar | ✓ | stdlib.habu | 4-level car |
| caaddr | ✓ | stdlib.habu | 4-level car |
| caadr | ✓ | stdlib.habu | 3-level car |
| caar | ✓ | compile.zig:166 | Car of car |
| cadaar | ✓ | stdlib.habu | 4-level car |
| cadadr | ✓ | stdlib.habu | 4-level car |
| cadar | ✓ | compile.zig:173 | Car of cdr of car |
| caddar | ✓ | stdlib.habu | 4-level car |
| cadddr | ✓ | stdlib.habu | 4th via cdr |
| caddr | ✓ | compile.zig:174 | Third |
| cadr | ✓ | compile.zig:167 | Second |
| car | ✓ | compile.zig:161 | First of cons |
| cdaaar | ✓ | stdlib.habu | 4-level cdr |
| cdaadr | ✓ | stdlib.habu | 4-level cdr |
| cdaar | ✓ | stdlib.habu | 3-level cdr |
| cdadar | ✓ | stdlib.habu | 4-level cdr |
| cdaddr | ✓ | stdlib.habu | 4-level cdr |
| cdadr | ✓ | compile.zig:176 | Cdr of cadr |
| cdar | ✓ | compile.zig:168 | Cdr of car |
| cddaar | ✓ | stdlib.habu | 4-level cdr |
| cddadr | ✓ | stdlib.habu | 4-level cdr |
| cddar | ✓ | compile.zig:177 | Cdr of cdar |
| cdddar | ✓ | stdlib.habu | 4-level cdr |
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
| get-properties | ✓ | stdlib.habu | Plist search |
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
| mapl | ✓ | stdlib.habu | Map for effect on tails |
| maplist | ✓ | stdlib.habu:467 | Map over tails |
| member | ✓ | compile.zig:189 | List member |
| member-if | ✓ | stdlib.habu | Member by pred |
| member-if-not | ✓ | stdlib.habu | Member not pred |
| nbutlast | ✓ | stdlib.habu | Destructive butlast |
| nconc | ✓ | stdlib.habu | Destructive append |
| nintersection | ✓ | stdlib.habu | Destructive intersection |
| ninth | ✓ | stdlib.habu | 9th element |
| nreconc | ✓ | stdlib.habu | Destructive revappend |
| nreverse | ✓ | stdlib.habu | Destructive reverse |
| nset-difference | ✓ | stdlib.habu | Destructive set-difference |
| nset-exclusive-or | ✓ | stdlib.habu | Destructive set-exclusive-or |
| nsublis | ✓ | stdlib.habu | Destructive sublis |
| nsubst | ✓ | stdlib.habu | Destructive subst |
| nsubst-if | ✓ | stdlib.habu | Destructive subst-if |
| nsubst-if-not | ✓ | stdlib.habu | Destructive subst-if-not |
| nth | ✓ | compile.zig:186 | Nth element |
| nthcdr | ✓ | compile.zig:187 | Nth cdr |
| null | ✓ | compile.zig:207 | Null predicate |
| nunion | ✓ | stdlib.habu | Destructive union |
| pairlis | ✓ | stdlib.habu | Make alist |
| pop | ✓ | stdlib.habu | Pop from list |
| push | ✓ | stdlib.habu | Push to list |
| pushnew | ✓ | stdlib.habu | Conditional push |
| rassoc | ✓ | stdlib.habu | Reverse assoc |
| rassoc-if | ✓ | stdlib.habu | Reverse assoc-if |
| rassoc-if-not | ✓ | stdlib.habu | Reverse assoc-if-not |
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
| adjust-array | ⚠ | stdlib.habu | Resize array (stub) |
| adjustable-array-p | ✓ | stdlib.habu | Adjustable array pred |
| aref | ✓ | compile.zig:276 | Array element |
| array-dimension | ✓ | stdlib.habu | Get dimension |
| array-dimensions | ✓ | stdlib.habu | Get dimensions list |
| array-displacement | ✓ | stdlib.habu | Get displacement |
| array-element-type | ✓ | stdlib.habu | Element type |
| array-has-fill-pointer-p | ✓ | stdlib.habu | Fill pointer pred |
| array-in-bounds-p | ✓ | stdlib.habu | Bounds check |
| array-rank | ✓ | stdlib.habu | Array rank |
| array-row-major-index | ✓ | stdlib.habu | Row-major index |
| array-total-size | ✓ | stdlib.habu | Total size |
| arrayp | ✓ | stdlib.habu | Array predicate |
| bit | ⚠ | stdlib.habu | Bit access (stub) |
| bit-and | ⚠ | stdlib.habu | Bitwise and (stub) |
| bit-andc1 | ⚠ | stdlib.habu | Bit andc1 (stub) |
| bit-andc2 | ⚠ | stdlib.habu | Bit andc2 (stub) |
| bit-eqv | ⚠ | stdlib.habu | Bit equiv (stub) |
| bit-ior | ⚠ | stdlib.habu | Bit ior (stub) |
| bit-nand | ⚠ | stdlib.habu | Bit nand (stub) |
| bit-nor | ⚠ | stdlib.habu | Bit nor (stub) |
| bit-not | ⚠ | stdlib.habu | Bit not (stub) |
| bit-orc1 | ⚠ | stdlib.habu | Bit orc1 (stub) |
| bit-orc2 | ⚠ | stdlib.habu | Bit orc2 (stub) |
| bit-vector-p | ✓ | stdlib.habu | Bit-vector pred |
| bit-xor | ⚠ | stdlib.habu | Bit xor (stub) |
| fill-pointer | ⚠ | stdlib.habu | Get fill pointer (stub) |
| make-array | ✓ | compile.zig:298 | Create array |
| row-major-aref | ✓ | stdlib.habu | Row-major access |
| sbit | ⚠ | stdlib.habu | Simple bit access (stub) |
| simple-bit-vector-p | ✓ | stdlib.habu | Bit-vector pred |
| simple-vector-p | ✓ | stdlib.habu | Simple-vector pred |
| svref | ✓ | compile.zig:277 | Simple-vector ref |
| upgraded-array-element-type | ✓ | stdlib.habu | Upgraded type |
| vector | ✓ | compile.zig:297 | Make vector |
| vector-pop | ⚠ | stdlib.habu | Vector pop (stub) |
| vector-push | ⚠ | stdlib.habu | Vector push (stub) |
| vector-push-extend | ⚠ | stdlib.habu | Vector push extend (stub) |
| vectorp | ✓ | compile.zig:204 | Vector predicate |

## Functions - Strings (28)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| char | ✓ | compile.zig:301 | String char |
| make-string | ✓ | compile.zig:358 | Create string |
| nstring-capitalize | ✓ | stdlib.habu | Destructive capitalize |
| nstring-downcase | ✓ | stdlib.habu | Destructive downcase |
| nstring-upcase | ✓ | stdlib.habu | Destructive upcase |
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
| count-if-not | ✓ | stdlib.habu | Count not pred |
| delete | ✓ | stdlib.habu | Destructive remove |
| delete-duplicates | ✓ | stdlib.habu | Delete dups |
| delete-if | ✓ | stdlib.habu | Delete by pred |
| delete-if-not | ✓ | stdlib.habu | Delete not pred |
| elt | ✓ | stdlib.habu:417 | Sequence element |
| every | ✓ | stdlib.habu | All satisfy |
| fill | ✓ | stdlib.habu | Fill sequence |
| find | ✓ | compile.zig:191 | Find element |
| find-if | ✓ | stdlib.habu | Find by pred |
| find-if-not | ✓ | stdlib.habu | Find not pred |
| length | ✓ | compile.zig:184 | Sequence length |
| map | ✓ | stdlib.habu | Map function |
| map-into | ✓ | stdlib.habu:125 | Map into result-seq |
| merge | ✓ | stdlib.habu | Merge sequences |
| mismatch | ✓ | stdlib.habu | Find first mismatch |
| notany | ✓ | stdlib.habu | None satisfy |
| notevery | ✓ | stdlib.habu | Not all satisfy |
| nsubstitute | ✓ | stdlib.habu | Destructive substitute |
| nsubstitute-if | ✓ | stdlib.habu:738 | Destructive substitute-if |
| nsubstitute-if-not | ✓ | stdlib.habu:742 | Destructive substitute-if-not |
| position | ✓ | compile.zig:192 | Find position |
| position-if | ✓ | stdlib.habu | Position by pred |
| position-if-not | ✓ | stdlib.habu | Position not pred |
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
| hash-table-rehash-size | ✓ | stdlib.habu | Rehash size |
| hash-table-rehash-threshold | ✓ | stdlib.habu | Rehash threshold |
| hash-table-size | ✓ | stdlib.habu | Table size |
| hash-table-test | ✓ | compile.zig:373 | Test function |
| make-hash-table | ✓ | compile.zig:367 | Create hash table |
| maphash | ✓ | stdlib.habu | Map over hash |
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
| make-pathname | ✓ | pathname.zig:9, vm.zig:3013, stdlib.habu:4080 | Create from components |
| merge-pathnames | ✓ | pathname.zig:269, vm.zig:3232 | Merge with defaults |
| namestring | ✓ | pathname.zig:78, vm.zig:3173 | Pathname to string |
| parse-namestring | ✓ | pathname.zig:153, vm.zig:3074 | String to pathname |
| pathname | ✓ | pathname.zig:71, vm.zig:3042 | Coerce to pathname |
| pathname-device | ✓ | pathname.zig:31, vm.zig:2721 | Get device component |
| pathname-directory | ✓ | pathname.zig:38, vm.zig:2728 | Get directory component |
| pathname-host | ✓ | pathname.zig:24, vm.zig:2714 | Get host component |
| pathname-match-p | ✗ | - | |
| pathname-name | ✓ | pathname.zig:45, vm.zig:2735 | Get name component |
| pathname-type | ✓ | pathname.zig:52, vm.zig:2742 | Get type component |
| pathname-version | ✓ | pathname.zig:59, vm.zig:2749 | Get version component |
| pathnamep | ✓ | pathname.zig:66 | Check if pathname |
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
| y-or-n-p | ✓ | stdlib.habu | Y/N query |
| yes-or-no-p | ✓ | stdlib.habu | Yes/No query |

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
| prin1 | ✓ | io.zig:641 | Print readably |
| prin1-to-string | ✓ | stdlib.habu:2815 | Print to string readably |
| princ | ✓ | io.zig:646 | Print aesthetic |
| princ-to-string | ✓ | stdlib.habu | Print to string |
| print | ✓ | io.zig:663 | Print with newline |
| print-object | ✗ | - | |
| set-pprint-dispatch | ✗ | - | |
| terpri | ✓ | compile.zig:316 | Output newline |
| write | ✓ | io.zig:624 | General output function |
| write-to-string | ✓ | io.zig:471 | To string |

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
| special-operator-p | ✓ | stdlib.habu | Special op pred |
| user-homedir-pathname | ✗ | - | |

## Functions - Symbols (17)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| boundp | ✓ | compile.zig:230 | Bound pred |
| copy-symbol | ✗ | - | |
| fboundp | ✓ | compile.zig:231 | Fbound pred |
| gensym | ✓ | compile.zig:227 | Generate symbol |
| gentemp | ✓ | stdlib.habu | Generate temp symbol |
| get | ✓ | compile.zig:238 | Property get |
| keywordp | ✓ | compile.zig:206 | Keyword pred |
| make-symbol | ✓ | stdlib.habu | Create uninterned symbol |
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
| abort | ✓ | stdlib.habu:472 | Standard restart |
| assert | ✓ | stdlib.habu | Assertion |
| break | ✓ | stdlib.habu:499 | Debugger entry |
| cell-error-name | ✗ | - | |
| cerror | ✓ | stdlib.habu:1041 | Continuable error |
| compute-restarts | ✗ | - | |
| continue | ✓ | stdlib.habu:477 | Standard restart |
| error | ✓ | compile.zig:241 | Signal error |
| find-restart | ✓ | compile.zig:110 | Find restart |
| invoke-debugger | ✗ | - | |
| invoke-restart | ✓ | compile.zig:109 | Invoke restart |
| invoke-restart-interactively | ✗ | - | |
| muffle-warning | ✓ | stdlib.habu:484 | Standard restart |
| package-error-package | ✗ | - | |
| print-not-readable-object | ✗ | - | |
| restart-name | ✗ | - | |
| signal | ✓ | compile.zig:105 | Signal condition |
| simple-condition-format-arguments | ✗ | - | |
| simple-condition-format-control | ✗ | - | |
| store-value | ✓ | stdlib.habu:489 | Standard restart |
| type-error-datum | ✗ | - | |
| type-error-expected-type | ✗ | - | |
| unbound-slot-instance | ✗ | - | |
| use-value | ✓ | stdlib.habu:494 | Standard restart |
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
| bit | ⚠ | stdlib.habu | Bit access (stub) |
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
| Constants | 43 | 43 | 0 | 0 |
| Special Operators | 25 | 19 | 0 | 6 |
| Macros | 86 | 53 | 1 | 32 |
| Arithmetic | 79 | 64 | 0 | 15 |
| Characters | 30 | 20 | 0 | 10 |
| Conses/Lists | 77 | 67 | 0 | 10 |
| Arrays | 38 | 8 | 0 | 30 |
| Strings | 28 | 23 | 0 | 5 |
| Sequences | 56 | 36 | 0 | 20 |
| Hash Tables | 17 | 11 | 0 | 6 |
| Filenames | 26 | 7 | 0 | 19 |
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

**Total: 942 symbols** (actual count)
- **Implemented: ~646 (69%)**
- **Partial: ~15 (1.6%)**
- **Missing: ~281 (30%)**

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
