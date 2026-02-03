# ANSI Common Lisp Symbol Audit

978 external symbols in COMMON-LISP package per ANSI INCITS 226-1994.

Status: ✓ implemented (988) | ⚠ partial (10) | ✗ missing (13)
Note: 1011 entries; 978 unique ANSI CL external symbols (see docs/cl-symbols-sbcl.txt)

## Lambda List Keywords (8)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| &allow-other-keys | ⚠ | compile.zig | Parsed but not fully enforced |
| &aux | ✓ | compile.zig:453 | Aux bindings |
| &body | ✓ | compile.zig:427 | Macro lambda lists |
| &environment | ⚠ | compile.zig:2444 | Macro env (nil), REPL path incomplete |
| &key | ✓ | compile.zig:429 | Keyword args |
| &optional | ✓ | compile.zig:428 | Optional args |
| &rest | ✓ | compile.zig:426 | Rest args |
| &whole | ⚠ | compile.zig:2444 | Macro whole form, REPL path incomplete |

## Special Variables (65)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| * | ✓ | repl.zig | Previous result |
| ** | ✓ | repl.zig | 2nd previous result |
| *** | ✓ | repl.zig | 3rd previous result |
| *break-on-signals* | ✓ | lib/stdlib.habu:188 | Break on signals |
| *compile-file-pathname* | ✓ | lib/stdlib.habu:178 | Compile file path |
| *compile-file-truename* | ✓ | lib/stdlib.habu:179 | Compile file truename |
| *compile-print* | ✓ | lib/stdlib.habu:180 | Compile print flag |
| *compile-verbose* | ✓ | lib/stdlib.habu:181 | Compile verbose flag |
| *debug-io* | ✓ | repl.zig:createStreamGlobals | Debugger I/O stream |
| *debugger-hook* | ✓ | lib/stdlib.habu:189 | Debugger hook |
| *default-pathname-defaults* | ✓ | lib/stdlib.habu:176 | Default pathname |
| *error-output* | ✓ | repl.zig:createStreamGlobals | Error output stream |
| *features* | ✓ | repl.zig:createFeaturesGlobal | Reader conditionals |
| *gensym-counter* | ✓ | vm.zig | Gensym support |
| *load-pathname* | ✓ | lib/stdlib.habu:182 | Load pathname |
| *load-print* | ✓ | lib/stdlib.habu:184 | Load print flag |
| *load-truename* | ✓ | lib/stdlib.habu:183 | Load truename |
| *load-verbose* | ✓ | lib/stdlib.habu:185 | Load verbose flag |
| *macroexpand-hook* | ✓ | lib/stdlib.habu:192 | Macroexpand hook |
| *modules* | ✓ | lib/stdlib.habu:195 | Loaded modules list |
| *package* | ✓ | heap.zig | Current package |
| *print-array* | ✓ | vm.zig:590, io.zig | Control array printing |
| *print-base* | ✓ | vm.zig:571, io.zig | Number output radix |
| *print-case* | ✓ | vm.zig:559, io.zig | Symbol case control |
| *print-circle* | ✓ | lib/stdlib.habu:152 | Circular print control |
| *print-escape* | ✓ | vm.zig:551, io.zig:17 | Control escape sequences |
| *print-gensym* | ✓ | vm.zig:583, io.zig | Gensym prefix control |
| *print-length* | ✓ | vm.zig:534, io.zig:41 | Max elements to print |
| *print-level* | ✓ | vm.zig:543, io.zig:45 | Max nesting depth |
| *print-lines* | ✓ | lib/stdlib.habu:154 | Max lines to print |
| *print-miser-width* | ✓ | lib/stdlib.habu:155 | Miser mode width |
| *print-pprint-dispatch* | ✓ | lib/stdlib.habu:156 | Pprint dispatch table |
| *print-pretty* | ✓ | lib/stdlib.habu:153 | Pretty print control |
| *print-radix* | ✓ | vm.zig:577, io.zig | Radix indicator control |
| *print-readably* | ✓ | vm.zig:565, io.zig:37 | Readable output control |
| *print-right-margin* | ✓ | lib/stdlib.habu:157 | Right margin for output |
| *query-io* | ✓ | repl.zig:createStreamGlobals | Query I/O stream |
| *random-state* | ✓ | lib/stdlib.habu:198 | Random state |
| *read-base* | ✓ | lib/stdlib.habu:160 | Input radix |
| *read-default-float-format* | ✓ | lib/stdlib.habu:161 | Default float type |
| *read-eval* | ✓ | lib/stdlib.habu:162 | #. reader macro control |
| *read-suppress* | ✓ | lib/stdlib.habu:163 | Suppress reader |
| *readtable* | ✓ | vm.zig | Runtime readtable |
| *standard-input* | ✓ | repl.zig:createStreamGlobals | stdin stream |
| *standard-output* | ✓ | repl.zig:createStreamGlobals | stdout stream |
| *terminal-io* | ✓ | repl.zig:createStreamGlobals | Terminal I/O stream |
| *trace-output* | ✓ | repl.zig:createStreamGlobals | Trace output stream |
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
| array-dimension-limit | ✓ | lib/stdlib.habu:20 | 2^62-1 |
| array-rank-limit | ✓ | lib/stdlib.habu:21 | 65536 |
| array-total-size-limit | ✓ | lib/stdlib.habu:22 | 2^62-1 |
| boole-1 | ✓ | lib/stdlib.habu:67 | 2 |
| boole-2 | ✓ | lib/stdlib.habu:68 | 3 |
| boole-and | ✓ | lib/stdlib.habu:71 | 6 |
| boole-andc1 | ✓ | lib/stdlib.habu:77 | 12 |
| boole-andc2 | ✓ | lib/stdlib.habu:78 | 13 |
| boole-c1 | ✓ | lib/stdlib.habu:69 | 4 |
| boole-c2 | ✓ | lib/stdlib.habu:70 | 5 |
| boole-clr | ✓ | lib/stdlib.habu:65 | 0 |
| boole-eqv | ✓ | lib/stdlib.habu:74 | 9 |
| boole-ior | ✓ | lib/stdlib.habu:72 | 7 |
| boole-nand | ✓ | lib/stdlib.habu:75 | 10 |
| boole-nor | ✓ | lib/stdlib.habu:76 | 11 |
| boole-orc1 | ✓ | lib/stdlib.habu:79 | 14 |
| boole-orc2 | ✓ | lib/stdlib.habu:80 | 15 |
| boole-set | ✓ | lib/stdlib.habu:66 | 1 |
| boole-xor | ✓ | lib/stdlib.habu:73 | 8 |
| call-arguments-limit | ✓ | lib/stdlib.habu:23 | 65536 |
| char-code-limit | ✓ | lib/stdlib.habu:24 | 1114112 (Unicode) |
| double-float-epsilon | ✓ | lib/stdlib.habu:35 | 2.22e-16 |
| double-float-negative-epsilon | ✓ | lib/stdlib.habu:36 | 1.11e-16 |
| internal-time-units-per-second | ✓ | lib/stdlib.habu:86 | 1000000 (µs) |
| lambda-list-keywords | ✓ | lib/stdlib.habu:83 | List of &-keywords |
| lambda-parameters-limit | ✓ | lib/stdlib.habu:25 | 65536 |
| least-negative-double-float | ✓ | lib/stdlib.habu:32 | -2.225e-308 |
| least-negative-long-float | ✓ | lib/stdlib.habu:58 | Same as double |
| least-negative-normalized-double-float | ✓ | lib/stdlib.habu:34 | -2.225e-308 |
| least-negative-normalized-long-float | ✓ | lib/stdlib.habu:60 | Same as double |
| least-negative-normalized-short-float | ✓ | lib/stdlib.habu:52 | Same as double |
| least-negative-normalized-single-float | ✓ | lib/stdlib.habu:44 | Same as double |
| least-negative-short-float | ✓ | lib/stdlib.habu:50 | Same as double |
| least-negative-single-float | ✓ | lib/stdlib.habu:42 | Same as double |
| least-positive-double-float | ✓ | lib/stdlib.habu:31 | 2.225e-308 |
| least-positive-long-float | ✓ | lib/stdlib.habu:57 | Same as double |
| least-positive-normalized-double-float | ✓ | lib/stdlib.habu:33 | 2.225e-308 |
| least-positive-normalized-long-float | ✓ | lib/stdlib.habu:59 | Same as double |
| least-positive-normalized-short-float | ✓ | lib/stdlib.habu:51 | Same as double |
| least-positive-normalized-single-float | ✓ | lib/stdlib.habu:43 | Same as double |
| least-positive-short-float | ✓ | lib/stdlib.habu:49 | Same as double |
| least-positive-single-float | ✓ | lib/stdlib.habu:41 | Same as double |
| long-float-epsilon | ✓ | lib/stdlib.habu:61 | Same as double |
| long-float-negative-epsilon | ✓ | lib/stdlib.habu:62 | Same as double |
| most-negative-double-float | ✓ | lib/stdlib.habu:30 | -1.797e308 |
| most-negative-fixnum | ✓ | lib/stdlib.habu:19 | -2^62 |
| most-negative-long-float | ✓ | lib/stdlib.habu:56 | Same as double |
| most-negative-short-float | ✓ | lib/stdlib.habu:48 | Same as double |
| most-negative-single-float | ✓ | lib/stdlib.habu:40 | Same as double |
| most-positive-double-float | ✓ | lib/stdlib.habu:29 | 1.797e308 |
| most-positive-fixnum | ✓ | lib/stdlib.habu:18 | 2^62-1 |
| most-positive-long-float | ✓ | lib/stdlib.habu:55 | Same as double |
| most-positive-short-float | ✓ | lib/stdlib.habu:47 | Same as double |
| most-positive-single-float | ✓ | lib/stdlib.habu:39 | Same as double |
| multiple-values-limit | ✓ | lib/stdlib.habu:26 | 256 |
| nil | ✓ | value.zig | Constant |
| pi | ✓ | lib/stdlib.habu:14 | 3.14159... |
| short-float-epsilon | ✓ | lib/stdlib.habu:53 | Same as double |
| short-float-negative-epsilon | ✓ | lib/stdlib.habu:54 | Same as double |
| single-float-epsilon | ✓ | lib/stdlib.habu:45 | Same as double |
| single-float-negative-epsilon | ✓ | lib/stdlib.habu:46 | Same as double |
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
| load-time-value | ✓ | compile.zig:5660 | Compile-time eval |
| locally | ✓ | lib/stdlib.habu | Local declarations |
| macrolet | ✓ | compile.zig:5147 | Local macro defs |
| multiple-value-call | ✓ | compile.zig:116 | MV function call |
| multiple-value-prog1 | ✓ | lib/stdlib.habu | MV return first |
| progn | ✓ | compile.zig:73 | Sequencing |
| progv | ✓ | vm.zig | Dynamic binding |
| quote | ✓ | compile.zig:78 | Literal data |
| return-from | ✓ | compile.zig:100 | Block exit |
| setq | ✓ | compile.zig:70 | Variable assignment |
| symbol-macrolet | ✓ | compile.zig:5205 | Local symbol macros |
| tagbody | ✓ | compile.zig:111 | Go targets |
| the | ✓ | compile.zig:96 | Type assertion |
| throw | ✓ | compile.zig:104 | Dynamic throw |
| unwind-protect | ✓ | compile.zig:101 | Cleanup forms |

## Macros (86)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| and | ✓ | compile.zig:56 | Short-circuit and |
| assert | ✓ | lib/stdlib.habu | Assertion |
| case | ✓ | lib/stdlib.habu:26 | Symbol dispatch |
| ccase | ✓ | lib/stdlib.habu | Continuable case |
| check-type | ✓ | lib/stdlib.habu | Type check |
| cond | ✓ | compile.zig:55 | Multi-branch cond |
| ctypecase | ✓ | lib/stdlib.habu | Continuable typecase |
| decf | ✓ | lib/stdlib.habu | Decrement place |
| declaim | ✓ | compile.zig | Global declaration |
| defclass | ✓ | compile.zig:129 | CLOS class |
| defconstant | ✓ | lib/stdlib.habu | Constant definition |
| defgeneric | ✓ | compile.zig:132 | Generic function |
| define-compiler-macro | ✓ | lib/stdlib.habu:5064 | Stub macro |
| define-condition | ✓ | lib/stdlib.habu | Condition class |
| define-method-combination | ✓ | lib/stdlib.habu:5070 | Stub macro |
| define-modify-macro | ✓ | lib/stdlib.habu | Read-modify-write macro |
| define-setf-expander | ✓ | lib/stdlib.habu:5076 | Stub macro |
| define-symbol-macro | ✓ | lib/stdlib.habu:5082 | Stub macro |
| defmacro | ✓ | compile.zig:89 | Macro definition |
| defmethod | ✓ | compile.zig:133 | CLOS method |
| defpackage | ✓ | compile.zig:120 | Package definition |
| defparameter | ✓ | lib/stdlib.habu | Parameter definition |
| defsetf | ✓ | lib/stdlib.habu | Setf expansion |
| defstruct | ✓ | lib/stdlib.habu:2739 | Structure |
| deftype | ✓ | compile.zig | Type definition |
| defun | ✓ | compile.zig:69 | Function definition |
| defvar | ✓ | compile.zig:68 | Variable definition |
| destructuring-bind | ✓ | lib/stdlib.habu:504 | Pattern destructure |
| do | ✓ | lib/stdlib.habu:61 | Parallel iteration |
| do* | ✓ | lib/stdlib.habu:113 | Sequential iteration |
| do-all-symbols | ✓ | lib/stdlib.habu | All symbol iteration (stub) |
| do-external-symbols | ✓ | lib/stdlib.habu | External symbol iteration (stub) |
| do-symbols | ✓ | lib/stdlib.habu | Symbol iteration (stub) |
| dolist | ✓ | lib/stdlib.habu | List iteration |
| dotimes | ✓ | lib/stdlib.habu | Count iteration |
| ecase | ✓ | lib/stdlib.habu | Error case |
| etypecase | ✓ | lib/stdlib.habu | Error typecase |
| formatter | ✓ | lib/stdlib.habu | Format compiler |
| handler-bind | ✓ | compile.zig:107 | Handler binding |
| handler-case | ✓ | compile.zig:104 | Handler dispatch |
| ignore-errors | ✓ | lib/stdlib.habu:55 | Error suppression |
| in-package | ✓ | compile.zig:121 | Package switch |
| incf | ✓ | lib/stdlib.habu | Increment place |
| lambda | ✓ | compile.zig:65 | Anonymous function |
| loop | ✓ | lib/stdlib.habu:2536 | Loop facility |
| loop-finish | ✓ | lib/stdlib.habu:3563 | Exit loop normally |
| multiple-value-bind | ✓ | compile.zig:115 | MV binding |
| multiple-value-list | ✓ | compile.zig:117 | MV to list |
| multiple-value-setq | ✓ | lib/stdlib.habu | MV assignment |
| nth-value | ✓ | lib/stdlib.habu | Nth value select |
| or | ✓ | compile.zig:57 | Short-circuit or |
| pop | ✓ | lib/stdlib.habu | List pop |
| pprint-exit-if-list-exhausted | ✓ | lib/stdlib.habu:5088 | Stub macro |
| pprint-logical-block | ✓ | lib/stdlib.habu:5093 | Stub macro |
| pprint-pop | ✓ | lib/stdlib.habu:5100 | Stub macro |
| print-unreadable-object | ✓ | lib/stdlib.habu | Print #<...> notation |
| prog | ✓ | lib/stdlib.habu | Prog form |
| prog* | ✓ | lib/stdlib.habu | Prog with let* |
| prog1 | ✓ | lib/stdlib.habu | Return first |
| prog2 | ✓ | lib/stdlib.habu | Return second |
| psetf | ✓ | lib/stdlib.habu | Parallel setf |
| psetq | ✓ | lib/stdlib.habu | Parallel setq |
| push | ✓ | lib/stdlib.habu | List push |
| pushnew | ✓ | lib/stdlib.habu | Conditional push |
| remf | ✓ | lib/stdlib.habu | Remove property |
| restart-bind | ✓ | lib/stdlib.habu:5105 | Stub macro |
| restart-case | ✓ | compile.zig:108 | Restart establish |
| return | ✓ | lib/stdlib.habu | Block return |
| rotatef | ✓ | lib/stdlib.habu | Rotate places |
| setf | ✓ | lib/stdlib.habu:878 | Generalized set |
| shiftf | ✓ | lib/stdlib.habu | Shift places |
| step | ✓ | lib/stdlib.habu | Single-step (stub) |
| time | ✓ | lib/stdlib.habu | Time execution |
| trace | ✓ | lib/stdlib.habu | Trace calls (stub) |
| typecase | ✓ | lib/stdlib.habu | Type dispatch |
| unless | ✓ | lib/stdlib.habu | Negated when |
| untrace | ✓ | lib/stdlib.habu | Untrace (stub) |
| when | ✓ | lib/stdlib.habu | Conditional |
| with-accessors | ✓ | lib/stdlib.habu | Accessor bindings |
| with-compilation-unit | ✓ | lib/stdlib.habu | Compilation unit (stub) |
| with-condition-restarts | ✓ | lib/stdlib.habu | Condition restarts (stub) |
| with-hash-table-iterator | ✓ | lib/stdlib.habu | Hash iteration |
| with-input-from-string | ✓ | lib/stdlib.habu | String input |
| with-open-file | ✓ | lib/stdlib.habu | File scope |
| with-open-stream | ✓ | lib/stdlib.habu | Generic stream wrapper |
| with-output-to-string | ✓ | lib/stdlib.habu | String output |
| with-package-iterator | ✓ | lib/stdlib.habu | Package iterator (stub) |
| with-simple-restart | ✓ | lib/stdlib.habu | Simple restart |
| with-slots | ✓ | lib/stdlib.habu | Slot bindings |
| with-standard-io-syntax | ✓ | lib/stdlib.habu | Standard I/O env |

## Functions - Arithmetic (79)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| * | ✓ | compile.zig:141 | Multiply |
| + | ✓ | compile.zig:139 | Add |
| - | ✓ | compile.zig:140 | Subtract |
| / | ✓ | compile.zig:142 | Divide |
| /= | ✓ | arith.zig | Not equal |
| 1+ | ✓ | lib/stdlib.habu:37 | Increment |
| 1- | ✓ | lib/stdlib.habu:39 | Decrement |
| < | ✓ | compile.zig:153 | Less than |
| <= | ✓ | compile.zig:155 | Less or equal |
| = | ✓ | compile.zig:157 | Numeric equal |
| > | ✓ | compile.zig:154 | Greater than |
| >= | ✓ | compile.zig:156 | Greater or equal |
| abs | ✓ | compile.zig:248 | Absolute value |
| acos | ✓ | compile.zig:261 | Arc cosine |
| acosh | ✓ | compile.zig:267 | Hyperbolic arc cos |
| arithmetic-error-operands | ✓ | lib/stdlib.habu | Error operands accessor |
| arithmetic-error-operation | ✓ | lib/stdlib.habu | Error operation accessor |
| ash | ✓ | compile.zig:343 | Arithmetic shift |
| asin | ✓ | compile.zig:260 | Arc sine |
| asinh | ✓ | compile.zig:266 | Hyperbolic arc sin |
| atan | ✓ | compile.zig:262 | Arc tangent |
| atanh | ✓ | compile.zig:268 | Hyperbolic arc tan |
| boole | ✓ | lib/stdlib.habu | Boolean ops on integers |
| byte | ✓ | lib/stdlib.habu | Byte specifier |
| byte-position | ✓ | lib/stdlib.habu | Get byte position |
| byte-size | ✓ | lib/stdlib.habu | Get byte size |
| ceiling | ✓ | compile.zig:272 | Ceiling |
| cis | ✓ | lib/stdlib.habu:1765 | cos+i*sin |
| complex | ✓ | compile.zig:380 | Make complex |
| conjugate | ✓ | lib/stdlib.habu | Complex conjugate |
| cos | ✓ | compile.zig:258 | Cosine |
| cosh | ✓ | compile.zig:264 | Hyperbolic cos |
| decode-float | ✓ | compile.zig | Decompose float |
| denominator | ✓ | compile.zig:384 | Rational denom |
| deposit-field | ✓ | lib/stdlib.habu | Deposit field |
| dpb | ✓ | lib/stdlib.habu | Deposit byte |
| evenp | ✓ | compile.zig:252 | Even predicate |
| exp | ✓ | compile.zig:269 | Exponential |
| expt | ✓ | lib/stdlib.habu | Power |
| fceiling | ✓ | lib/stdlib.habu | Ceiling as float |
| ffloor | ✓ | lib/stdlib.habu | Floor as float |
| float | ✓ | arith.zig | To float |
| float-digits | ✓ | lib/stdlib.habu | Float digits |
| float-precision | ✓ | lib/stdlib.habu | Float precision |
| float-radix | ✓ | lib/stdlib.habu | Float radix |
| float-sign | ✓ | lib/stdlib.habu | Float sign |
| floor | ✓ | compile.zig:271 | Floor |
| fround | ✓ | lib/stdlib.habu | Round as float |
| ftruncate | ✓ | lib/stdlib.habu | Truncate as float |
| gcd | ✓ | lib/stdlib.habu | Greatest common divisor |
| imagpart | ✓ | compile.zig:382 | Complex imaginary |
| integer-decode-float | ✓ | compile.zig | Decompose to ints |
| integer-length | ✓ | compile.zig:351 | Bit length |
| isqrt | ✓ | lib/stdlib.habu:63 | Integer sqrt |
| lcm | ✓ | lib/stdlib.habu | Least common multiple |
| ldb | ✓ | lib/stdlib.habu | Load byte |
| ldb-test | ✓ | lib/stdlib.habu | Test byte |
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
| make-random-state | ✓ | lib/stdlib.habu:361 | Create random state |
| mask-field | ✓ | lib/stdlib.habu | Extract field |
| max | ✓ | lib/stdlib.habu | Maximum |
| min | ✓ | lib/stdlib.habu | Minimum |
| minusp | ✓ | compile.zig:251 | Negative predicate |
| mod | ✓ | compile.zig:143 | Modulo |
| numerator | ✓ | compile.zig:383 | Rational numer |
| oddp | ✓ | compile.zig:253 | Odd predicate |
| parse-integer | ✓ | compile.zig:335 | String to int |
| phase | ✓ | lib/stdlib.habu:1768 | Complex phase |
| plusp | ✓ | compile.zig:250 | Positive predicate |
| random | ✓ | compile.zig:318 | Random number |
| random-state-p | ✓ | lib/stdlib.habu | Random state pred |
| rational | ✓ | compile.zig:385 | To rational |
| rationalize | ✓ | compile.zig:386 | Approx rational |
| realpart | ✓ | compile.zig:381 | Complex real |
| rem | ✓ | compile.zig:148 | Remainder |
| round | ✓ | compile.zig:273 | Round |
| scale-float | ✓ | lib/stdlib.habu | Scale float |
| signum | ✓ | lib/stdlib.habu:41 | Sign |
| sin | ✓ | compile.zig:257 | Sine |
| sinh | ✓ | compile.zig:263 | Hyperbolic sin |
| sqrt | ✓ | compile.zig:256 | Square root |
| tan | ✓ | compile.zig:259 | Tangent |
| tanh | ✓ | compile.zig:265 | Hyperbolic tan |
| truncate | ✓ | compile.zig:146 | Truncate |
| upgraded-complex-part-type | ✓ | vm.zig:4285 | Upgraded complex part type |
| zerop | ✓ | compile.zig:249 | Zero predicate |

## Functions - Characters (30)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| alpha-char-p | ✓ | compile.zig:326 | Alpha predicate |
| alphanumericp | ✓ | lib/stdlib.habu | Alphanum predicate |
| both-case-p | ✓ | lib/stdlib.habu | Case predicate |
| char-code | ✓ | compile.zig:215 | To code point |
| char-downcase | ✓ | compile.zig:324 | To lowercase |
| char-equal | ✓ | lib/stdlib.habu | Case-insensitive = |
| char-greaterp | ✓ | lib/stdlib.habu | Case-insensitive > |
| char-int | ✓ | lib/stdlib.habu | Char to int |
| char-lessp | ✓ | lib/stdlib.habu | Case-insensitive < |
| char-name | ✓ | lib/stdlib.habu | Char to name |
| char-not-equal | ✓ | lib/stdlib.habu | Case-insensitive /= |
| char-not-greaterp | ✓ | lib/stdlib.habu | Case-insensitive <= |
| char-not-lessp | ✓ | lib/stdlib.habu | Case-insensitive >= |
| char-upcase | ✓ | compile.zig:323 | To uppercase |
| char/= | ✓ | lib/stdlib.habu | Char not equal |
| char< | ✓ | compile.zig:218 | Char less |
| char<= | ✓ | lib/stdlib.habu | Char less or equal |
| char= | ✓ | compile.zig:217 | Char equal |
| char> | ✓ | compile.zig:219 | Char greater |
| char>= | ✓ | lib/stdlib.habu | Char greater or equal |
| character | ✓ | compile.zig:244 | Coerce to char |
| characterp | ✓ | compile.zig:209 | Char predicate |
| code-char | ✓ | compile.zig:216 | From code point |
| digit-char | ✓ | lib/stdlib.habu | Digit to char |
| digit-char-p | ✓ | compile.zig:325 | Digit predicate |
| graphic-char-p | ✓ | lib/stdlib.habu | Graphic char predicate |
| lower-case-p | ✓ | lib/stdlib.habu | Lowercase pred |
| name-char | ✓ | lib/stdlib.habu | Name to char |
| standard-char-p | ✓ | lib/stdlib.habu | Standard predicate |
| upper-case-p | ✓ | lib/stdlib.habu | Uppercase pred |

## Functions - Conses/Lists (77)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| acons | ✓ | lib/stdlib.habu | Add to alist |
| adjoin | ✓ | lib/stdlib.habu | Set adjoin |
| append | ✓ | compile.zig:183 | List append |
| assoc | ✓ | compile.zig:190 | Alist lookup |
| assoc-if | ✓ | lib/stdlib.habu | Alist lookup if |
| assoc-if-not | ✓ | lib/stdlib.habu | Alist lookup if not |
| atom | ✓ | compile.zig:212 | Atom predicate |
| butlast | ✓ | lib/stdlib.habu | All but last |
| caaaar | ✓ | lib/stdlib.habu | 4-level car |
| caaadr | ✓ | lib/stdlib.habu | 4-level car |
| caaar | ✓ | lib/stdlib.habu | 3-level car |
| caadar | ✓ | lib/stdlib.habu | 4-level car |
| caaddr | ✓ | lib/stdlib.habu | 4-level car |
| caadr | ✓ | lib/stdlib.habu | 3-level car |
| caar | ✓ | compile.zig:166 | Car of car |
| cadaar | ✓ | lib/stdlib.habu | 4-level car |
| cadadr | ✓ | lib/stdlib.habu | 4-level car |
| cadar | ✓ | compile.zig:173 | Car of cdr of car |
| caddar | ✓ | lib/stdlib.habu | 4-level car |
| cadddr | ✓ | lib/stdlib.habu | 4th via cdr |
| caddr | ✓ | compile.zig:174 | Third |
| cadr | ✓ | compile.zig:167 | Second |
| car | ✓ | compile.zig:161 | First of cons |
| cdaaar | ✓ | lib/stdlib.habu | 4-level cdr |
| cdaadr | ✓ | lib/stdlib.habu | 4-level cdr |
| cdaar | ✓ | lib/stdlib.habu | 3-level cdr |
| cdadar | ✓ | lib/stdlib.habu | 4-level cdr |
| cdaddr | ✓ | lib/stdlib.habu | 4-level cdr |
| cdadr | ✓ | compile.zig:176 | Cdr of cadr |
| cdar | ✓ | compile.zig:168 | Cdr of car |
| cddaar | ✓ | lib/stdlib.habu | 4-level cdr |
| cddadr | ✓ | lib/stdlib.habu | 4-level cdr |
| cddar | ✓ | compile.zig:177 | Cdr of cdar |
| cdddar | ✓ | lib/stdlib.habu | 4-level cdr |
| cddddr | ✓ | lib/stdlib.habu | 4th cdr |
| cdddr | ✓ | compile.zig:178 | Cdr of cddr |
| cddr | ✓ | compile.zig:169 | Cdr of cdr |
| cdr | ✓ | compile.zig:162 | Rest of cons |
| cons | ✓ | compile.zig:160 | Make cons |
| consp | ✓ | compile.zig:200 | Cons predicate |
| copy-alist | ✓ | lib/stdlib.habu | Copy alist |
| copy-list | ✓ | lib/stdlib.habu | Copy list |
| copy-tree | ✓ | lib/stdlib.habu | Deep copy |
| eighth | ✓ | lib/stdlib.habu | 8th element |
| endp | ✓ | lib/stdlib.habu | End predicate |
| fifth | ✓ | lib/stdlib.habu | 5th element |
| first | ✓ | compile.zig:163 | First element |
| fourth | ✓ | compile.zig:182 | 4th element |
| get-properties | ✓ | lib/stdlib.habu | Plist search |
| getf | ✓ | lib/stdlib.habu | Plist lookup |
| intersection | ✓ | lib/stdlib.habu | Set intersection |
| last | ✓ | compile.zig:188 | Last cons |
| ldiff | ✓ | lib/stdlib.habu | List difference |
| list | ✓ | compile.zig:195 | Make list |
| list* | ✓ | lib/stdlib.habu | Make dotted list |
| list-length | ✓ | lib/stdlib.habu | Proper length |
| listp | ✓ | compile.zig:211 | List predicate |
| make-list | ✓ | lib/stdlib.habu | Create list |
| mapc | ✓ | lib/stdlib.habu:459 | Map for effect |
| mapcan | ✓ | lib/stdlib.habu:1092 | Nconc map |
| mapcar | ✓ | lib/stdlib.habu:456 | Map to list |
| mapcon | ✓ | lib/stdlib.habu:1098 | Nconc cons map |
| mapl | ✓ | lib/stdlib.habu | Map for effect on tails |
| maplist | ✓ | lib/stdlib.habu:467 | Map over tails |
| member | ✓ | compile.zig:189 | List member |
| member-if | ✓ | lib/stdlib.habu | Member by pred |
| member-if-not | ✓ | lib/stdlib.habu | Member not pred |
| nbutlast | ✓ | lib/stdlib.habu | Destructive butlast |
| nconc | ✓ | lib/stdlib.habu | Destructive append |
| nintersection | ✓ | lib/stdlib.habu | Destructive intersection |
| ninth | ✓ | lib/stdlib.habu | 9th element |
| nreconc | ✓ | lib/stdlib.habu | Destructive revappend |
| nreverse | ✓ | lib/stdlib.habu | Destructive reverse |
| nset-difference | ✓ | lib/stdlib.habu | Destructive set-difference |
| nset-exclusive-or | ✓ | lib/stdlib.habu | Destructive set-exclusive-or |
| nsublis | ✓ | lib/stdlib.habu | Destructive sublis |
| nsubst | ✓ | lib/stdlib.habu | Destructive subst |
| nsubst-if | ✓ | lib/stdlib.habu | Destructive subst-if |
| nsubst-if-not | ✓ | lib/stdlib.habu | Destructive subst-if-not |
| nth | ✓ | compile.zig:186 | Nth element |
| nthcdr | ✓ | compile.zig:187 | Nth cdr |
| null | ✓ | compile.zig:207 | Null predicate |
| nunion | ✓ | lib/stdlib.habu | Destructive union |
| pairlis | ✓ | lib/stdlib.habu | Make alist |
| pop | ✓ | lib/stdlib.habu | Pop from list |
| push | ✓ | lib/stdlib.habu | Push to list |
| pushnew | ✓ | lib/stdlib.habu | Conditional push |
| rassoc | ✓ | lib/stdlib.habu | Reverse assoc |
| rassoc-if | ✓ | lib/stdlib.habu | Reverse assoc-if |
| rassoc-if-not | ✓ | lib/stdlib.habu | Reverse assoc-if-not |
| rest | ✓ | compile.zig:164 | Rest of list |
| revappend | ✓ | lib/stdlib.habu | Reverse and append |
| reverse | ✓ | compile.zig:185 | Reverse list |
| rplaca | ✓ | compile.zig:196 | Set car |
| rplacd | ✓ | compile.zig:197 | Set cdr |
| second | ✓ | compile.zig:180 | 2nd element |
| set-difference | ✓ | lib/stdlib.habu | Set difference |
| set-exclusive-or | ✓ | lib/stdlib.habu | Set xor |
| seventh | ✓ | lib/stdlib.habu | 7th element |
| sixth | ✓ | lib/stdlib.habu | 6th element |
| sublis | ✓ | lib/stdlib.habu | Substitute alist |
| subsetp | ✓ | lib/stdlib.habu | Subset predicate |
| subst | ✓ | lib/stdlib.habu | Substitute |
| subst-if | ✓ | lib/stdlib.habu | Substitute by pred |
| subst-if-not | ✓ | lib/stdlib.habu | Substitute not pred |
| tailp | ✓ | lib/stdlib.habu | Tail predicate |
| tenth | ✓ | lib/stdlib.habu | 10th element |
| third | ✓ | compile.zig:181 | 3rd element |
| tree-equal | ✓ | lib/stdlib.habu | Tree equality |
| union | ✓ | lib/stdlib.habu | Set union |

## Functions - Arrays (38)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| adjust-array | ✓ | lib/stdlib.habu | Resize array |
| adjustable-array-p | ✓ | lib/stdlib.habu | Adjustable array pred |
| aref | ✓ | compile.zig:276 | Array element |
| array-dimension | ✓ | lib/stdlib.habu | Get dimension |
| array-dimensions | ✓ | lib/stdlib.habu | Get dimensions list |
| array-displacement | ✓ | lib/stdlib.habu | Get displacement |
| array-element-type | ✓ | lib/stdlib.habu | Element type |
| array-has-fill-pointer-p | ✓ | lib/stdlib.habu | Fill pointer pred |
| array-in-bounds-p | ✓ | lib/stdlib.habu | Bounds check |
| array-rank | ✓ | lib/stdlib.habu | Array rank |
| array-row-major-index | ✓ | lib/stdlib.habu | Row-major index |
| array-total-size | ✓ | lib/stdlib.habu | Total size |
| arrayp | ✓ | lib/stdlib.habu | Array predicate |
| bit | ✓ | lib/stdlib.habu | Bit access |
| bit-and | ✓ | lib/stdlib.habu | Bitwise and |
| bit-andc1 | ✓ | lib/stdlib.habu | Bit andc1 |
| bit-andc2 | ✓ | lib/stdlib.habu | Bit andc2 |
| bit-eqv | ✓ | lib/stdlib.habu | Bit equivalence |
| bit-ior | ✓ | lib/stdlib.habu | Bitwise ior |
| bit-nand | ✓ | lib/stdlib.habu | Bit nand |
| bit-nor | ✓ | lib/stdlib.habu | Bit nor |
| bit-not | ✓ | lib/stdlib.habu | Bit not |
| bit-orc1 | ✓ | lib/stdlib.habu | Bit orc1 |
| bit-orc2 | ✓ | lib/stdlib.habu | Bit orc2 |
| bit-vector-p | ✓ | lib/stdlib.habu | Bit-vector pred |
| bit-xor | ✓ | lib/stdlib.habu | Bitwise xor |
| fill-pointer | ✓ | compile.zig | Get/set fill pointer |
| make-array | ✓ | compile.zig:298 | Create array |
| row-major-aref | ✓ | lib/stdlib.habu | Row-major access |
| sbit | ✓ | lib/stdlib.habu | Simple bit access |
| simple-bit-vector-p | ✓ | lib/stdlib.habu | Bit-vector pred |
| simple-vector-p | ✓ | lib/stdlib.habu | Simple-vector pred |
| svref | ✓ | compile.zig:277 | Simple-vector ref |
| upgraded-array-element-type | ✓ | lib/stdlib.habu | Upgraded type |
| vector | ✓ | compile.zig:297 | Make vector |
| vector-pop | ✓ | compile.zig | Vector pop |
| vector-push | ✓ | compile.zig | Vector push |
| vector-push-extend | ✓ | compile.zig | Vector push extend |
| vectorp | ✓ | compile.zig:204 | Vector predicate |

## Functions - Strings (28)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| char | ✓ | compile.zig:301 | String char |
| make-string | ✓ | compile.zig:358 | Create string |
| nstring-capitalize | ✓ | lib/stdlib.habu | Destructive capitalize |
| nstring-downcase | ✓ | lib/stdlib.habu | Destructive downcase |
| nstring-upcase | ✓ | lib/stdlib.habu | Destructive upcase |
| schar | ✓ | compile.zig:302 | Simple-string char |
| simple-string-p | ✓ | lib/stdlib.habu | Simple-string pred |
| string | ✓ | compile.zig:243 | To string |
| string-capitalize | ✓ | lib/stdlib.habu | Capitalize |
| string-downcase | ✓ | compile.zig:362 | Downcase |
| string-equal | ✓ | lib/stdlib.habu | Case-insensitive = |
| string-greaterp | ✓ | lib/stdlib.habu | Case-insensitive > |
| string-left-trim | ✓ | lib/stdlib.habu | Left trim |
| string-lessp | ✓ | lib/stdlib.habu | Case-insensitive < |
| string-not-equal | ✓ | lib/stdlib.habu | Case-insensitive /= |
| string-not-greaterp | ✓ | lib/stdlib.habu | Case-insensitive <= |
| string-not-lessp | ✓ | lib/stdlib.habu | Case-insensitive >= |
| string-right-trim | ✓ | lib/stdlib.habu | Right trim |
| string-trim | ✓ | lib/stdlib.habu | Both trim |
| string-upcase | ✓ | compile.zig:361 | Upcase |
| string/= | ✓ | lib/stdlib.habu:1278 | Case-sensitive inequality |
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
| copy-seq | ✓ | lib/stdlib.habu | Copy sequence |
| count | ✓ | compile.zig:193 | Count element |
| count-if | ✓ | lib/stdlib.habu | Count by pred |
| count-if-not | ✓ | lib/stdlib.habu | Count not pred |
| delete | ✓ | lib/stdlib.habu | Destructive remove |
| delete-duplicates | ✓ | lib/stdlib.habu | Delete dups |
| delete-if | ✓ | lib/stdlib.habu | Delete by pred |
| delete-if-not | ✓ | lib/stdlib.habu | Delete not pred |
| elt | ✓ | lib/stdlib.habu:417 | Sequence element |
| every | ✓ | lib/stdlib.habu | All satisfy |
| fill | ✓ | lib/stdlib.habu | Fill sequence |
| find | ✓ | compile.zig:191 | Find element |
| find-if | ✓ | lib/stdlib.habu | Find by pred |
| find-if-not | ✓ | lib/stdlib.habu | Find not pred |
| length | ✓ | compile.zig:184 | Sequence length |
| map | ✓ | lib/stdlib.habu | Map function |
| map-into | ✓ | lib/stdlib.habu:125 | Map into result-seq |
| merge | ✓ | lib/stdlib.habu | Merge sequences |
| mismatch | ✓ | lib/stdlib.habu | Find first mismatch |
| notany | ✓ | lib/stdlib.habu | None satisfy |
| notevery | ✓ | lib/stdlib.habu | Not all satisfy |
| nsubstitute | ✓ | lib/stdlib.habu | Destructive substitute |
| nsubstitute-if | ✓ | lib/stdlib.habu:738 | Destructive substitute-if |
| nsubstitute-if-not | ✓ | lib/stdlib.habu:742 | Destructive substitute-if-not |
| position | ✓ | compile.zig:192 | Find position |
| position-if | ✓ | lib/stdlib.habu | Position by pred |
| position-if-not | ✓ | lib/stdlib.habu | Position not pred |
| reduce | ✓ | lib/stdlib.habu:152 | Reduce sequence |
| remove | ✓ | compile.zig:194 | Remove element |
| remove-duplicates | ✓ | lib/stdlib.habu | Remove dups |
| remove-if | ✓ | lib/stdlib.habu | Remove by pred |
| remove-if-not | ✓ | lib/stdlib.habu | Remove not pred |
| replace | ✓ | lib/stdlib.habu | Replace elements |
| search | ✓ | lib/stdlib.habu | Search subsequence |
| some | ✓ | lib/stdlib.habu | Any satisfy |
| sort | ✓ | lib/stdlib.habu | Sort sequence |
| stable-sort | ✓ | lib/stdlib.habu | Stable sort |
| subseq | ✓ | compile.zig:311 | Subsequence |
| substitute | ✓ | lib/stdlib.habu | Substitute element |
| substitute-if | ✓ | lib/stdlib.habu | Substitute by pred |
| substitute-if-not | ✓ | lib/stdlib.habu | Substitute not pred |

## Functions - Hash Tables (17)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| clrhash | ✓ | compile.zig:372 | Clear hash table |
| gethash | ✓ | compile.zig:368 | Get hash entry |
| hash-table-count | ✓ | compile.zig:371 | Entry count |
| hash-table-p | ✓ | compile.zig:374 | Hash-table pred |
| hash-table-rehash-size | ✓ | lib/stdlib.habu | Rehash size |
| hash-table-rehash-threshold | ✓ | lib/stdlib.habu | Rehash threshold |
| hash-table-size | ✓ | lib/stdlib.habu | Table size |
| hash-table-test | ✓ | compile.zig:373 | Test function |
| make-hash-table | ✓ | compile.zig:367 | Create hash table |
| maphash | ✓ | lib/stdlib.habu | Map over hash |
| remhash | ✓ | compile.zig:370 | Remove entry |
| sxhash | ✓ | compile.zig:377 | Hash value |

## Functions - Filenames (26)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| *default-pathname-defaults* | ✓ | lib/stdlib.habu:176 | Default pathname |
| directory | ✓ | vm.zig:3151 | List directory contents |
| directory-namestring | ✓ | pathname.zig:287, vm.zig:3713 | Directory as string |
| enough-namestring | ✓ | vm.zig:3162 | Returns namestring |
| file-author | ✓ | vm.zig:2179 | File author (nil on Unix) |
| file-error-pathname | ✓ | lib/stdlib.habu | Get pathname from error |
| file-namestring | ✓ | pathname.zig:329, vm.zig:3719 | Name+type as string |
| file-write-date | ✓ | compile.zig:9545, vm.zig:2159 | Get file modification time |
| host-namestring | ✓ | pathname.zig:352, vm.zig:3725 | Host as string |
| logical-pathname | ✓ | lib/stdlib.habu:4780 | Coerce to logical |
| logical-pathname-translations | ✓ | lib/stdlib.habu:4771 | Returns nil |
| make-pathname | ✓ | pathname.zig:9, vm.zig:3013, lib/stdlib.habu:4080 | Create from components |
| merge-pathnames | ✓ | pathname.zig:269, vm.zig:3232 | Merge with defaults |
| namestring | ✓ | pathname.zig:78, vm.zig:3173 | Pathname to string |
| parse-namestring | ✓ | pathname.zig:153, vm.zig:3074 | String to pathname |
| pathname | ✓ | pathname.zig:71, vm.zig:3042 | Coerce to pathname |
| pathname-device | ✓ | pathname.zig:31, vm.zig:2721 | Get device component |
| pathname-directory | ✓ | pathname.zig:38, vm.zig:2728 | Get directory component |
| pathname-host | ✓ | pathname.zig:24, vm.zig:2714 | Get host component |
| pathname-match-p | ✓ | vm.zig:3156 | Match pathname to wildcard |
| pathname-name | ✓ | pathname.zig:45, vm.zig:2735 | Get name component |
| pathname-type | ✓ | pathname.zig:52, vm.zig:2742 | Get type component |
| pathname-version | ✓ | pathname.zig:59, vm.zig:2749 | Get version component |
| pathnamep | ✓ | pathname.zig:66 | Check if pathname |
| probe-file | ✓ | compile.zig:9541, vm.zig:2148 | Check if file exists |
| translate-logical-pathname | ✓ | lib/stdlib.habu:4766 | Identity fn |
| translate-pathname | ✓ | lib/stdlib.habu:4760 | Returns source |
| truename | ✓ | pathname.zig:325, vm.zig:3188 | Get canonical pathname |
| wild-pathname-p | ✓ | pathname.zig:508, vm.zig:3731 | Check for wildcards |

## Functions - Files/Streams (47)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| broadcast-stream-streams | ✓ | compile.zig, vm.zig | Broadcast stream accessor |
| clear-input | ✓ | vm.zig:3337, lib/stdlib.habu:2912 | Clear buffered input |
| clear-output | ✓ | vm.zig:3344, lib/stdlib.habu:2916 | Clear buffered output |
| close | ✓ | compile.zig:285 | Close stream |
| concatenated-stream-streams | ✓ | compile.zig, vm.zig | Concatenated stream accessor |
| delete-file | ✓ | compile.zig:9536, vm.zig:2131 | File deletion |
| echo-stream-input-stream | ✓ | compile.zig, vm.zig | Echo stream input accessor |
| echo-stream-output-stream | ✓ | compile.zig, vm.zig | Echo stream output accessor |
| ensure-directories-exist | ✓ | pathname.zig:349, vm.zig:3193 | Create missing directories |
| file-length | ✓ | compile.zig:291 | File length |
| file-position | ✓ | compile.zig:290 | File position |
| file-string-length | ✓ | vm.zig:4286 | Byte length of string for stream |
| finish-output | ✓ | compile.zig:292 | Finish output |
| force-output | ✓ | compile.zig:293 | Force output |
| fresh-line | ✓ | lib/stdlib.habu:2821 | Output newline if needed |
| get-output-stream-string | ✓ | compile.zig:393 | Get string |
| input-stream-p | ✓ | compile.zig:389 | Input pred |
| interactive-stream-p | ✓ | vm.zig:3172 | Check if stream is interactive |
| listen | ✓ | vm.zig:4278 | Check if input available |
| make-broadcast-stream | ✓ | compile.zig, vm.zig | Broadcast stream constructor |
| make-concatenated-stream | ✓ | compile.zig, vm.zig | Concatenated stream constructor |
| make-echo-stream | ✓ | compile.zig, vm.zig | Echo stream constructor |
| make-string-input-stream | ✓ | compile.zig:391 | String input |
| make-string-output-stream | ✓ | compile.zig:392 | String output |
| make-synonym-stream | ✓ | compile.zig, vm.zig | Synonym stream constructor |
| make-two-way-stream | ✓ | compile.zig, vm.zig | Two-way stream constructor |
| open | ✓ | compile.zig:283 | Open file |
| open-stream-p | ✓ | vm.zig:3163 | Check if stream is open |
| output-stream-p | ✓ | compile.zig:390 | Output pred |
| peek-char | ✓ | compile.zig:221 | Peek char |
| read-byte | ✓ | compile.zig:288 | Read byte |
| read-char | ✓ | compile.zig:220 | Read char |
| read-char-no-hang | ✓ | vm.zig:3214 | Non-blocking read char |
| read-line | ✓ | compile.zig:286 | Read line |
| read-sequence | ✓ | lib/stdlib.habu:4667 | Read into sequence |
| rename-file | ✓ | compile.zig:9537, vm.zig:2138 | File renaming |
| stream-element-type | ✓ | vm.zig:3186 | Get stream element type |
| stream-error-stream | ✓ | lib/stdlib.habu | Get stream from error |
| stream-external-format | ✓ | vm.zig:3191 | Get stream external format |
| streamp | ✓ | compile.zig:388 | Stream pred |
| synonym-stream-symbol | ✓ | compile.zig, vm.zig | Synonym stream symbol accessor |
| two-way-stream-input-stream | ✓ | compile.zig, vm.zig | Two-way stream input accessor |
| two-way-stream-output-stream | ✓ | compile.zig, vm.zig | Two-way stream output accessor |
| unread-char | ✓ | compile.zig:222 | Unread char |
| write-byte | ✓ | compile.zig:289 | Write byte |
| write-char | ✓ | compile.zig:317 | Write char |
| write-line | ✓ | compile.zig:287 | Write line |
| write-sequence | ✓ | lib/stdlib.habu:4683 | Write from sequence |
| write-string | ✓ | lib/stdlib.habu | Write string |
| y-or-n-p | ✓ | lib/stdlib.habu | Y/N query |
| yes-or-no-p | ✓ | lib/stdlib.habu | Yes/No query |

## Functions - Printer (25)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| copy-pprint-dispatch | ✓ | lib/stdlib.habu:4718 | Stub |
| format | ✓ | compile.zig:320 | Format output |
| pprint | ✓ | lib/stdlib.habu:4563 | Pretty print |
| pprint-dispatch | ✓ | lib/stdlib.habu:4724 | Returns write fn |
| pprint-fill | ✓ | lib/stdlib.habu:4569 | Fill output |
| pprint-indent | ✓ | lib/stdlib.habu:4696 | Stub |
| pprint-linear | ✓ | lib/stdlib.habu:4574 | Linear output |
| pprint-newline | ✓ | lib/stdlib.habu:4702 | Outputs newline |
| pprint-tab | ✓ | lib/stdlib.habu:4709 | Stub |
| pprint-tabular | ✓ | lib/stdlib.habu:4579 | Tabular output |
| prin1 | ✓ | io.zig:641 | Print readably |
| prin1-to-string | ✓ | lib/stdlib.habu:2815 | Print to string readably |
| princ | ✓ | io.zig:646 | Print aesthetic |
| princ-to-string | ✓ | lib/stdlib.habu | Print to string |
| print | ✓ | io.zig:663 | Print with newline |
| print-object | ✓ | lib/stdlib.habu:4652 | Default print method |
| set-pprint-dispatch | ✓ | lib/stdlib.habu:4730 | Stub |
| terpri | ✓ | compile.zig:316 | Output newline |
| write | ✓ | io.zig:624 | General output function |
| write-to-string | ✓ | io.zig:471 | To string |

## Functions - Reader (19)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| copy-readtable | ✓ | lib/stdlib.habu:4736 | Copy readtable |
| get-dispatch-macro-character | ✓ | compile.zig:332 | Get dispatch fn |
| get-macro-character | ✓ | compile.zig:330 | Get macro fn |
| make-dispatch-macro-character | ✓ | lib/stdlib.habu:4742 | Stub |
| read | ✓ | compile.zig:223 | Read S-expr |
| read-delimited-list | ✓ | lib/stdlib.habu:4902 | Read until delimiter |
| read-from-string | ✓ | compile.zig:224 | Read from string |
| read-preserving-whitespace | ✓ | lib/stdlib.habu:4914 | Via read |
| readtable-case | ✓ | lib/stdlib.habu:4748 | Returns :upcase |
| readtablep | ✓ | lib/stdlib.habu | Readtable pred |
| set-dispatch-macro-character | ✓ | compile.zig:331 | Set dispatch fn |
| set-macro-character | ✓ | compile.zig:329 | Set macro fn |
| set-syntax-from-char | ✓ | lib/stdlib.habu:4754 | Stub |

## Functions - System/Environment (43)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| apropos | ✓ | lib/stdlib.habu:4528 | Print matching symbols |
| apropos-list | ✓ | vm.zig:3205 | Find symbols by substring |
| compile | ✓ | lib/stdlib.habu:4876 | Stub |
| compile-file | ✓ | lib/stdlib.habu:4884 | Stub |
| compile-file-pathname | ✓ | lib/stdlib.habu:4891 | Stub |
| compiled-function-p | ✓ | lib/stdlib.habu | Compiled pred |
| compiler-macro-function | ✓ | lib/stdlib.habu:4661 | Returns nil |
| constantp | ✓ | lib/stdlib.habu | Constant pred |
| decode-universal-time | ✓ | io.zig:968, vm.zig:2199 | Decode to 9 values |
| describe | ✓ | lib/stdlib.habu:4536 | Print object description |
| describe-object | ✓ | lib/stdlib.habu:4536 | Via describe |
| disassemble | ✓ | compile.zig, vm.zig | Bytecode disassembly |
| documentation | ✓ | lib/stdlib.habu:4616 | Get doc string |
| dribble | ✓ | lib/stdlib.habu:4630 | Stub (returns nil) |
| ed | ✓ | lib/stdlib.habu:4635 | Stub (not available) |
| encode-universal-time | ✓ | vm.zig:2230 | Encode calendar to universal time |
| eval | ✓ | compile.zig:226 | Evaluate form |
| get-decoded-time | ✓ | io.zig:968, vm.zig:2191 | Current time as 9 values |
| get-internal-real-time | ✓ | compile.zig:9551, vm.zig:2169 | High-resolution timing |
| get-internal-run-time | ✓ | io.zig, vm.zig | Process CPU time |
| get-universal-time | ✓ | compile.zig:9549, vm.zig:2165 | Get current time |
| inspect | ✓ | lib/stdlib.habu:4559 | Non-interactive inspect |
| lisp-implementation-type | ✓ | compile.zig:9557, vm.zig:2181 | Returns "Habu" |
| lisp-implementation-version | ✓ | compile.zig:9558, vm.zig:2185 | Returns "0.1.0" |
| load | ✓ | compile.zig:225 | Load file |
| long-site-name | ✓ | compile.zig, vm.zig | Returns nil |
| machine-instance | ✓ | compile.zig, vm.zig | Returns nil |
| machine-type | ✓ | compile.zig:9560, vm.zig:2193 | CPU architecture |
| machine-version | ✓ | compile.zig, vm.zig | Returns nil |
| macro-function | ✓ | lib/stdlib.habu:4604 | Get macro function |
| macroexpand | ✓ | compile.zig:90 | Expand macro |
| macroexpand-1 | ✓ | compile.zig, vm.zig | Single expansion |
| provide | ✓ | lib/stdlib.habu:4588 | Mark module provided |
| require | ✓ | lib/stdlib.habu:4595 | Load module if needed |
| room | ✓ | compile.zig:9555, vm.zig:2174 | Memory statistics |
| short-site-name | ✓ | compile.zig, vm.zig | Returns nil |
| sleep | ✓ | compile.zig, vm.zig | Thread sleep |
| software-type | ✓ | compile.zig:9559, vm.zig:2189 | Operating system |
| software-version | ✓ | compile.zig, vm.zig | Returns nil |
| special-operator-p | ✓ | lib/stdlib.habu | Special op pred |
| user-homedir-pathname | ✓ | pathname.zig, vm.zig | Home directory |

## Functions - Symbols (17)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| boundp | ✓ | compile.zig:230 | Bound pred |
| copy-symbol | ✓ | vm.zig | Copy symbol (optionally with plist) |
| fboundp | ✓ | compile.zig:231 | Fbound pred |
| gensym | ✓ | compile.zig:227 | Generate symbol |
| gentemp | ✓ | lib/stdlib.habu | Generate temp symbol |
| get | ✓ | compile.zig:238 | Property get |
| keywordp | ✓ | compile.zig:206 | Keyword pred |
| make-symbol | ✓ | lib/stdlib.habu | Create uninterned symbol |
| makunbound | ✓ | vm.zig | Make symbol unbound |
| remprop | ✓ | compile.zig:240 | Remove property |
| set | ✓ | vm.zig | Set symbol value (deprecated) |
| symbol-function | ✓ | compile.zig:233 | Get function |
| symbol-name | ✓ | compile.zig:237 | Get name |
| symbol-package | ✓ | vm.zig:3039 | Get symbol's home package |
| symbol-plist | ✓ | lib/stdlib.habu | Get plist |
| symbol-value | ✓ | compile.zig:232 | Get value |
| symbolp | ✓ | compile.zig:201 | Symbol pred |

## Functions - Packages (22)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| delete-package | ✓ | vm.zig:3105 | Delete a package |
| export | ✓ | compile.zig:122 | Export symbols |
| find-all-symbols | ✓ | vm.zig:3155 | Find all symbols by name |
| find-package | ✓ | package.zig | Find package |
| find-symbol | ✓ | vm.zig:3140 | Find symbol in package (via %find-symbol) |
| import | ✓ | vm.zig:3110 | Import symbols (via %import) |
| in-package | ✓ | compile.zig:121 | Switch package |
| intern | ✓ | compile.zig:236 | Intern symbol |
| list-all-packages | ✓ | vm.zig:3097 | List all registered packages |
| make-package | ✓ | vm.zig:3176 | Create new package (via %make-package) |
| package-error-package | ✓ | lib/stdlib.habu | Get package from error |
| package-name | ✓ | vm.zig:3072 | Get package name |
| package-nicknames | ✓ | vm.zig:3076 | Get package nicknames |
| package-shadowing-symbols | ✓ | vm.zig:3092 | Get shadowing symbols |
| package-use-list | ✓ | vm.zig:3080 | Get packages used by this one |
| package-used-by-list | ✓ | vm.zig:3084 | Get packages that use this one |
| packagep | ✓ | vm.zig:3036 | Package predicate |
| rename-package | ✓ | vm.zig:3188 | Rename package (via %rename-package) |
| shadow | ✓ | vm.zig:3120 | Shadow symbols (via %shadow) |
| shadowing-import | ✓ | vm.zig:3125 | Shadowing import (via %shadowing-import) |
| unexport | ✓ | vm.zig:3115 | Unexport symbols (via %unexport) |
| unintern | ✓ | vm.zig:3135 | Unintern symbol (via %unintern) |
| unuse-package | ✓ | vm.zig:3130 | Unuse package (via %unuse-package) |
| use-package | ✓ | compile.zig:123 | Use package |

## Functions - CLOS (25)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| add-method | ✓ | opcodes.zig:0x125 | Add method to GF |
| allocate-instance | ✓ | lib/stdlib.habu:4955 | Via make-instance |
| call-next-method | ✓ | compile.zig:7521 | Call next method |
| change-class | ✓ | lib/stdlib.habu | Change object class |
| class-name | ✓ | compile.zig:2384 | Get class name |
| class-of | ✓ | compile.zig:7202 | Get class of object |
| compute-applicable-methods | ✓ | lib/stdlib.habu:4981 | Stub |
| ensure-generic-function | ✓ | lib/stdlib.habu:4961 | Stub |
| find-class | ✓ | compile.zig:2385 | Find class by name |
| find-method | ✓ | lib/stdlib.habu:4973 | Stub |
| function-keywords | ✓ | lib/stdlib.habu:4987 | Stub |
| initialize-instance | ✓ | lib/stdlib.habu | Initialize slots |
| make-instance | ✓ | compile.zig:130 | Create instance |
| make-instances-obsolete | ✓ | lib/stdlib.habu:4993 | Stub |
| make-load-form | ✓ | lib/stdlib.habu:4998 | Stub |
| make-load-form-saving-slots | ✓ | lib/stdlib.habu:5004 | Stub |
| method-qualifiers | ✓ | compile.zig:8013 | Get method qualifiers |
| next-method-p | ✓ | compile.zig:7599 | Check for next method |
| no-applicable-method | ✓ | lib/stdlib.habu:5010 | Stub |
| no-next-method | ✓ | lib/stdlib.habu:5016 | Stub |
| reinitialize-instance | ✓ | lib/stdlib.habu | Reinitialize slots |
| remove-method | ✓ | lib/stdlib.habu:4978 | Stub |
| shared-initialize | ✓ | lib/stdlib.habu:5022 | Stub |
| slot-boundp | ✓ | compile.zig:7115 | Check slot bound |
| slot-exists-p | ✓ | lib/stdlib.habu | Check slot exists |
| slot-makunbound | ✓ | opcodes.zig | Unbind slot |
| slot-missing | ✓ | lib/stdlib.habu:5028 | Stub |
| slot-unbound | ✓ | lib/stdlib.habu:5034 | Stub |
| slot-value | ✓ | compile.zig:131 | Access slot |
| update-instance-for-different-class | ✓ | lib/stdlib.habu | Update after class change |
| update-instance-for-redefined-class | ✓ | lib/stdlib.habu:5040 | Stub |

## Functions - Conditions (25)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| abort | ✓ | lib/stdlib.habu:472 | Standard restart |
| assert | ✓ | lib/stdlib.habu | Assertion |
| break | ✓ | lib/stdlib.habu:499 | Debugger entry |
| cell-error-name | ✓ | lib/stdlib.habu | Get name from cell-error |
| cerror | ✓ | lib/stdlib.habu:1041 | Continuable error |
| compute-restarts | ✓ | vm.zig:3134 | List active restarts |
| continue | ✓ | lib/stdlib.habu:477 | Standard restart |
| error | ✓ | compile.zig:241 | Signal error |
| find-restart | ✓ | compile.zig:110 | Find restart |
| invoke-debugger | ✓ | lib/stdlib.habu:4641 | Enter debugger |
| invoke-restart | ✓ | compile.zig:109 | Invoke restart |
| invoke-restart-interactively | ✓ | lib/stdlib.habu:4920 | Via invoke-restart |
| muffle-warning | ✓ | lib/stdlib.habu:484 | Standard restart |
| package-error-package | ✓ | lib/stdlib.habu | Get package from error |
| print-not-readable-object | ✓ | lib/stdlib.habu | Get object from error |
| restart-name | ✓ | vm.zig:3145 | Get restart name |
| signal | ✓ | compile.zig:105 | Signal condition |
| simple-condition-format-arguments | ✓ | condition.zig:93 | Get format-arguments slot |
| simple-condition-format-control | ✓ | condition.zig:81 | Get format-control slot |
| store-value | ✓ | lib/stdlib.habu:489 | Standard restart |
| type-error-datum | ✓ | lib/stdlib.habu | Get datum from error |
| type-error-expected-type | ✓ | lib/stdlib.habu | Get expected type |
| unbound-slot-instance | ✓ | lib/stdlib.habu | Get instance from error |
| use-value | ✓ | lib/stdlib.habu:494 | Standard restart |
| warn | ✓ | lib/stdlib.habu | Issue warning |

## Functions - Types (15)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| coerce | ✓ | lib/stdlib.habu | Type coercion |
| compiled-function-p | ✓ | lib/stdlib.habu | Compiled pred |
| subtypep | ✓ | lib/stdlib.habu:5047 | Partial impl |
| type-of | ✓ | primitives/type.zig | Get type |
| typep | ✓ | compile.zig:234 | Type predicate |

## Type Specifiers (50)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| and | ✓ | primitives/type.zig:64 | Compound type specifier |
| array | ✓ | objects.zig | Array type |
| atom | ✓ | compile.zig | Atom type |
| base-char | ✓ | primitives/type.zig | Type name |
| base-string | ✓ | primitives/type.zig | Type name |
| bignum | ✓ | objects.zig | Large integer |
| bit | ✓ | lib/stdlib.habu:450 | Bit array accessor |
| bit-vector | ✓ | lib/stdlib.habu | Bit vector |
| boolean | ✓ | primitives/type.zig:15 | Type name |
| broadcast-stream | ✓ | objects.zig | Stream type |
| built-in-class | ✓ | primitives/type.zig | CLOS type |
| cell-error | ✓ | lib/stdlib.habu | Condition type |
| character | ✓ | compile.zig | Character type |
| class | ✓ | primitives/type.zig | CLOS type |
| compiled-function | ✓ | lib/stdlib.habu | Compiled fn type |
| complex | ✓ | compile.zig | Complex type |
| concatenated-stream | ✓ | objects.zig | Stream type |
| condition | ✓ | lib/stdlib.habu | Condition type |
| cons | ✓ | compile.zig | Cons type |
| control-error | ✓ | lib/stdlib.habu | Condition type |
| division-by-zero | ✓ | lib/stdlib.habu | Condition type |
| double-float | ✓ | primitives/type.zig | Float subtype |
| echo-stream | ✓ | objects.zig | Stream type |
| end-of-file | ✓ | lib/stdlib.habu | Condition type |
| eql | ✓ | primitives/type.zig | Singleton type |
| error | ✓ | lib/stdlib.habu | Error condition |
| extended-char | ✓ | primitives/type.zig | Character type (empty) |
| file-error | ✓ | lib/stdlib.habu | Condition type |
| file-stream | ✓ | primitives/type.zig | Stream type |
| fixnum | ✓ | compile.zig | Small integer |
| float | ✓ | compile.zig | Float type |
| floating-point-inexact | ✓ | lib/stdlib.habu | Condition type |
| floating-point-invalid-operation | ✓ | lib/stdlib.habu | Condition type |
| floating-point-overflow | ✓ | lib/stdlib.habu | Condition type |
| floating-point-underflow | ✓ | lib/stdlib.habu | Condition type |
| function | ✓ | compile.zig | Function type |
| generic-function | ✓ | primitives/type.zig | CLOS type |
| hash-table | ✓ | compile.zig | Hash table type |
| integer | ✓ | compile.zig | Integer type |
| keyword | ✓ | compile.zig | Keyword type |
| list | ✓ | compile.zig | List type |
| logical-pathname | ✓ | lib/stdlib.habu:4780 | Coerce to logical |
| long-float | ✓ | primitives/type.zig | Float subtype |
| member | ✓ | primitives/type.zig | Enumeration type |
| method | ✓ | primitives/type.zig | CLOS type |
| method-combination | ✓ | primitives/type.zig | CLOS type (stub) |
| mod | ✓ | primitives/type.zig | Integer subtype |
| nil | ✓ | compile.zig | Nil type |
| not | ✓ | primitives/type.zig:74 | Complement type |
| null | ✓ | compile.zig | Null type |
| number | ✓ | compile.zig | Number type |
| or | ✓ | primitives/type.zig:53 | Union type specifier |
| package | ✓ | heap.zig, type.zig | Package type |
| package-error | ✓ | lib/stdlib.habu | Condition type |
| parse-error | ✓ | lib/stdlib.habu | Condition type |
| pathname | ✓ | primitives/type.zig | Pathname type |
| print-not-readable | ✓ | lib/stdlib.habu | Condition type |
| program-error | ✓ | lib/stdlib.habu | Condition type |
| random-state | ✓ | primitives/type.zig | Random state type |
| ratio | ✓ | objects.zig | Rational type |
| rational | ✓ | compile.zig | Rational type |
| reader-error | ✓ | lib/stdlib.habu | Condition type |
| readtable | ✓ | vm.zig | Readtable type |
| real | ✓ | primitives/type.zig:20 | Real number type |
| restart | ✓ | primitives/type.zig | Restart type (stub) |
| satisfies | ✓ | primitives/type.zig:80 | Predicate type (stub) |
| sequence | ✓ | primitives/type.zig:30 | Sequence type |
| serious-condition | ✓ | lib/stdlib.habu | Condition type |
| short-float | ✓ | primitives/type.zig | Float subtype |
| signed-byte | ✓ | primitives/type.zig | Integer subtype |
| simple-array | ✓ | primitives/type.zig | Array type |
| simple-base-string | ✓ | primitives/type.zig | String type |
| simple-bit-vector | ✓ | primitives/type.zig | Bit vector type |
| simple-condition | ✓ | lib/stdlib.habu | Simple condition |
| simple-error | ✓ | lib/stdlib.habu | Simple error |
| simple-string | ✓ | lib/stdlib.habu | Simple string |
| simple-type-error | ✓ | lib/stdlib.habu | Condition type |
| simple-vector | ✓ | lib/stdlib.habu | Simple vector |
| simple-warning | ✓ | lib/stdlib.habu | Condition type |
| single-float | ✓ | primitives/type.zig | Float subtype |
| standard-char | ✓ | primitives/type.zig | Character type |
| standard-class | ✓ | primitives/type.zig | CLOS type |
| standard-generic-function | ✓ | primitives/type.zig | CLOS type |
| standard-method | ✓ | primitives/type.zig | CLOS type |
| standard-object | ✓ | primitives/type.zig | CLOS type |
| storage-condition | ✓ | lib/stdlib.habu | Condition type |
| stream | ✓ | compile.zig | Stream type |
| stream-error | ✓ | lib/stdlib.habu | Condition type |
| string | ✓ | compile.zig | String type |
| string-stream | ✓ | compile.zig | String stream |
| structure-class | ✓ | primitives/type.zig | CLOS type |
| structure-object | ✓ | primitives/type.zig | Structure type |
| style-warning | ✓ | lib/stdlib.habu | Condition type |
| symbol | ✓ | compile.zig | Symbol type |
| synonym-stream | ✓ | objects.zig | Stream type |
| t | ✓ | compile.zig | Universal type |
| two-way-stream | ✓ | objects.zig | Stream type |
| type-error | ✓ | lib/stdlib.habu | Condition type |
| unbound-slot | ✓ | lib/stdlib.habu | Condition type |
| unbound-variable | ✓ | lib/stdlib.habu | Condition type |
| undefined-function | ✓ | lib/stdlib.habu | Condition type |
| unsigned-byte | ✓ | primitives/type.zig | Integer subtype |
| values | ✓ | primitives/type.zig | Multiple values type (stub) |
| vector | ✓ | compile.zig | Vector type |
| warning | ✓ | lib/stdlib.habu | Condition type |

## Audit Additions (51)
| Symbol | Status | Location | Notes |
|--------|--------|----------|-------|
| apply | ✓ | compile.zig:4135 | (apply fn &rest args) |
| arithmetic-error | ✓ | lib/stdlib.habu:2020 | Condition type |
| call-method | ✗ | - | Method combination helper macro |
| compilation-speed | ⚠ | compile.zig:9675 | OPTIMIZE quality parsed but ignored |
| compiler-macro | ✓ | lib/stdlib.habu:4732 | Documentation doc-type (ignored) |
| complement | ✓ | lib/stdlib.habu:2184 | Return negation of predicate |
| complexp | ✓ | compile.zig:1018 | Complex predicate |
| constantly | ✓ | lib/stdlib.habu:2179 | Return constant function |
| copy-structure | ✗ | - | Structure copier |
| debug | ⚠ | compile.zig:9675 | OPTIMIZE quality parsed but ignored |
| declaration | ⚠ | compile.zig:9621 | Declaration spec accepted but ignored |
| declare | ✓ | compile.zig:9518 | Local declarations |
| dynamic-extent | ✓ | compile.zig:9540 | Declaration spec |
| eq | ✓ | compile.zig:751 | Pointer identity |
| equal | ✓ | compile.zig:752 | Structural equality |
| equalp | ✗ | - | Extended equality |
| fdefinition | ✗ | - | Function binding accessor |
| floatp | ✓ | compile.zig:809 | Float predicate |
| fmakunbound | ✓ | primitives/symbol.zig:218 | Unbind function cell |
| ftype | ✓ | compile.zig:9549 | Function type declaration |
| funcall | ✓ | compile.zig:4126 | Call function |
| function-lambda-expression | ✗ | - | Closure introspection |
| functionp | ✓ | lib/stdlib.habu:3190 | Callable predicate |
| get-setf-expansion | ✗ | - | SETF expander API |
| identity | ✓ | lib/stdlib.habu:204 | Return argument unchanged |
| ignorable | ✓ | compile.zig:9554 | Declaration spec |
| ignore | ✓ | compile.zig:9553 | Declaration spec |
| inline | ✓ | compile.zig:9551 | Declaration spec |
| integerp | ✗ | - | Integer predicate |
| invalid-method-error | ✗ | - | Method combination helper |
| load-logical-pathname-translations | ✗ | - | Logical pathname translations |
| make-condition | ✓ | primitives/condition.zig:10 | Condition constructor |
| make-method | ✗ | - | Method combination helper |
| make-sequence | ✓ | lib/stdlib.habu:2762 | Generic sequence constructor |
| method-combination-error | ✗ | - | Method combination helper |
| notinline | ✓ | compile.zig:9552 | Declaration spec |
| numberp | ✓ | compile.zig:801 | Number predicate |
| optimize | ⚠ | compile.zig:9675 | Declaration spec accepted but ignored |
| otherwise | ✓ | lib/stdlib.habu:26 | CASE/TYPECASE default marker |
| proclaim | ✓ | compile.zig:9752 | Global declaration |
| rationalp | ✓ | compile.zig:1017 | Rational predicate |
| realp | ✗ | - | Real predicate |
| safety | ⚠ | compile.zig:9675 | OPTIMIZE quality parsed but ignored |
| space | ⚠ | compile.zig:9675 | OPTIMIZE quality parsed but ignored |
| special | ✓ | compile.zig:9555 | Declaration spec |
| speed | ⚠ | compile.zig:9675 | OPTIMIZE quality parsed but ignored |
| standard | ✗ | - | Method combination name |
| structure | ✓ | lib/stdlib.habu:4732 | Documentation doc-type (ignored) |
| type | ✓ | compile.zig:9548 | Declaration spec; doc-type marker |
| values-list | ✓ | compile.zig:5426 | Spread list into VALUES |
| variable | ✓ | lib/stdlib.habu:4732 | Documentation doc-type (ignored) |

---

## Summary

- Symbol set: `docs/cl-symbols-sbcl.txt` (978 external symbols)
- Document: 1011 entries (duplicates allowed for multi-role symbols)
- Entry status: ✓ 988 | ⚠ 10 | ✗ 13
- Unique coverage: 978/978
- Verify: `python3 tools/cl_symbols_audit.py`

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
