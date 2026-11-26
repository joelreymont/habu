\ error.f - canonical stdlib throw codes, and the rejection record of a throw of
\ one. -999999..-100000 is reserved for external applications and libraries
\ (docs/forth.md, Errors). A file owns the whole block between its FIRST and
\ LAST, minted or not; a subsystem that outgrows its block takes a second
\ region, never renumbers.
\ A package's block is minted in the package's file; a comment here says where.
\ STORAGE CLASS. PROCESS-WIDE: the rows ERROR:CODE and ERROR:FAULT lay in data
\ space and the persisted head cell that holds the newest one's address. A row
\ holds its own fault flag: only this package reads a row's layout.

require src/habu/layout.f
require src/habu/xref.f
require src/core/bytes.f
require src/core/engine-error.f
require src/lib/prelude.f
require src/core/rejection.f
require src/os/env-base.f

\ Arrays: -2000..-2099, declared at this file's end.
-2000 constant E-A-FIRST
-2099 constant E-A-LAST

\ Filesystem/files: -2100..-2199, declared at this file's end.
-2100 constant E-FS-FIRST
-2199 constant E-FS-LAST

\ True for a code of this block, declared or not.
: FS-CODE? ( n -- bool )  dup E-FS-FIRST <= swap E-FS-LAST >= and ;

\ Strings: -2200..-2299, declared at this file's end.
-2200 constant E-STR-FIRST
-2299 constant E-STR-LAST

\ Maps: -2400..-2499, declared at this file's end.
-2400 constant E-MAP-FIRST
-2499 constant E-MAP-LAST

\ Processes: -2500..-2599, declared at this file's end.
-2500 constant E-PROC-FIRST
-2599 constant E-PROC-LAST

\ Time/date: -2600..-2699, declared at this file's end.
-2600 constant E-TIME-FIRST
-2699 constant E-TIME-LAST

\ Property tests: -2700..-2799, declared at this file's end.
-2700 constant E-PROP-FIRST
-2799 constant E-PROP-LAST

\ Builds: -2800..-2899, declared at this file's end.
-2800 constant E-BUILD-FIRST
-2899 constant E-BUILD-LAST


\ Tables: -3000..-3099, declared at this file's end.
-3000 constant E-TBL-FIRST
-3099 constant E-TBL-LAST

\ JSON writer: -3100..-3199, declared at this file's end.
-3100 constant E-JSON-WRITE-FIRST
-3199 constant E-JSON-WRITE-LAST

\ OS-backed memory: -3200..-3299, minted in src/core/engine-error.f.

\ Vectors: -3300..-3399, declared at this file's end.
-3300 constant E-VEC-FIRST
-3399 constant E-VEC-LAST

\ FFI: -3500..-3599, declared at this file's end.
-3500 constant E-FFI-FIRST
-3599 constant E-FFI-LAST

\ Tasking/threads: -3600..-3699, declared at this file's end.
-3600 constant E-TASK-FIRST
-3699 constant E-TASK-LAST

\ Engine self-identity: -3800..-3899, declared at this file's end.
-3800 constant E-ENGINE-FIRST
-3899 constant E-ENGINE-LAST

\ JSON reader: -3900..-3999, minted by src/lib/json/read.f, a file of package JSON (src/lib/json.f).

\ Remote device harness (ssh zed): -4000..-4099, declared at this file's end.
-4000 constant E-ZED-FIRST
-4099 constant E-ZED-LAST
\ -4006 unassigned: a ZED run's expired deadline throws E-PROC-TIMEOUT.

\ Floating mathematics: -9020..-9029, minted by src/lib/fmath.f, which owns package
\ FMATH.

\ Source-composition discovery: -4100..-4199, declared at this file's end.
-4100 constant E-DISC-FIRST
-4199 constant E-DISC-LAST

\ Test-suite DSL: -4200..-4299, declared at this file's end.
-4200 constant E-SUITE-FIRST
-4299 constant E-SUITE-LAST


\ Owned growable byte buffer: -5700..-5799, declared at this file's end, past
\ the stdlib's -2000..-4999 and research/maki's own -5000..-5699.
-5700 constant E-BUF-FIRST
-5799 constant E-BUF-LAST

\ Open-addressing integer-key hash probe: -5800..-5899, declared at this file's end.
-5800 constant E-HM-FIRST
-5899 constant E-HM-LAST

\ Number formatting: -5900..-5999, declared at this file's end.
-5900 constant E-FMT-FIRST
-5999 constant E-FMT-LAST

\ Bounded pointers (package SPAN, src/lib/span.f): -6100..-6199, declared at
\ this file's end.
-6100 constant E-SPAN-FIRST
-6199 constant E-SPAN-LAST


\ Engine arithmetic: -6400..-6499, declared at this file's end.
-6400 constant E-ARITH-FIRST
-6499 constant E-ARITH-LAST


\ Shared compiler block: -6600..-6699; later compiler stages mint from
\ -8000..-8999.
-6600 constant E-IR-FIRST
-6699 constant E-IR-LAST
\ Compiler target contract (package CTARGET): -6610..-6619, declared at this
\ file's end.

\ Compiler numerical policy (package CNUM): -6620..-6629, declared at this
\ file's end.

\ Compiler target/policy binding (package CBIND): -6630..-6639, declared at
\ this file's end.


\ Checker behavior and schema checks (package CHECKER-MODEL-PROOF): -6860..-6879,
\ declared at this file's end.
-6860 constant E-CMP-FIRST
-6879 constant E-CMP-LAST


\ Content-key folds: -6920..-6929, minted in src/lib/content-key.f, which owns
\ package CONTENT-KEY.
\ Encoding Standard table generator and verifier: -7900..-7909, minted in
\ tools/encoding/generate.f and verify.f, which own packages ENCODING-TOOL
\ and ENCODING-VERIFY.


\ Compiler growth region: -8000..-8999, the compiler's second block. Sub-blocks
\ are 20 codes wide, each headed below by its range; a stage that outgrows its
\ own takes the next one no `-8NNN constant` under src, tools or test claims.
-8000 constant E-COMP-FIRST
-8999 constant E-COMP-LAST

\ ARM64 routine machine-effect contracts (package NEFF): -8200..-8219, declared
\ at this file's end.
\ Minted for src/compiler/native-effect.f: what an emitted ARM64 routine does
\ to the registers, flags, link register, stack pointer and control.

\ The does> cut of a definition's source: -8400..-8409, declared at this file's end.

\ Native ARM64 data-stack calling convention: -8480..-8499, declared at this
\ file's end.
\
\ A position of a calling convention is a PLACE: a register or a data-stack
\ slot. Rejections another authority owns keep its name: a non-canonical place
\ list is NEFF's E-NEFF-SEQ, a register no routine may hold state in E-NEFF-GPR,
\ an operand outside its field the assembler's.

\ Native ARM64 control flow: -8500..-8519, declared at this file's end.

\ Native publication: -8560..-8579, declared at this file's end.

\ Asking the running dictionary about a spelling: -8600..-8609, declared at this
\ file's end.

\ Native publication, continued: -8621..-8623, declared at this file's end.

\ Native emission and string storage: -8635..-8639, declared at this file's end.

\ The register-file description and the machine description over it:
\ -8670..-8672, declared at this file's end.

\ The shadow target's routines: -8680..-8689, declared at this file's end.
\
\ Rejections of the shadow chain's stages and of publication keep their own
\ names.

\ Native x86-64 instruction emission: -8780..-8799, declared at this file's end.

\ Embedded instruction constructors (compiler growth region), declared at this
\ file's end.

\ C66x instruction facts for scheduling and packet checks: -8870..-8879, declared
\ at this file's end.

\ C66x subset interpreter for host tests: -8880..-8889, declared at this file's end.

\ C6000 EABI helper emission: -8890..-8899, declared at this file's end.

\ ELF32 bounded binary reader: -9000..-9019, declared at this file's end.
-9000 constant E-ELF32-FIRST
-9019 constant E-ELF32-LAST

\ IPv4 UDP host library: -9100..-9109, declared at this file's end.
-9100 constant E-UDP4-FIRST
-9109 constant E-UDP4-LAST
\ -9101..-9103 unassigned.

\ Raw serial host library: -9110..-9119, declared at this file's end.
-9110 constant E-SERIAL-FIRST
-9119 constant E-SERIAL-LAST
\ -9111 unassigned.
\ -9112 unassigned: a FUNCTION: symbol that is missing is E-FFI-DLSYM.

\ XMODEM packet codec and serial transfer: -9120..-9139, declared at this file's
\ end.
-9120 constant E-XMODEM-FIRST
-9139 constant E-XMODEM-LAST

\ Pseudoterminal pairs for test peers: -9160..-9169 (-9140..-9159 belong to
\ src/lib/byte-edit.f and src/lib/unicode.f), declared at this file's end.
-9160 constant E-PTY-FIRST
-9169 constant E-PTY-LAST

\ Directory listings: -9170..-9179, declared at this file's end.
-9170 constant E-FS-LIST-FIRST
-9179 constant E-FS-LIST-LAST

\ IPv4 TCP host library: -9180..-9189, declared at this file's end.
-9180 constant E-TCP4-FIRST
-9189 constant E-TCP4-LAST
\ -9181 unassigned.
\ -9182 unassigned: a FUNCTION: symbol that is missing is E-FFI-DLSYM.

\ Bounded task queues: -9190..-9199, declared at this file's end.
-9190 constant E-QUEUE-FIRST
-9199 constant E-QUEUE-LAST

\ Generic I/O devices: -9220..-9229, declared at this file's end.
-9220 constant E-GENIO-FIRST
-9229 constant E-GENIO-LAST

\ HTTPS client over libcurl: -9230..-9239, declared at this file's end.
-9230 constant E-CURL-FIRST
-9239 constant E-CURL-LAST
\ -9231 unassigned: a FUNCTION: symbol that is missing is E-FFI-DLSYM.
\ -9235 unassigned: src/lib/net/curl.f grows its transfer records.

\ HMAC and RS256 over libcrypto (package CRYPTO): -9270..-9279, declared at
\ this file's end.
\ -9271..-9274 unassigned.
-9270 constant E-CRYPTO-FIRST
-9279 constant E-CRYPTO-LAST

\ Datagram sockets of any address family: -9240..-9249, minted by
\ src/lib/net/datagram.f, which owns package DATAGRAM.

\ PostgreSQL over libpq: -9250..-9259, minted by src/lib/pg.f, which owns package
\ PG. The block is full; PG's row readers are -9320..-9329 and its growth region
\ -9560..-9569.

\ Directory change notices: -9260..-9269, minted in src/lib/inotify.f, which
\ owns package INOTIFY.

\ Process signals on a self-pipe (package SIGNAL): -9280..-9289, declared at this
\ file's end.
-9280 constant E-SIGNAL-FIRST
-9289 constant E-SIGNAL-LAST

\ The io_uring readiness loop (package AIO): -9290..-9299, declared at this
\ file's end.
-9290 constant E-AIO-FIRST
-9299 constant E-AIO-LAST

\ The debugger's breakpoints (package DEBUG): -9300..-9309, minted in
\ src/habu/debug.f.

\ The host's available memory: -9310..-9319, minted in src/lib/os-memory.f,
\ which owns package OS-MEMORY.

\ Package PG's row readers: -9320..-9329, minted by src/lib/pg-rows.f.

\ The WebSocket frame codec, RFC 6455 (package WS, src/lib/net/ws-frame.f):
\ -9330..-9339, declared at this file's end. The codes after E-WS-MASK are its
\ reserve.
-9330 constant E-WS-FIRST
-9339 constant E-WS-LAST

\ The HTTP/1.1 server (package HTTP, src/lib/net/http.f) and HTTP Basic
\ credentials (src/lib/net/http-basic.f): -9340..-9349, declared at this file's
\ end.
-9340 constant E-HTTP-FIRST
-9349 constant E-HTTP-LAST

\ Base64 (package BASE64): -9350..-9359, declared at this file's end.
-9350 constant E-BASE64-FIRST
-9359 constant E-BASE64-LAST

\ Private C2 owner-frame runtime, declared at this file's end.

\ URIs (package URI, src/lib/uri.f): -9370..-9379, declared at this file's end.
-9370 constant E-URI-FIRST
-9379 constant E-URI-LAST

\ Content-Length framing (package CONTENT-LENGTH, src/lib/content-length.f):
\ -9380..-9389, declared at this file's end.
-9380 constant E-CONTENT-LENGTH-FIRST
-9389 constant E-CONTENT-LENGTH-LAST

\ Language server: -9400..-9401, minted by tools/lsp-core.f, which owns package
\ LSP.

\ WebSocket connections over the HTTP server (package WS, src/lib/net/ws.f):
\ -9410..-9419, declared at this file's end.
-9410 constant E-WS-CONN-FIRST
-9419 constant E-WS-CONN-LAST

\ SSE owns -9450..-9459. DECIMAL owns -9460..-9469.
\ JSONL framing (package JSONL, src/lib/jsonl.f): -9470..-9479.
\ MCP tool server and client (package MCP, src/lib/net/mcp.f): -9480..-9489,
\ minted there.
\ SVG to PDF (package SVG-PDF): -9490..-9499, minted in src/lib/svg-pdf.f.
\ Secret store, native or host-bound: -9500..-9509, minted in src/lib/secret.f.

\ Intel disassembly: -9510..-9519, minted by src/arch/x86-64/disasm.f,
\ which owns package X64DIS.
\ Raw DEFLATE and CRC-32: -9520..-9529, minted in src/lib/zlib.f, which owns
\ package ZLIB.
\ Strict UTF-16 conversion: -9530..-9539, minted in src/lib/utf16.f, which
\ owns package UTF16.
\ OS random bytes: -9540..-9549, minted in src/lib/entropy.f, which owns
\ package ENTROPY.
\ Online processor count and load average: -9550..-9559, minted in
\ src/lib/os-cpu.f, which owns package OS-CPU.
\ PostgreSQL's growth region: -9560..-9569, minted by src/lib/pg.f.
\ Word contracts read from source: -9570..-9579, minted in src/lib/word-docs.f,
\ which owns package WORD-DOCS.
\ Scoped commands: -9580..-9589, minted in src/lib/process-command.f, which
\ owns package CMD.
\ Tier 0 emitter seam: -9590..-9599, minted in src/habu/t0-emit.f, package
\ COMP.
\ Stderr capture for tests: -9610..-9619, minted in src/lib/test/stderr.f, which
\ owns package STDERR.
\ SQLite: -9620..-9629, minted in src/lib/sqlite.f, which owns package SQLITE.
\ OAuth bearer tokens: -9630..-9639, minted in src/lib/net/bearer.f, which owns
\ package BEARER.
\ Keeping the machine awake: -9640..-9649, minted in src/lib/power.f, which
\ owns package POWER.
\ Code signing: -9650..-9659, minted in src/lib/codesign.f, which owns package
\ CODESIGN.
\ State machines: -9660..-9669, minted in src/lib/fsm.f, which owns package
\ FSM.
\ Codes the ERROR:CODE tests declare: -9670..-9689, in src/lib/error-test.f,
\ src/lib/log-test.f and test/fixtures/error/.
\ The linker's failed build: -9690..-9699, minted in src/habu/linker.f, package
\ LINKER.
\ Model chat and embedding client: -9700..-9799, minted in src/lib/net/model.f,
\ which owns package MODEL.

\ Wasm back end: -9800..-9834, minted in src/arch/wasm/: WLEB -9800..-9804,
\ WLINK -9830..-9834.
\ Adobe Ascii85: -9840..-9849, minted in src/lib/base85.f, which owns package
\ BASE85.
\ Engine faults: -9850..-9859, thrown by the kernel (src/habu/habu2.f) and
\ the interpreter (src/habu/interpreter.f), declared at this file's end.
\ Finite binary64 text: -9860..-9869, minted in src/lib/f64-text.f, which owns
\ package F64-TEXT.
\ JSON Pointers: -9870..-9879, minted in src/lib/json/pointer.f, a file of
\ package JSON (src/lib/json.f).
\ Enum tags: -9880..-9889, minted in src/core/sumtype.f (FAM:TAG>).
\ Encoding Standard decoding: -9890..-9899, minted in src/lib/encoding/fault.f,
\ part of package ENCODING.
\ Extended attributes: -9900..-9909, minted in src/lib/xattr.f, which owns
\ package XATTR.
\ JSON Schema: -9910..-9919, minted in src/lib/json-schema.f, which owns
\ package JSON-SCHEMA.
\ The key light's volume: -9920..-9929, minted in src/lib/browser/studio.f,
\ which owns package STUDIO.
\ OData: -9930..-9939, minted in src/lib/odata/metadata.f, a file of package
\ ODATA (src/lib/odata.f).
\ Regular expressions: -9940..-9949, minted in src/lib/regex.f, which owns
\ package REGEX.
\ Tests' own fixture codes: -9950..-9999, each declared in the test file that
\ throws it; no lib block is taken from this range.
\ SPARQL queries: -10000..-10099, minted in src/lib/sparql/syntax.f, a file of
\ package SPARQL (src/lib/sparql.f).
\ PNG decoding: -10100..-10109, minted in src/lib/png.f, which owns package PNG.

\ T0 (src/habu/t0.f): -9600..-9609, declared at this file's end.
-9600 constant E-T0-FIRST
-9609 constant E-T0-LAST

\ HEAD and each row's first cell are declared address cells holding the newest
\ row and the row before, NULL-PTR for none, so the linker relocates them.
package ERROR

private
EXPORT BOOT:data-base

255 constant NAME-CAP

PERSISTED-PTR-VARIABLE HEAD

: ROW-PREV ( ^n -- ^n )  0 ptr-field @ ;
: ROW-CODE ( ^n -- n )  cell+ @ ;


: ROW-FAULT? ( ^n -- bool )  4 cells + @ 0<> ;


: ROW-NAME$ ( ^n -- str ) {: r:^ :}
   r 5 cells + BYTE-VIEW r 2 cells + @ >str ;


: ROW-TEXT$ ( ^n -- str ) {: r:^ :}
   r 5 cells + BYTE-VIEW r 2 cells + @ + r 3 cells + @ >str ;


: FIND ( n -- ^n ) {: code:n :}
   HEAD @
   begin dup NULL-PTR <> while
      dup ROW-CODE code = if exit then
      ROW-PREV
   repeat ;


\ The open package's own spelling, empty outside a package.
: PKG$ ( -- str )
   data-base PKG-REC-CELL + @ dup 0= if drop s" " exit then
   XREF-ADDR>REC XREF-NAME$ ;


: SAME-NAME? ( ^n str str -- bool )
   {: at:^ p:str name:str :}
   at ROW-NAME$ {: r:str :}
   p STR:LEN 0= if r name STR:=CI exit then
   r STR:LEN p STR:LEN name STR:LEN + 1+ <> if false exit then
   r STR:DATA p STR:LEN + c@ $3A <> if false exit then
   r STR:DATA p STR:LEN >str p STR:=CI if r STR:DATA p STR:LEN + 1+ name STR:LEN >str name STR:=CI exit then
   false ;


\ Rejects code's declaration as p : name, from token t after kw:
\ no name, a name past NAME-CAP bytes, an empty text, or a code another holds.
: CHECK ( n str str n str str -- )
   {: code:n p:str name:str textu:n kw:str t:str :}
   kw t REJECTION:NOT-A-NAME
   name STR:LEN 0= if
      kw kw s" no name follows it" REJECTION:MISSING-NAME
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   name STR:LEN NAME-CAP > if
      s" E-BAD-DECLARATION" REJECTION:OPEN
      s" the name of a code is " REJECTION:EMIT-STR  name STR:LEN REJECTION:EMIT-U
      s"  bytes, more than the " REJECTION:EMIT-STR  NAME-CAP REJECTION:EMIT-U
      s"  a name holds" REJECTION:EMIT-STR
      t REJECTION:AT  s" too long" REJECTION:EMIT-STR
      REJECTION:HELP  s" shorten the name to " REJECTION:EMIT-STR
      NAME-CAP REJECTION:EMIT-U  s"  bytes or fewer" REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   textu 0= if
      s" E-BAD-DECLARATION" REJECTION:OPEN
      s" code " REJECTION:EMIT-STR  code REJECTION:EMIT-N
      s"  has an empty text" REJECTION:EMIT-STR
      kw REJECTION:AT  s" declared here" REJECTION:EMIT-STR
      REJECTION:HELP  s" give the code a text that says what failed" REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   code FIND {: at:^ :}
   at NULL-PTR = if exit then
   at p name SAME-NAME? if exit then
   s" E-DUPLICATE-DEFINITION" REJECTION:OPEN
   s" code " REJECTION:EMIT-STR  code REJECTION:EMIT-N
   s"  is already `" REJECTION:EMIT-STR  at ROW-NAME$ REJECTION:EMIT-STR  96 REJECTION:EMIT1
   t REJECTION:AT  s" declared again" REJECTION:EMIT-STR
   REJECTION:HELP  s" give `" REJECTION:EMIT-STR
   p STR:LEN 0<> if p REJECTION:EMIT-STR  s" :" REJECTION:EMIT-STR then
   name REJECTION:EMIT-STR
   s" ` a code its block has not declared" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;


CAST: AS-CHILD-RUN ( n -- [ str n n str -- n ] )

\ The name becomes a folded constant in the current wordlist, made with no
\ text: a value child, whose type is empty.
: DEFINE ( n str -- ) {: code:n name:str :}
   name CHILD-ABI:VALUE code NULL-PTR 0 >str
   data-base PROVIDED-XT:CHILD-CELL + @ AS-CHILD-RUN execute drop ;


: BYTES, ( str -- ) {: s:str :}
   s STR:DATA here s STR:LEN BYTE-COPY
   s STR:LEN allot ;


: PAD, ( -- )
   begin here NULL-PTR - 1 cells 1- and 0<> while 0 c, repeat ;


\ A row is five cells, the previous row, the code, the qualified name's length,
\ the text's length and the fault flag (-1 or 0), then the name and the text.
: ROW, ( n str str str bool -- )
   {: code:n p:str name:str text:str fault:bool :}
   datum
   here CELL-VIEW {: at:^ :}
   here ptr-cell-mark 0 ,
   HEAD @ at 0 ptr-field !
   code ,
   p STR:LEN name STR:LEN + p STR:LEN 0<> if 1+ then ,
   text STR:LEN ,
   fault bool>n ,
   p BYTES,
   p STR:LEN 0<> if s" :" BYTES, then
   name BYTES,
   text BYTES,
   PAD,
   at HEAD ! ;


\ CODE's and FAULT's declaration of the name after them, the flag saying FAULT.
: DECLARE ( n str bool -- )
   {: code:n text:str fault:bool :}
   SRCLOC:TOKEN$ >str {: kw:str :}
   parse-name {: name:str :}
   PKG$ {: p:str :}
   code p name text STR:LEN kw name CHECK
   code name DEFINE
   code p name text fault ROW, ;


\ The text of a code a file below this one mints by `constant`, recorded under
\ the name after it, PKG:TAIL split at its last colon; it defines no word.
: MINTED ( n str bool -- )
   {: code:n text:str fault:bool :}
   SRCLOC:TOKEN$ >str {: kw:str :}
   parse-name {: name:str :}
   name STR:LEN
   begin dup 0 > if dup 1- name STR:DATA + c@ $3A <> else false then while 1- repeat
   {: cut:n :}
   cut 0 > if cut 1- else 0 then {: pu:n :}
   name STR:DATA cut + name STR:LEN cut - >str {: tail:str :}
   code name STR:DATA pu >str tail text STR:LEN kw name CHECK
   code name STR:DATA pu >str tail text fault ROW, ;
parses: MINTED 1

public

\ A name the wordlist holds is E-DUPLICATE-DEFINITION; a keyword or a number
\ is rejected as any definition's name is.
: CODE ( n str -- )  false DECLARE ;
generates: CODE ( -- n )

\ Declares a code as CODE does, rejections included, and marks it a fault: a
\ defect in the program or its environment rather than in what its caller gave.
: FAULT ( n str -- )  true DECLARE ;
generates: FAULT ( -- n )

\ True when a code's declaration was by FAULT; false when it was by CODE or
\ when there is none.
: FAULT? ( n -- bool )
   FIND dup NULL-PTR = if drop false exit then
   ROW-FAULT? ;

\ The name or text a code's declaration gave it, empty when none.
: NAME$ ( n -- str )
   FIND dup NULL-PTR = if drop s" " exit then
   ROW-NAME$ ;


: TEXT$ ( n -- str )
   FIND dup NULL-PTR = if drop s" " exit then
   ROW-TEXT$ ;

\ Opens the rejection record of a throw of code c: its declaration's name and
\ text, or E-THROW and the code when it has none.
: OPEN ( n -- )
   {: c:n :}
   c NAME$ {: name:str :}
   name STR:LEN 0= if s" E-THROW" else name then REJECTION:OPEN
   c TEXT$ {: text:str :}
   text STR:LEN 0 > if text REJECTION:EMIT-STR exit then
   s" nothing caught a throw of code " REJECTION:EMIT-STR
   c REJECTION:EMIT-N ;

;package

-9850 s" data space is full: hold bulk data in MEM:ALLOC-BYTES or a DYNAMIC-BUFFER, not in the dictionary" ERROR:CODE E-DATA-SPACE
-9851 s" a word took more values than the data stack held: give it the inputs its effect states" ERROR:CODE E-UNDERFLOW
-9852 s" a definition ran while a checker replay is open: define after the `CHECK:SCOPED` quotation returns" ERROR:CODE E-OVERLAY-OPEN
-9853 s" no memory for a definition's captured text" ERROR:CODE E-BODY-TEXT
-9854 s" a computed 0 was thrown, but 0 means no error: throw a nonzero code, as an error constant the package declares" ERROR:CODE E-THROW-ZERO
-9855 s" no memory to grow the table of address cells: make fewer address cells, or raise the process's address-space limit" ERROR:CODE E-XTCELL-GROW
-2100 s" a path is empty, too long or of a form the call rejects" ERROR:CODE E-FS-PATH
-2101 s" a path's status could not be read or it names no regular file" ERROR:CODE E-FS-STAT
-2102 s" a file, directory or pipe could not be opened" ERROR:CODE E-FS-OPEN
-2103 s" a directory could not be read or is not one" ERROR:CODE E-FS-DIR
-2105 s" a read, write or other operation on an open file failed or fell short" ERROR:CODE E-FS-IO
-2106 s" a path or a file's bytes do not fit the room given for them" ERROR:CODE E-FS-CAPACITY
-2107 s" a path holds a byte its quoted spelling cannot carry" ERROR:CODE E-FS-PATH-UNSAFE
-2108 s" the layout's FS-ABI band widths disagree with the file-system caps" ERROR:CODE E-FS-BAND
-2109 s" the target holds the new bytes; its directory sync failed" ERROR:CODE E-FS-SYNC
-2110 s" a descriptor transfer reached its deadline" ERROR:CODE E-FS-TIMEOUT
-2111 s" a file read whole yielded more bytes than its size sampled before the read" ERROR:CODE E-FS-CHANGED
-2000 s" an array word that needs an element was given an empty array" ERROR:CODE E-A-EMPTY
-2001 s" an index or count is negative or outside the array or slot table it names" ERROR:CODE E-A-BOUNDS
-2200 s" a string length, offset, index or byte value is negative or out of range" ERROR:CODE E-STR-BOUNDS
-2201 s" a string does not fit the room its builder or buffer holds" ERROR:CODE E-STR-CAPACITY
-2400 s" the map holds as many entries as its capacity: make it with a larger one" ERROR:CODE E-MAP-FULL
-2401 s" a map capacity, count, index, length or slot field is out of range, or a handle does not match its capacity" ERROR:CODE E-MAP-BAD-CAP
-2500 s" a child process could not be started" ERROR:CODE E-PROC-SPAWN
-2501 s" waiting for a child process failed, or the pid is not one this process can wait for" ERROR:CODE E-PROC-WAIT
-2502 s" a child process or a wait on it ran past its deadline" ERROR:CODE E-PROC-TIMEOUT
-2503 s" a child's pipe, descriptor or poll could not be set up or read, or a pid or copied argument is out of range" ERROR:CODE E-PROC-OUTPUT
-2504 s" a child wrote more to its stdout than the capture holds" ERROR:CODE E-PROC-STDOUT-CAPACITY
-2505 s" an environment name, entry or index is malformed: negative, empty, a name holding `=` or a 0 byte, an entry with no `=`, or too large to store" ERROR:CODE E-PROC-ENV
-2506 s" a program or directory path is empty, too long, or names no executable" ERROR:CODE E-PROC-PATH
-2507 s" every pseudoterminal child slot is in use" ERROR:CODE E-PROC-PTY-CAPACITY
-2508 s" a pseudoterminal child handle is stale, another process's, or in the wrong state for the call" ERROR:CODE E-PROC-PTY-HANDLE
-2510 s" the caller's borrowed cancellation fd became ready" ERROR:CODE E-PROC-CANCELLED
-2511 s" a child wrote more to its stderr than the capture holds" ERROR:CODE E-PROC-STDERR-CAPACITY
-2600 s" a date's year is outside 0..9999, or a time is before the epoch" ERROR:CODE E-TIME-RANGE
-2601 s" the buffer is shorter than the date or time text" ERROR:CODE E-TIME-CAPACITY
-2602 s" reading the clock failed" ERROR:FAULT E-TIME-CLOCK
-2700 s" a property test's seed or run count is out of range" ERROR:CODE E-PROP-SEED
-2701 s" a property generator was given a bound below 1, or a byte or digit out of range" ERROR:CODE E-PROP-GENERATOR
-2703 s" a generated value does not fit the property test's buffer" ERROR:CODE E-PROP-CAPACITY
-2800 s" a build input's digest is malformed or does not match, or the target is not one the build knows" ERROR:CODE E-BUILD-SOURCE
-2801 s" a tool the build runs is not installed where the build looks for it" ERROR:FAULT E-BUILD-COMMAND
-2802 s" a build step exited nonzero, wrote to stderr, or left an incomplete or inconsistent report" ERROR:CODE E-BUILD-STATUS
-2803 s" a build path is empty, too long, missing, or outside the tree it must lie under" ERROR:CODE E-BUILD-PATH
-2806 s" replacing an installed file failed before its rename: the old file is kept" ERROR:CODE E-BUILD-INSTALL
-3000 s" a table index or count is outside the table" ERROR:CODE E-TBL-BOUNDS
-3001 s" a table field's value is negative, too long, or of a kind the table has no row for" ERROR:CODE E-TBL-FIELD
-3100 s" the value does not fit the caller's output buffer" ERROR:CODE E-JSON-WRITE-CAPACITY
-3101 s" a negative U or indent, or string, key or part text that is not UTF-8" ERROR:CODE E-JSON-WRITE-BYTE
-3102 s" the writer is not open, or a call its layout rejects" ERROR:CODE E-JSON-WRITE-STATE
-3103 s" the output buffer is null, or its capacity is negative" ERROR:CODE E-JSON-WRITE-OUTPUT
-3104 s" an appended span is negative, null with a positive length, or inside a growable output past its written bytes" ERROR:CODE E-JSON-WRITE-SOURCE
-3300 s" an index outside the vector's elements, or a vector not made" ERROR:CODE E-VEC-BOUNDS
-3301 s" a vector count below 0 or past the most bytes a vector holds" ERROR:CODE E-VEC-CAPACITY
-3500 s" a declaration's arguments need more cells than a C call passes, or a callback argument index is outside its registers" ERROR:CODE E-FFI-ARITY
-3502 s" a C symbol a FUNCTION: names is not in its library" ERROR:FAULT E-FFI-DLSYM
-3503 s" a FUNCTION: with no library selected in its declaring scope: state LIBRARY or PROCESS-SYMBOLS beside the declarations rather than inherit another file's" ERROR:CODE E-FFI-LIBRARY
-3504 s" a string with a 0 byte handed to C, which would end it early" ERROR:CODE E-FFI-NUL
-3505 s" a library a FUNCTION: names could not be opened" ERROR:FAULT E-FFI-DLOPEN
-3507 s" a callback frame accessor used outside a live callback, a descriptor no declaration made, or a dispatch whose body was never stored" ERROR:CODE E-FFI-CALLBACK-STATE
-3600 s" a task's data stack is smaller than TASK:MIN-STACK" ERROR:CODE E-TASK-SIZE
-3603 s" a thread, semaphore or sleep call failed" ERROR:FAULT E-TASK-THREAD
-3604 s" a task word called in a state it rejects, for example TASK:RETURN outside a task or a second result, an activation of a running or halting task, a free of a task that is not pooled and empty, a context or callback slot used outside its owner, the task table changed while tasks run, or a task started while the checker's replay parks a definition" ERROR:CODE E-TASK-STATE
-3605 s" a task user-area claim does not fit, is made while a task is live, or names no slot of the area" ERROR:CODE E-TASK-USER
-3606 s" a semaphore initialized twice" ERROR:CODE E-TASK-SEM-STATE
-3607 s" a semaphore count out of range" ERROR:CODE E-TASK-SEM-COUNT
-3609 s" a join of a task never activated or already joined" ERROR:CODE E-TASK-JOIN
-3611 s" a pooled semaphore handle that is not a pooled record" ERROR:CODE E-TASK-SEM-POOL
-3612 s" the task ended without storing a result; carried by the err arm of a join" ERROR:CODE E-TASK-NO-RESULT
-3614 s" a TASK:SLEEP or TASK:WAIT-MS duration below zero or past the last deadline a cell holds" ERROR:CODE E-TASK-SLEEP-MS
-3800 s" the engine's own executable path cannot be resolved" ERROR:FAULT E-ENGINE-PATH
-3801 s" the engine's own binary cannot be content-hashed" ERROR:FAULT E-ENGINE-KEY
-3803 s" an interval handed to prof-rate that is below 0 or past the longest the OS timer holds" ERROR:CODE E-PROF-RATE
-3804 s" an evaluated text or a loaded file that left cells on the stack above its floor" ERROR:CODE E-EVAL-RESIDUE
-4000 s" ssh could not reach the host (connect or auth)" ERROR:FAULT E-ZED-UNREACH
-4001 s" a remote command exited nonzero" ERROR:CODE E-ZED-RC
-4002 s" an scp or rsync artifact transfer failed" ERROR:CODE E-ZED-PUT
-4003 s" a required remote tool is missing (ptxas, nvcc, ...)" ERROR:FAULT E-ZED-TOOLCHAIN
-4004 s" a bad harness argument or buffer capacity" ERROR:CODE E-ZED-ARG
-4005 s" a device is required but HABU_ZED is unset or 0" ERROR:CODE E-ZED-DISABLED
-4100 s" a loader word used after the file defined its name: load files before the definition that reuses it, or rename that definition" ERROR:CODE E-DISC-SHADOW
-4101 s" a dynamic (non-literal) loader path: write the path as a literal" ERROR:CODE E-DISC-DYNAMIC
-4102 s" an unsupported string opener before a loader word: write the path as an s-quote string before `required`" ERROR:CODE E-DISC-OPENER
-4103 s" the source ends inside a string literal or a `{:` group" ERROR:CODE E-DISC-UNTERM
-4104 s" a loader path is longer than discovery's path buffer: shorten it" ERROR:CODE E-DISC-CAPACITY
-4105 s" a loader word used after the file retired it: load files before `undefine` retires it" ERROR:CODE E-DISC-RETIRE
-4200 s" a GROUP mode token is missing or not SEQ or PARA" ERROR:CODE E-SUITE-MODE
-4201 s" a GROUP name is missing or a reserved DSL keyword" ERROR:CODE E-SUITE-NAME
-4202 s" a row's argument list ran into a row keyword or the end of input (no ;SUITE)" ERROR:CODE E-SUITE-ROW
-5700 s" a byte offset or active length outside the buffer" ERROR:CODE E-BUF-BOUNDS
-5701 s" a zero, negative or cell-overflowing byte capacity" ERROR:CODE E-BUF-CAPACITY
-5702 s" a buffer in the wrong state for the operation: used after it was disposed, or initialized while it is live" ERROR:CODE E-BUF-STATE
-5800 s" a probe capacity that is not a nonzero power of two" ERROR:CODE E-HM-CAP
-5801 s" a probe scanned every slot without finding an empty slot or the key" ERROR:CODE E-HM-FULL
-5900 s" an unsigned formatter (SB-U or .U) given a negative value" ERROR:CODE E-FMT-DOMAIN
-5901 s" an SB-FIX scaled magnitude |x|*10^k does not fit an i64" ERROR:CODE E-FMT-OVERFLOW
-5902 s" the layout's FMT-ABI band width disagrees with FMT-NUM-CAP" ERROR:FAULT E-FMT-BAND
-6100 s" a narrowing or an index outside the span's byte reach" ERROR:CODE E-SPAN-RANGE
-6101 s" a copy source longer than the destination span's reach" ERROR:CODE E-SPAN-CAPACITY
-6102 s" a negative reach at the mint, or a negative source length" ERROR:CODE E-SPAN-LENGTH
-6400 s" `/`, `mod` or `/mod` given a zero divisor" ERROR:CODE E-DIV-ZERO
-6610 s" the ABI is not one the architecture can run" ERROR:CODE E-CTGT-ABI
-6611 s" the byte order is not one the ABI is defined for" ERROR:CODE E-CTGT-ENDIAN
-6612 s" the pointer width is not one the ABI is defined for" ERROR:CODE E-CTGT-PTR
-6613 s" the raw feature mask holds a bit outside the vocabulary" ERROR:CODE E-CTGT-FEATURE-BITS
-6614 s" the feature set holds a feature the architecture has not" ERROR:CODE E-CTGT-FEATURE
-6615 s" the baseline instruction-set feature is absent" ERROR:CODE E-CTGT-BASE
-6619 s" a backend id that is not positive" ERROR:CODE E-CTGT-ID
-6620 s" contraction is allowed under a bit-exact rewrite licence" ERROR:CODE E-CNUM-CONTRACT
-6630 s" contraction is allowed but the target has no fused multiply-add" ERROR:CODE E-CBIND-CONTRACT
-6860 s" a shared checker-model vector index or table slot is outside its declared table" ERROR:FAULT E-CMP-ROW
-6863 s" the frozen schema's own string pool or table is full" ERROR:FAULT E-CMP-POOL
-8200 s" a general-register set naming a register no routine can hold state in: x18 is platform-reserved, x30 is the link register and has its own field, 31 is the zero register or the stack pointer, or the bit is past the 32-register file" ERROR:CODE E-NEFF-GPR
-8201 s" a floating-register set bit past the 32-register file" ERROR:CODE E-NEFF-FPR
-8202 s" one register in two roles that exclude each other: a declared result that is also declared destroyed" ERROR:CODE E-NEFF-ROLE
-8203 s" a frame size that is negative, not a multiple of the stack alignment, or past the reach of the load and store offset field" ERROR:CODE E-NEFF-FRAME
-8204 s" a stack-pointer delta that rises above the caller's stack, is not a multiple of the stack alignment, is deeper than the declared frame, or is nonzero on a routine whose control returns" ERROR:CODE E-NEFF-SP
-8205 s" a link-register effect that cannot hold: control returns or tail-calls while x30 is declared destroyed" ERROR:CODE E-NEFF-LINK
-8206 s" results declared on a routine that never returns" ERROR:CODE E-NEFF-CONTROL
-8207 s" a trait mask holding a bit outside the vocabulary of things a routine can do" ERROR:CODE E-NEFF-TRAIT
-8208 s" a frame slot the routine cannot address: a width no load or store form carries, a negative or unaligned offset, an offset past the frame, or an offset past the reach of that width's offset field" ERROR:CODE E-NEFF-SLOT
-8209 s" an ordered register list no calling convention can have: more positions than one cell holds, a register no routine can hold state in, one register named at two positions, or bits set past the last position" ERROR:CODE E-NEFF-SEQ
-8210 s" a contract whose declared convention and whose places are two different statements: a data-stack convention naming a register, or a register convention naming a slot of the caller's data stack" ERROR:CODE E-NEFF-CONV
-8407 s" the does> cut the engine recorded is not just past a `does> ` in the definition source: it lies outside the source, or after other bytes - the source and the pending definition disagree" ERROR:FAULT E-NFEED-CUT
-8480 s" a place read as the kind it is not: the register of a data-stack slot, or the slot index of a register" ERROR:CODE E-NEFF-KIND
-8506 s" a branch distance that is not a whole instruction or does not fit the field its form encodes it in, or an adr or adrp distance its 21-bit field cannot hold" ERROR:CODE E-A64EMIT-REACH
-8563 s" an instruction whose source-map offset does not lie inside the emission it belongs to, or that is not instruction aligned" ERROR:FAULT E-NPUB-OFFSET
-8564 s" a reader asked src/compiler/native/emission.f about rows no backend has sealed, or that its RETIRE row has cleared; rows opened over live ones or added to none; a placement asked of an unplaced emission" ERROR:FAULT E-NEMIT-STATE
-8565 s" a row index at or past the count the sealed emission holds, a row offset outside the emission, an empty emission or a call site of neither kind" ERROR:FAULT E-NEMIT-ROW
-8566 s" an emission whose branches were measured from an address that is not the code slot this seam is claiming for it" ERROR:FAULT E-NPUB-PLACE
-8570 s" a compiler entry reached while another one is open, or a checker scan or store write from a callback while a definition compiles" ERROR:CODE E-COMP-STATE
-8572 s" the engine's own check did not certify the pending definition" ERROR:CODE E-COMP-VERDICT
-8573 s" the pending record and the checker's tape name different definitions" ERROR:FAULT E-COMP-NAME
-8574 s" a declaration-owner record is invalid or lacks the field a reader calls" ERROR:FAULT E-COMP-OWNER
-8575 s" a required call ordinal with no successful scan or resolved binding behind it" ERROR:FAULT E-COMP-BINDING
-8576 s" the active checker counted a rejection or borrowed its recovery fact; abandon the unpublished definition and continue" ERROR:CODE E-COMP-RECOVERY
-8579 s" the checker holds no declared effect for the pending definition" ERROR:CODE E-COMP-ARITY
-8600 s" a spelling that denotes no word where the definition naming it is compiled: the open package's two wordlists, the global wordlist, the used publics and the namespace record for a qualified token all answer absent" ERROR:CODE E-NDICT-NAME
-8602 s" the record a spelling denoted carries no definer kind: it was not made by `constant`, `create` or `variable`, or a `does>` clause took that kind away from it, so entering it would run a body rather than collect a decided value" ERROR:CODE E-NDICT-KIND
-8621 s" a prepared body whose size or clause offset is no whole run of its code" ERROR:FAULT E-NPUB-RELOC
-8622 s" the pending record is not the unpublished slot the dictionary count points at" ERROR:FAULT E-NPUB-PENDING
-8623 s" a sealed emission for another machine than the engine publishing it runs on: the code region holds only that machine's instructions, so another's bytes are rejected before anything about the record is asked" ERROR:CODE E-NPUB-TARGET
-8638 s" a string interned in a build's data space after DP moved back inside it: intern every string before moving DP back, or end the build there" ERROR:CODE E-INTERN-REWIND
-8670 s" a register-file description that cannot describe a machine: a file of no registers or more than one set's bit mask holds, a reserved register outside the file it is reserved from, a file whose every register is reserved so nothing is allocatable at all, a call-destroyed register that is not allocatable in the first place, or a spill slot width that is not a positive power of two" ERROR:CODE E-NREGFILE
-8671 s" a machine description that cannot describe a machine: a link register set holding more than one register, a link register or a stack pointer that is not a register of the general file or that the file says is allocatable, a stack alignment that is not a positive power of two, a frame bound that is negative or not a multiple of that alignment, or a negative offset-field reach" ERROR:CODE E-NMACH
-8680 s" a shadow opened over an open one or natively with no address rewriter installed, or its binding, a count or a source function asked of none" ERROR:CODE E-NSHADOW-STATE
-8681 s" a patched record the shadow does not hold, a taken emission whose bytes or rows are not as stated, or a `does>` clause function the taken emission does not have or that starts where the definer enters" ERROR:CODE E-NSHADOW-ROW
-8682 s" an emission taken for another machine than the one the shadow's binding names" ERROR:CODE E-NSHADOW-TARGET
-8790 s" a branch, call or tail branch whose displacement is outside the signed thirty-two bits a rel32 field holds" ERROR:CODE E-X64EMIT-REACH
-8830 s" an operand the x86-64 encoder cannot encode: a value outside its field, a scale other than 1, 2, 4 or 8, or a register the form forbids" ERROR:CODE E-X64ASM-OPERAND
-8840 s" an operand the Thumb encoder cannot encode: a value outside its field, a misaligned value, or a register the form forbids" ERROR:CODE E-A32ASM-OPERAND
-8860 s" an operand the C6000 encoder cannot encode: a value outside or misaligned for its field, registers on two sides, an odd pair register, or a predicate the word cannot take" ERROR:CODE E-C6XASM-OPERAND
-8870 s" a word outside the constructed subset" ERROR:CODE E-C6XFACTS-DECODE
-8880 s" a word the interpreter does not model" ERROR:CODE E-C6XSIM-DECODE
-8881 s" a memory access outside the modelled window, or a misaligned word, fetch or branch target" ERROR:CODE E-C6XSIM-FAULT
-8882 s" the cycle budget or a table of pending writes ran out" ERROR:CODE E-C6XSIM-LIMIT
-8883 s" a register, address or program outside the model" ERROR:CODE E-C6XSIM-OPERAND
-8884 s" an execute packet or write SPRUGH7 3.8 forbids" ERROR:CODE E-C6XSIM-CONFLICT
-8890 s" no helper with that index, or a NOP or branch given to EMIT" ERROR:CODE E-C6XEABI-OPERAND
-8891 s" the program or fixup table is full" ERROR:CODE E-C6XEABI-CAPACITY
-9000 s" the bytes are not a well-formed ELF32 file: a bad magic, header size, program-header table, or segment extent or alignment" ERROR:CODE E-ELF32-FORMAT
-9001 s" an offset, size or index outside the file or its program-header table" ERROR:CODE E-ELF32-BOUNDS
-9002 s" an ELF32 form the reader does not accept: not version 1 little-endian, or PN_XNUM program headers" ERROR:CODE E-ELF32-UNSUPPORTED
-9100 s" a UDP argument out of range: a length, port, timeout or dotted-quad address text" ERROR:CODE E-UDP4-OPERAND
-9110 s" a serial argument out of range: a negative length, or a baud outside 1 to 4294967295" ERROR:CODE E-SERIAL-OPERAND
-9120 s" an XMODEM argument out of range: a sequence past 255, a block other than 128 or 1024 bytes, an unknown check kind, or a payload longer than its block" ERROR:CODE E-XMODEM-OPERAND
-9121 s" a received XMODEM frame is malformed: a bad header byte, length, sequence complement or check value" ERROR:CODE E-XMODEM-FRAME
-9122 s" an XMODEM transfer argument out of range: a wait outside 1..2147483647 ms, a negative length or a bad serial handle" ERROR:CODE E-SXMODEM-OPERAND
-9123 s" the XMODEM transfer ran past its deadline" ERROR:CODE E-SXMODEM-TIMEOUT
-9124 s" the serial port closed during the XMODEM transfer" ERROR:CODE E-SXMODEM-CLOSED
-9125 s" a serial read or write failed during the XMODEM transfer" ERROR:CODE E-SXMODEM-IO
-9126 s" the peer cancelled the XMODEM transfer" ERROR:CODE E-SXMODEM-CANCELLED
-9127 s" the XMODEM transfer ran out of retries" ERROR:CODE E-SXMODEM-RETRIES
-9128 s" the peer sent more bytes than the XMODEM transfer's maximum" ERROR:CODE E-SXMODEM-CAPACITY
-9160 s" a pseudoterminal could not be opened" ERROR:FAULT E-PTY-OPEN
-9161 s" an ioctl on a new pseudoterminal failed" ERROR:FAULT E-PTY-IOCTL
-9162 s" a pseudoterminal write fell short, a child could not be spawned on it, or the pair is already open or has no child" ERROR:CODE E-PTY-IO
-9163 s" a pseudoterminal read, write, path or watch does not fit its buffer or table, or names no watch" ERROR:CODE E-PTY-CAPACITY
-9170 s" a directory could not be opened for listing" ERROR:CODE E-FS-LIST-OPEN
-9171 s" reading a directory's entries failed" ERROR:CODE E-FS-LIST-READ
-9172 s" a directory entry is malformed, or its name holds a newline" ERROR:CODE E-FS-LIST-ENTRY
-9173 s" the listed names do not fit the buffer given" ERROR:CODE E-FS-LIST-CAPACITY
-9180 s" a TCP argument out of range: a length, port, timeout, address or backlog" ERROR:CODE E-TCP4-OPERAND
-9183 s" the OS answered a socket call with a result it cannot give: a send of no bytes, a cancelled wait, a failed fcntl or a failed poll" ERROR:FAULT E-TCP4-RESULT
-9190 s" a queue index or capacity out of range" ERROR:CODE E-QUEUE-OPERAND
-9191 s" a queue used before it is initialized, or initialized while live" ERROR:CODE E-QUEUE-STATE
-9192 s" more queue definitions than the package table holds" ERROR:CODE E-QUEUE-TABLE
-9220 s" a device index out of range, or a terminal line longer than the buffer" ERROR:CODE E-GENIO-OPERAND
-9221 s" a device handle that is stale or closing, or a host with no FIONREAD" ERROR:CODE E-GENIO-STATE
-9222 s" a device read or write failed, or the device faulted earlier" ERROR:CODE E-GENIO-IO
-9223 s" no device row free" ERROR:CODE E-GENIO-FULL
-9230 s" CURL's transfer loop did not start: curl_global_init or curl_multi_init failed, the wake pipe was not made, or the loop's thread did not start" ERROR:FAULT E-CURL-LOOP
-9232 s" a curl handle that is empty, not live or another task's, or a transfer already in flight" ERROR:CODE E-CURL-STATE
-9233 s" libcurl answered a call with a result it cannot give, or a capture that is not whole" ERROR:FAULT E-CURL-RESULT
-9234 s" a curl argument out of range, a header holding CR or LF, or a record index outside the table" ERROR:CODE E-CURL-OPERAND
-9270 s" a span the operation cannot accept, or an RS256 word run while one runs on the same task" ERROR:CODE E-CRYPTO-OPERAND
-9275 s" libcrypto rejected the keyed hash, or answered a digest of the wrong width" ERROR:FAULT E-CRYPTO-MAC
-9277 s" an RS256 key whose encoding, import or validation was rejected" ERROR:CODE E-CRYPTO-KEY
-9278 s" RS256 verification setup or operation failed" ERROR:FAULT E-CRYPTO-VERIFY
-9279 s" RS256 signing setup or generation failed" ERROR:FAULT E-CRYPTO-SIGN
-9280 s" a word that needs the facility before INIT, or a second INIT without RELEASE" ERROR:CODE E-SIGNAL-STATE
-9281 s" the published stub cells read zero: no baked stub, or not the main task" ERROR:FAULT E-SIGNAL-ABI
-9282 s" a signal number outside the range sigaction installs" ERROR:CODE E-SIGNAL-NUMBER
-9283 s" sigaction failed at the install or the restore" ERROR:FAULT E-SIGNAL-INSTALL
-9284 s" the self-pipe answered short of the four-byte signal number" ERROR:FAULT E-SIGNAL-READ
-9285 s" poll of the self-pipe's read end failed with an errno" ERROR:FAULT E-SIGNAL-POLL
-9286 s" closing an end of the self-pipe failed" ERROR:FAULT E-SIGNAL-CLOSE
-9290 s" the host's queues could not be set up: io_uring_setup, a ring mapping, the eventfd or its registration on Linux; the self-pipe or its non-blocking mark on Darwin" ERROR:FAULT E-AIO-SETUP
-9291 s" the host failed a submission or the loop's wait: io_uring_enter failed or took fewer entries than offered, or the eventfd failed, on Linux; poll failed on Darwin" ERROR:FAULT E-AIO-ENTER
-9292 s" no loop running, STOP ending it, or a second START" ERROR:CODE E-AIO-STATE
-9293 s" asynchronous I/O is full: as many operations in flight as the process may hold, no io_uring submission entry free, or as many operations in flight as half a completion ring holds" ERROR:CODE E-AIO-FULL
-9294 s" STOP with an operation its task has still to await" ERROR:CODE E-AIO-BUSY
-9295 s" the group is empty, holds no such ticket, or names no defined group" ERROR:CODE E-AIO-GROUP
-9296 s" a transfer count past its allocation or below zero, a file offset below -1, or a socket address length not positive" ERROR:CODE E-AIO-BOUNDS
-9297 s" Darwin's poll(2) takes at most OPEN_MAX (10240) descriptors per call, and one more would be needed" ERROR:CODE E-AIO-POLL-BOUND
-9330 s" a reserved bit set in a frame's first byte" ERROR:CODE E-WS-RSV
-9331 s" a reserved opcode" ERROR:CODE E-WS-OPCODE
-9332 s" a control frame that is fragmented or carries more than 125 bytes" ERROR:CODE E-WS-CONTROL
-9333 s" a negative payload length, or an extended length with its high bit set or not in its shortest form" ERROR:CODE E-WS-LENGTH
-9334 s" a payload longer than the receiver's bound" ERROR:CODE E-WS-TOO-BIG
-9335 s" a mask bit the sender must not set or must set, or a key the sender cannot use" ERROR:CODE E-WS-MASK
-9340 s" START on a running server, STOP on a stopped one, a hook installed while one runs, a second STATIC-ROOT, a TAKE-OVER outside a running handler or a second one inside it, a BODY$ outside its running handler or after its TAKE-OVER, BEARER:ISSUER!, BEARER:METADATA!, BEARER:METADATA-ROUTES, BEARER:APP! or TEST-ISSUER:CLIENT+ while serving, BEARER:ADMIT? or BEARER:RESOURCE-METADATA before ISSUER!, BEARER:RESOURCE-METADATA or BEARER:METADATA-ROUTES with no METADATA! URL stated, BEARER:CALLER-KEY outside an admitted request of a person or a listed app, or BEARER:TOKEN or BEARER:REFRESH before CLIENT!" ERROR:CODE E-HTTP-STATE
-9341 s" a request or response handle past its request, or one over another worker's slot, or a slot asked by a task that is no worker" ERROR:CODE E-HTTP-HANDLE
-9342 s" a worker count outside 1..MAX-WORKERS, or an idle deadline under 1 ms" ERROR:CODE E-HTTP-WORKERS
-9343 s" more hooks, headers or bound segments than a table holds, or a negative body length" ERROR:CODE E-HTTP-CAPACITY
-9344 s" an empty method or a pattern not starting with /" ERROR:CODE E-HTTP-ROUTE
-9345 s" a bad or non-directory static root, a URL wider than PATH-CAP, or a directory cycle" ERROR:CODE E-HTTP-STATIC
-9346 s" the listener could not be bound, put to listen or asked its own address" ERROR:CODE E-HTTP-SOCKET
-9347 s" a handler's body read that did not complete; the server answers it with the read's rejection, never as a fault" ERROR:CODE E-HTTP-BODY-READ
-9348 s" a Basic user-id holding a control character (a byte below $20, or $7F), which RFC 7617 forbids, or a `:`, at which the server splits user-id from password and so would read another user; use a user-id with neither" ERROR:CODE E-HTTP-BASIC-USER
-9349 s" a Basic password holding a control character (a byte below $20, or $7F), which RFC 7617 forbids: use a password with none, and drop any line end left by reading it" ERROR:CODE E-HTTP-BASIC-PASSWORD
-9350 s" a byte outside the selected alphabet" ERROR:CODE E-BASE64-CHAR
-9351 s" misplaced/forbidden '=', or nonzero unused bits" ERROR:CODE E-BASE64-PAD
-9352 s" padded length not divisible by four, or URL remainder one" ERROR:CODE E-BASE64-LENGTH
-9360 s" this task has no owner frame left" ERROR:CODE E-C2-CAPACITY
-9361 s" a frame operation contradicts its transition state" ERROR:CODE E-C2-STATE
-9370 s" a scheme other than `file`, compared without case, or none" ERROR:CODE E-URI-SCHEME
-9371 s" after `file:`, anything but `//`, an empty or `localhost` authority, and the `/` that starts the path" ERROR:CODE E-URI-AUTHORITY
-9372 s" a `%` without two hex digits after it, or a byte this part of a URI carries only escaped, such as a `?` or `#` in a file URI's path" ERROR:CODE E-URI-ESCAPE
-9373 s" a path to encode that does not start with `/`, so no file URI names it" ERROR:CODE E-URI-RELATIVE
-9374 s" not an absolute URL whose scheme is `http` or `https`: give the whole URL, as `https://host/path`" ERROR:CODE E-URI-HTTP
-9375 s" an http URL's authority that is not a host with an optional port: user information (`@`), an empty host, a byte outside letters, digits, `-._~` and a bracketed address, or a port over 65535; give `host` or `host:port`" ERROR:CODE E-URI-HOST
-9380 s" a header block that names no single decimal body length within the reader's maximum or a charset other than UTF-8, or a line that is not `token: value` ended by CR LF within LINE-CAP" ERROR:CODE E-CONTENT-LENGTH-MALFORMED
-9381 s" end of file inside a header block or a body" ERROR:CODE E-CONTENT-LENGTH-TRUNCATED
-9382 s" a reader used before BIND or after a rejection, NEXT-LENGTH with a body pending, BODY with none, or BIND with a buffer under LINE-CAP + 2 bytes or a negative maximum" ERROR:CODE E-CONTENT-LENGTH-STATE
-9410 s" a send on a socket that is closing, closed or gone, or one whose queue overflowed and lost it" ERROR:CODE E-WS-CLOSED
-9411 s" a close status no endpoint may send: outside 1000..4999, reserved or unassigned" ERROR:CODE E-WS-CODE
-9414 s" SEND-TEXT of bytes that are not UTF-8 (RFC 6455 section 5.6)" ERROR:CODE E-WS-TEXT
-9415 s" ACCEPT or STOP outside a run or while START still starts it, a START while one starts or runs, a STOP while another STOP runs or on WS's own task" ERROR:CODE E-WS-STATE
-9417 s" a WS config's sockets below 1 or past the handle's 32-bit slot field" ERROR:CODE E-WS-LIMIT
-9604 s" the body binds other than its scan's count of locals" ERROR:FAULT E-T0-BINDS
-9608 s" an operation over a width it cannot move" ERROR:FAULT E-T0-BUNDLE

\ HELP names E-THROW-ZERO, which ERROR:CODE declares outside the package, and
\ UNCAUGHT calls HELP.
package ERROR

private

7203 s" a value record's field names a type not yet declared: declare the type before the record" false MINTED E-CHECKER-RECORD-UNRESOLVED
7144 s" a bare name is public in more than one package a `using` imports: call it qualified, as `PKG:NAME`" false MINTED E-USING-AMBIGUOUS
7202 s" a storage definition names a type not yet declared: declare the type before the storage" false MINTED E-CHECKER-STORAGE-UNRESOLVED
7205 s" a cast's signature names a type not yet declared: declare the type before the cast" false MINTED E-CHECKER-IDENTITY-UNRESOLVED
7163 s" a second POLICY clause in one declaration: keep one `POLICY` clause in the declaration header" false MINTED TYPE-DECL:DECL-EVENT:E-DEV-DUP-POLICY
7164 s" the same DERIVE feature twice in one declaration: name each `DERIVE` feature once" false MINTED TYPE-DECL:DECL-EVENT:E-DEV-DUP-DERIVE
7206 s" a second OPAQUE clause in one declaration: keep one `OPAQUE` clause in the declaration header" false MINTED TYPE-DECL:DECL-EVENT:E-DEV-DUP-OPAQUE
7207 s" OPAQUE requires a family declared in a package: declare the `OPAQUE` structure inside a `package`, or drop `OPAQUE`" false MINTED TYPE-DECL:DECL-EVENT:E-DEV-OPAQUE-PACKAGE
7182 s" a declaration reject code was given its reason twice: call `DECL-REJECT:EXPLAIN` once for each code" true MINTED DECL-REJECT:E-REASON-DUP
7192 s" a type declaration names a type not yet declared: declare that type before it" false MINTED TYPE-DECL:DECL-FRONT:E-UNRESOLVED
7122 s" an index outside the entries a buffer holds: pass an index from 0 to one below its count" false MINTED E-LAYOUT-BOUNDS
7134 s" a path is empty, of negative length, or longer than the bytes a path buffer holds: pass a nonempty path of at most `PATH-CAP` bytes" false MINTED E-PATH-RANGE
7165 s" a path holds a 0 byte, which would end it early: remove the 0 byte from the path" false MINTED E-PATH-NUL
7180 s" a script argument index is negative or not below `SCRIPT-ARGC`: pass an index from 0 to one below `SCRIPT-ARGC`" false MINTED E-SCRIPT-ARGV-RANGE
-7231 s" a slot and occurrence name no live definition, retired or reissued since they were selected: select the word again with `DEF-OCC:SELECT`" false MINTED DEF-OCC:E-STALE
-7232 s" the address given to select names no published dictionary record: pass the record of a word the dictionary holds" false MINTED DEF-OCC:E-SELECT
-7234 s" the dictionary ran out of occurrence numbers to issue" true MINTED DEF-OCC:E-EXHAUSTED
-3200 s" a memory request's size is zero, negative or past the largest an allocator takes: ask for a size from 1 to that largest" false MINTED E-MEM-SIZE
-3201 s" the system gave no memory: raise the process's address-space limit (ulimit -v), or reserve less storage" true MINTED E-MEM-MAP
-3203 s" unmapping memory Habu mapped failed" true MINTED E-MEM-UNMAP
-3204 s" an allocator record was filled a second time: call `ALLOC:INIT` once, on a zeroed record" true MINTED E-ALLOC-STATE
7101 s" a type, variant or field name is not a lowercase tail: write it in lower case with no package qualifier, as in `point`" false MINTED TFAM:E-TFAM-CASE
7102 s" a family, variant or field name is declared twice where it must be unique: give each a name no other declaration in its package, family or variant uses" false MINTED TFAM:E-TFAM-DUP
7106 s" a bare type name is public in more than one package a `using` imports: write it qualified, as `PKG:name`" false MINTED TFAM:E-TFAM-AMBIG
7123 s" a family, variant or public field name is a keyword or reads as a number: rename it, since a keyword or a number names no definition" false MINTED TFAM:E-TDECL-DEF-NAME
7125 s" a field name spells a declaration word or a generated word such as `make`, `tag` or `eq`: rename the field" false MINTED TFAM:E-PF-NAME
7107 s" a malformed type declaration, missing its name, a clause token or its closing word: write the declaration word, its name, its clauses, then its closing word" false MINTED TYPE-DECL:E-TDECL-SYNTAX
7108 s" a malformed type parameter header: name each type parameter once, as in `box<a>` or `result<a,b>`" false MINTED TYPE-DECL:E-TDECL-ARITY
7109 s" a field or payload names an unknown type: give each a type declared before it, such as `n`, `^u8`, a `STRUCTURE` or an `ENUM`" false MINTED TYPE-DECL:E-TDECL-PAYLOAD
7110 s" a family, variant or generated name is reserved or already taken: rename it so it is no reserved word and no word the declaration generates" false MINTED TYPE-DECL:E-TDECL-NAME
7116 s" a layout policy that is missing, unknown or unsupported, or that the family's shape does not fit: name `stack-cell-tag`, `packed-tag` or `niche-null` after `POLICY`, or drop the clause" false MINTED TYPE-DECL:E-TDECL-POLICY
7117 s" a sum's payload names the family being declared: drop that payload, since no family can hold itself" false MINTED TYPE-DECL:E-TDECL-RECURSIVE
7118 s" a record takes more items to make, or a type nests deeper, than the checker's depth limit: split the record, or declare an inner part as a type of its own" false MINTED TYPE-DECL:E-TDECL-CAP
7119 s" a `DERIVE` feature is unknown, or a payload type lacks it: derive a feature only when every payload type has it, or drop it from `DERIVE`" false MINTED TYPE-DECL:E-TDECL-DERIVE
7120 s" a declaration would add a family or a generated word to a sealed package: declare the type in a package that is not sealed" false MINTED TYPE-DECL:E-TDECL-SEALED
7204 s" a type declaration's payload names a type not yet declared: declare that type before it" false MINTED TYPE-DECL:E-TDECL-UNRESOLVED
-9880 s" a number given to an enum's `TAG>` is none of its tags: pass a tag the enum declares" false MINTED TYPE-DECL:E-ENUM-TAG

public

\ The code note and the help of an OPEN record of code c; E-THROW-ZERO's has
\ no code note.
: HELP ( n -- )
   {: c:n :}
   c E-THROW-ZERO = if
      REJECTION:HELP
      s" throw a computed code only when it is nonzero: `rc 0<> if rc throw then`"
      REJECTION:EMIT-STR exit
   then
   s" code" REJECTION:NOTE  c REJECTION:EMIT-N
   REJECTION:HELP
   s" catch the code where the program can go on, or remove its cause"
   REJECTION:EMIT-STR ;

\ Sends the record of a throw of code c that nothing caught: the one its throw
\ carried, else one with no place.
: UNCAUGHT ( n -- )
   {: c:n :}
   REJECTION:CARRIED? 0= if c OPEN  c HELP  REJECTION:SAVE then
   REJECTION:SEND-SAVED ;

\ The status a process exits with when nothing caught code n: SYSTEM for a
\ failed map, UNCAUGHT for any other.
: STATUS ( n -- n )
   E-MEM-MAP = if EXIT-STATUS:SYSTEM exit then
   EXIT-STATUS:UNCAUGHT ;

\ Runs q as a process root. A code from 1 to 255 is an exit status the program
\ chose; any other ends the process with the record of the throw nothing caught.
: TOP-LEVEL ( [ -- ] -- )
   {: q :}
   q catch {: rc:n :}
   rc 0= if exit then
   REJECTION:RELEASE
   rc 0 >  rc 256 <  and if rc throw then
   rc UNCAUGHT
   NULL$ rc STATUS die ;

\ Runs q as a just-forked child's root and exits 0 after it, never returning: it
\ clears the catch chain the fork copied (HND-CELL), so no throw resumes the parent's.
: CHILD-TOP ( [ -- ] -- )
   {: q :}
   0 data-base HND-CELL + !
   q TOP-LEVEL
   s" " EXIT-STATUS:SUCCESS die ;

seal-package
;package
