\ process-env.f - checked argv/env process helpers and PATH lookup.
\
\ Kept separate from process-argv so old native seeds can still run the
\ build-fixpoint installer before this newer primitive exists.
\
\ CHILD ENVIRONMENT CEILING. The prepared environment is not a fixed 256 rows.
\ Its table and byte buffer are sized, the first time a builder allocates, from
\ the envp this process was actually started with: room for every parent entry
\ plus PROC-ENV-EXTRA rows and PROC-ENV-EXTRA-BYTES bytes the caller adds
\ itself. So PROC-ENV-INHERIT-MISSING always fits, whatever a developer shell
\ or a CI runner exports, and only the caller's own additions have a ceiling.
\ The inherited-default table is all caller rows, so it stays at PROC-ENV-EXTRA
\ rows and PROC-ENV-EXTRA-BYTES bytes.
\
\ COST, for a parent envp of n entries and b bytes: (n + PROC-ENV-EXTRA + 1)
\ cells and b + PROC-ENV-EXTRA-BYTES bytes for the prepared environment, plus
\ (PROC-ENV-EXTRA + 1) cells and PROC-ENV-EXTRA-BYTES bytes for the defaults -
\ about 290KB of mapping for a 16KB environment, and none of it is allocated
\ until a builder is used. IMAGE-LIFECYCLE releases all four and clears the
\ measured sizes, so a restored image re-measures its own envp.
\
\ Every capacity refusal writes one line to stderr naming what filled up, the
\ count it saw and the ceiling, then throws E-PROC-ENV.

require lib/errors.f
require lib/string.f
require lib/string-roles.f               \ package STR: the typed string surface
require lib/memory.f
require lib/codegen.f                    \ the refusal line is built in a CODEGEN buffer
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/adt/option.f                 \ option<len> for the FIND-EXECUTABLE cluster (switchover wave A)

1024 constant PROC-ENV-EXTRA             \ rows a caller may add beyond the parent's environment
$20000 constant PROC-ENV-EXTRA-BYTES     \ bytes those added rows may occupy
61 constant PROC-ENV-EQUAL
58 constant PROC-PATH-SEP
47 constant PROC-PATH-SLASH
variable PROC-ENV-N
variable PROC-ENV-OFF
variable PROC-ENV-I
variable PROC-PATH-I
TYPED-VARIABLE PROC-ENV-TABLE-A ptr ptr u8
TYPED-VARIABLE PROC-ENV-BUF-A ptr u8
variable PROC-ENV-DEF-N
variable PROC-ENV-DEF-OFF
TYPED-VARIABLE PROC-ENV-DEF-TABLE-A ptr ptr u8
TYPED-VARIABLE PROC-ENV-DEF-BUF-A ptr u8
variable PROC-ENV-CAP-N                  \ 0 until the parent envp has been measured
variable PROC-ENV-BUF-CAP-N
variable PROC-ENV-INHERITED-N            \ the parent envp's entry count, as measured

\ The parent's environment as it actually is, walked from the startup envp
\ vector. Both walks stop at the NULL terminator the kernel wrote, so neither
\ depends on a compile-time guess about how much a shell exports.
: PROC-ENVP-COUNT ( -- n )
   0 begin dup ENVP 0= 0= while 1 + repeat ;

: PROC-ENVP-BYTES ( -- n )
   0 0 begin over ENVP 0= 0= while
      over ENVP ZLEN 1 + +
      swap 1 + swap
   repeat nip ;

\ Measure once per process, and again after an image restore, which clears all
\ three cells. Every capacity question goes through PROC-ENV-CAP,
\ PROC-ENV-BUF-CAP or PROC-ENV-INHERITED, so none of them can read an
\ unmeasured ceiling.
: PROC-ENV-MEASURE ( -- )
   PROC-ENV-CAP-N @ 0= 0= if exit then
   PROC-ENVP-BYTES PROC-ENV-EXTRA-BYTES + PROC-ENV-BUF-CAP-N !
   PROC-ENVP-COUNT PROC-ENV-INHERITED-N !
   PROC-ENV-INHERITED-N @ PROC-ENV-EXTRA + PROC-ENV-CAP-N ! ;

: PROC-ENV-CAP ( -- n )
   PROC-ENV-MEASURE PROC-ENV-CAP-N @ ;

: PROC-ENV-BUF-CAP ( -- n )
   PROC-ENV-MEASURE PROC-ENV-BUF-CAP-N @ ;

: PROC-ENV-INHERITED ( -- n )
   PROC-ENV-MEASURE PROC-ENV-INHERITED-N @ ;

\ ---- capacity refusals -----------------------------------------------------
\ A bare E-PROC-ENV told a gate nothing: a shell with 251 exported variables
\ plus a fixture's own three died as `uncaught throw code -2505`. Each refusal
\ below names the ceiling, how it was arrived at, and the count that broke it,
\ on stderr, exactly as a load-time diagnostic does.
2 constant PROC-ENV-DIAG-FD              \ stderr
$0A constant PROC-ENV-DIAG-LF
$100 constant PROC-ENV-DIAG-CAP          \ one refusal line: its wording and three decimals
PROC-ENV-DIAG-CAP CODEGEN:BUFFER PROC-ENV-DIAG

: PROC-ENV-DIAG+ ( ptr u8 n -- )
   PROC-ENV-DIAG CODEGEN:APPEND-STRING ;

: PROC-ENV-DIAG-N ( n -- )
   PROC-ENV-DIAG CODEGEN:APPEND-DECIMAL ;

: PROC-ENV-DIAG-LINE ( -- )
   PROC-ENV-DIAG-LF PROC-ENV-DIAG CODEGEN:APPEND-BYTE
   PROC-ENV-DIAG CODEGEN:CONTENTS {: a:ptr u:n :}
   PROC-ENV-DIAG-FD a u write drop ;

: PROC-ENV-REPORT-FULL ( n -- ) {: saw:n :}
   PROC-ENV-DIAG CODEGEN:RESET
   s" process-env: child environment full at " PROC-ENV-DIAG+
   PROC-ENV-CAP PROC-ENV-DIAG-N
   s"  entries (" PROC-ENV-DIAG+
   PROC-ENV-INHERITED PROC-ENV-DIAG-N
   s"  inherited + " PROC-ENV-DIAG+
   PROC-ENV-EXTRA PROC-ENV-DIAG-N
   s"  added): entry " PROC-ENV-DIAG+
   saw PROC-ENV-DIAG-N
   s"  refused" PROC-ENV-DIAG+
   PROC-ENV-DIAG-LINE ;

: PROC-ENV-DEF-REPORT-FULL ( n -- ) {: saw:n :}
   PROC-ENV-DIAG CODEGEN:RESET
   s" process-env: inherited defaults full at " PROC-ENV-DIAG+
   PROC-ENV-EXTRA PROC-ENV-DIAG-N
   s"  entries: entry " PROC-ENV-DIAG+
   saw PROC-ENV-DIAG-N
   s"  refused" PROC-ENV-DIAG+
   PROC-ENV-DIAG-LINE ;

: PROC-ENV-REPORT-BYTES ( n n -- ) {: need:n limit:n :}
   PROC-ENV-DIAG CODEGEN:RESET
   s" process-env: environment buffer full at " PROC-ENV-DIAG+
   limit PROC-ENV-DIAG-N
   s"  bytes: " PROC-ENV-DIAG+
   need PROC-ENV-DIAG-N
   s"  needed" PROC-ENV-DIAG+
   PROC-ENV-DIAG-LINE ;

package PROC-ENV-LIFECYCLE
private

TYPED-VARIABLE REGISTERED bool
false REGISTERED !

: RELEASE-MAPPING ( ptr ptr a n -- ) {: field:ptr bytes:n :}
   field @ {: mapping:ptr :}
   NULL-PTR field !
   mapping 0= if exit then
   mapping BYTE-VIEW bytes MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: RELEASE ( -- )
   0 >COUNT PROC-ENV-N !
   0 >OFF PROC-ENV-OFF !
   0 >COUNT PROC-ENV-DEF-N !
   0 >OFF PROC-ENV-DEF-OFF !
   PROC-ENV-TABLE-A PROC-ENV-CAP 1+ cells RELEASE-MAPPING
   PROC-ENV-BUF-A PROC-ENV-BUF-CAP RELEASE-MAPPING
   PROC-ENV-DEF-TABLE-A PROC-ENV-EXTRA 1+ cells RELEASE-MAPPING
   PROC-ENV-DEF-BUF-A PROC-ENV-EXTRA-BYTES RELEASE-MAPPING
   0 PROC-ENV-CAP-N !
   0 PROC-ENV-BUF-CAP-N !
   0 PROC-ENV-INHERITED-N !
   false REGISTERED ! ;

public

\ One hook owns all four caches, including partially allocated pairs.
\ Installing it first also covers an allocation that throws before publication.
: REGISTER ( -- )
   REGISTERED @ if exit then
   [: RELEASE ;] IMAGE-LIFECYCLE:REGISTER
   true REGISTERED ! ;

;package

\ The PATH scan reads a split field as STR:SPLIT-NEXT states it: a NUM:byte-len
\ field length and a NUM:byte-off cursor. Both have to cross into the engine's
\ own len/off roles, because PROC-TRY-PATH-SEG, JOIN-PATH and the PROC-PATH-I
\ cursor cell are stated in those and that chain reaches this file's public entry.
\ Two checked role-to-role casts at this file's scope do the crossing, so no raw
\ cell exists in between and NUM is not reopened. (A NUM role cannot be
\ stored directly: `!` takes ( a ptr a ) and a type variable does not bind an
\ arity-zero family, which is what dot habu-nominal-storage-raw-a3430ef2 is for.)
CAST: PROC-SEG>LEN ( NUM:byte-len -- len )
CAST: PROC-CURSOR>OFF ( NUM:byte-off -- off )

: PROC-ENV-TABLE@ ( -- ptr ptr u8 )
   PROC-ENV-TABLE-A @ ;

: PROC-ENV-TABLE! ( ptr ptr u8 -- )
   PROC-ENV-TABLE-A ! ;

: PROC-ENV-TABLE ( -- ptr ptr u8 )
   PROC-ENV-TABLE@ 0= if
      PROC-ENV-LIFECYCLE:REGISTER
      PROC-ENV-CAP 1 + >COUNT MEM-ALLOC-CELLS PROC-ENV-TABLE!
   then
   PROC-ENV-TABLE@ ;

: PROC-ENV-BUF@ ( -- ptr u8 )
   PROC-ENV-BUF-A @ ;

: PROC-ENV-BUF! ( ptr u8 -- )
   PROC-ENV-BUF-A ! ;

\ PROC-ENV-BUF-CAP is the measured envp bytes plus a positive constant, so it is
\ always positive: MEM:BYTES-ALLOC-LEN narrows the raw size to the validated
\ alloc role before MEM:ALLOC-BYTES, throwing E-MEM-SIZE on any refusal
\ (unreachable here). Same narrowing guards the default env buffer below.
: PROC-ENV-BUF ( -- ptr u8 )
   PROC-ENV-BUF@ 0= if
      PROC-ENV-LIFECYCLE:REGISTER
      PROC-ENV-BUF-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop PROC-ENV-BUF!
   then
   PROC-ENV-BUF@ ;

: PROC-ENV-DEF-TABLE@ ( -- ptr ptr u8 )
   PROC-ENV-DEF-TABLE-A @ ;

: PROC-ENV-DEF-TABLE! ( ptr ptr u8 -- )
   PROC-ENV-DEF-TABLE-A ! ;

: PROC-ENV-DEF-TABLE ( -- ptr ptr u8 )
   PROC-ENV-DEF-TABLE@ 0= if
      PROC-ENV-LIFECYCLE:REGISTER
      PROC-ENV-EXTRA 1 + >COUNT MEM-ALLOC-CELLS PROC-ENV-DEF-TABLE!
   then
   PROC-ENV-DEF-TABLE@ ;

: PROC-ENV-DEF-BUF@ ( -- ptr u8 )
   PROC-ENV-DEF-BUF-A @ ;

: PROC-ENV-DEF-BUF! ( ptr u8 -- )
   PROC-ENV-DEF-BUF-A ! ;

: PROC-ENV-DEF-BUF ( -- ptr u8 )
   PROC-ENV-DEF-BUF@ 0= if
      PROC-ENV-LIFECYCLE:REGISTER
      PROC-ENV-EXTRA-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop PROC-ENV-DEF-BUF!
   then
   PROC-ENV-DEF-BUF@ ;

: PROC-ENV-TRUE ( -- bool )
   0 0= ;

: PROC-ENV-FALSE ( -- bool )
   0 0= 0= ;

: PROC-SPAWN-ARGV-ENV-RAW ( ptr u8 ptr a ptr a fd fd fd -- pid )
   {: pathz:ptr argv:ptr envp:ptr infd outfd errfd :}
   pathz argv envp infd FD>N outfd FD>N errfd FD>N spawn-argv-env-io >PID ;

: PROC-ENV-RESET ( -- )
   0 >COUNT PROC-ENV-N !
   0 >OFF PROC-ENV-OFF ! ;

: PROC-ENV-DEFAULT-RESET ( -- )
   0 >COUNT PROC-ENV-DEF-N !
   0 >OFF PROC-ENV-DEF-OFF ! ;

: PROC-ENV-SLOT ( idx -- ptr ptr u8 ) {: idx :}
   idx IDX>N 0 < if E-PROC-ENV throw then
   idx IDX>N PROC-ENV-CAP > if E-PROC-ENV throw then
   idx IDX>N cells PROC-ENV-TABLE + ;

: PROC-ENV-CHECK-EXTRA ( -- )
   PROC-ENV-N @ COUNT>N {: have:n :}
   have PROC-ENV-CAP < if exit then
   have 1 + PROC-ENV-REPORT-FULL
   E-PROC-ENV throw ;

: PROC-ENV-DEF-CHECK-EXTRA ( -- )
   PROC-ENV-DEF-N @ COUNT>N {: have:n :}
   have PROC-ENV-EXTRA < if exit then
   have 1 + PROC-ENV-DEF-REPORT-FULL
   E-PROC-ENV throw ;

: PROC-ENV-CHECK-BYTES ( n -- ) {: need:n :}
   need PROC-ENV-BUF-CAP <= if exit then
   need PROC-ENV-BUF-CAP PROC-ENV-REPORT-BYTES
   E-PROC-ENV throw ;

: PROC-ENV-DEF-CHECK-BYTES ( n -- ) {: need:n :}
   need PROC-ENV-EXTRA-BYTES <= if exit then
   need PROC-ENV-EXTRA-BYTES PROC-ENV-REPORT-BYTES
   E-PROC-ENV throw ;

: PROC-ENV-HAS-EQUAL? ( ptr u8 len -- bool ) {: a:ptr u :}
   0 begin dup u LEN>N < while
      dup a + c@ PROC-ENV-EQUAL = if drop PROC-ENV-TRUE exit then
      1+
   repeat drop PROC-ENV-FALSE ;

: PROC-ENV-CHECK-NAME ( ptr u8 len -- ) {: a:ptr u :}
   u LEN>N 0 <= if E-PROC-ENV throw then
   0 begin dup u LEN>N < while
      dup a + c@ PROC-ENV-EQUAL = if E-PROC-ENV throw then
      1+
   repeat drop ;

: PROC-ENV-CHECK-ENTRY ( ptr u8 len -- ) {: a:ptr u :}
   u LEN>N 0 <= if E-PROC-ENV throw then
   a c@ PROC-ENV-EQUAL = if E-PROC-ENV throw then
   a u PROC-ENV-HAS-EQUAL? 0= if E-PROC-ENV throw then ;

: PROC-ENV-STORE-Z ( ptr u8 len -- ptr u8 ) {: a:ptr u :}
   u LEN>N 0 < if E-PROC-ENV throw then
   PROC-ENV-OFF @ {: off :}
   off OFF>N u LEN>N 1 + + PROC-ENV-CHECK-BYTES
   a PROC-ENV-BUF off OFF>N + u LEN>N BYTE-COPY
   0 PROC-ENV-BUF off OFF>N + u LEN>N + c!
   off OFF>N u LEN>N 1 + + >OFF PROC-ENV-OFF !
   PROC-ENV-BUF off OFF>N + ;

: PROC-ENV-INSTALL-Z ( ptr u8 -- )
   PROC-ENV-N @ COUNT>N >IDX PROC-ENV-SLOT !
   PROC-ENV-N @ COUNT>N 1+ >COUNT PROC-ENV-N ! ;

: PROC-ENV-DEF-SLOT ( idx -- ptr ptr u8 ) {: idx:idx :}
   idx IDX>N 0 < if E-PROC-ENV throw then
   idx IDX>N PROC-ENV-EXTRA > if E-PROC-ENV throw then
   idx IDX>N cells PROC-ENV-DEF-TABLE + ;

: PROC-ENV-DEF-INSTALL-Z ( ptr u8 -- )
   PROC-ENV-DEF-N @ COUNT>N >IDX PROC-ENV-DEF-SLOT !
   PROC-ENV-DEF-N @ COUNT>N 1+ >COUNT PROC-ENV-DEF-N ! ;

: PROC-ENV-ENTRY+ ( ptr u8 len -- ) {: a:ptr u :}
   a u PROC-ENV-CHECK-ENTRY
   PROC-ENV-CHECK-EXTRA
   a u PROC-ENV-STORE-Z PROC-ENV-INSTALL-Z ;

: PROC-ENV-NAME-LEN ( ptr u8 len -- len ) {: a:ptr u :}
   0 begin dup u LEN>N < while
      dup a + c@ PROC-ENV-EQUAL = if >LEN exit then
      1+
   repeat >LEN ;

: PROC-ENV-SAME-NAME? ( ptr u8 len ptr u8 len -- bool ) {: a:ptr u b:ptr v :}
   a u PROC-ENV-NAME-LEN {: au :}
   b v PROC-ENV-NAME-LEN {: bv :}
   au LEN>N bv LEN>N <> if PROC-ENV-FALSE exit then
   a au LEN>N b bv LEN>N STR= ;

: PROC-ENV-SLOT-NAME? ( ptr u8 len idx -- bool ) {: a:ptr u idx :}
   idx PROC-ENV-SLOT @ {: z:ptr :}
   a u z z ZLEN >LEN PROC-ENV-SAME-NAME? ;

: PROC-ENV-NAME-IDX ( ptr u8 len -- n ) {: a:ptr u:len :}
   0 begin dup PROC-ENV-N @ COUNT>N < while
      dup >IDX PROC-ENV-I !
      a u PROC-ENV-I @ PROC-ENV-SLOT-NAME? if exit then
      1+
   repeat drop -1 ;

: PROC-ENV-HAS-NAME? ( ptr u8 len -- bool )
   PROC-ENV-NAME-IDX 0 >= ;

: PROC-ENV-DEFAULT$? ( ptr u8 len -- ptr u8 len bool ) {: a:ptr u:len :}
   0 >IDX begin dup IDX>N PROC-ENV-DEF-N @ COUNT>N < while
      dup PROC-ENV-I !
      PROC-ENV-I @ PROC-ENV-DEF-SLOT @ {: z:ptr :}
      a u z z ZLEN >LEN PROC-ENV-SAME-NAME? if
         drop
         z ZLEN >LEN {: zu:len :}
         z zu PROC-ENV-NAME-LEN {: nameu:len :}
         z nameu LEN>N + 1 + zu LEN>N nameu LEN>N - 1 - >LEN
         PROC-ENV-TRUE
         exit
      then
      IDX>N 1+ >IDX
   repeat drop
   s" " >LEN PROC-ENV-FALSE ;

: PROC-ENV-ROW-Z ( ptr u8 len ptr u8 len -- ptr u8 ) {: name:ptr nameu:len val:ptr valu:len :}
   name nameu PROC-ENV-CHECK-NAME
   valu LEN>N 0 < if E-PROC-ENV throw then
   PROC-ENV-OFF @ {: off:off :}
   off OFF>N nameu LEN>N valu LEN>N + 2 + + PROC-ENV-CHECK-BYTES
   name PROC-ENV-BUF off OFF>N + nameu LEN>N BYTE-COPY
   PROC-ENV-EQUAL PROC-ENV-BUF off OFF>N + nameu LEN>N + c!
   val PROC-ENV-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N BYTE-COPY
   0 PROC-ENV-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N + c!
   off OFF>N nameu LEN>N valu LEN>N + 2 + + >OFF PROC-ENV-OFF !
   PROC-ENV-BUF off OFF>N + ;

: PROC-ENV+ ( ptr u8 len ptr u8 len -- ) {: name:ptr nameu:len val:ptr valu:len :}
   PROC-ENV-CHECK-EXTRA
   name nameu val valu PROC-ENV-ROW-Z PROC-ENV-INSTALL-Z ;

\ Replace-or-add: exactly one row for the name survives. Unlike PROC-ENV+,
\ which appends and leaves duplicate-key resolution to the child's getenv,
\ this overwrites an existing row in place (e.g. one copied from the parent's
\ own environment by PROC-ENV-INHERIT-MISSING) and appends only when absent.
: PROC-ENV-SET ( ptr u8 len ptr u8 len -- ) {: name:ptr nameu:len val:ptr valu:len :}
   name nameu PROC-ENV-NAME-IDX {: i:n :}
   i 0 < if name nameu val valu PROC-ENV+ exit then
   name nameu val valu PROC-ENV-ROW-Z i >IDX PROC-ENV-SLOT ! ;

: PROC-ENV-DEFAULT+ ( ptr u8 len ptr u8 len -- ) {: name:ptr nameu:len val:ptr valu:len :}
   name nameu PROC-ENV-CHECK-NAME
   valu LEN>N 0 < if E-PROC-ENV throw then
   PROC-ENV-DEF-CHECK-EXTRA
   PROC-ENV-DEF-OFF @ {: off:off :}
   off OFF>N nameu LEN>N valu LEN>N + 2 + + PROC-ENV-DEF-CHECK-BYTES
   name PROC-ENV-DEF-BUF off OFF>N + nameu LEN>N BYTE-COPY
   PROC-ENV-EQUAL PROC-ENV-DEF-BUF off OFF>N + nameu LEN>N + c!
   val PROC-ENV-DEF-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N BYTE-COPY
   0 PROC-ENV-DEF-BUF off OFF>N + nameu LEN>N + 1 + valu LEN>N + c!
   PROC-ENV-DEF-BUF off OFF>N + PROC-ENV-DEF-INSTALL-Z
   off OFF>N nameu LEN>N valu LEN>N + 2 + + >OFF PROC-ENV-DEF-OFF ! ;

\ The envp array the child receives ends at a null entry; the declared null
\ pointer spells it, because the table now holds `ptr u8` and not a bare cell.
: PROC-ENV-PREPARE ( -- ptr ptr u8 )
   NULL-PTR PROC-ENV-N @ COUNT>N >IDX PROC-ENV-SLOT !
   PROC-ENV-TABLE ;

: PROC-ENV-INHERIT-ONE ( idx -- idx ) {: idx :}
   idx IDX>N ENVP dup ZLEN {: z:ptr u :}
   z u >LEN PROC-ENV-CHECK-ENTRY
   z u >LEN PROC-ENV-HAS-NAME? 0= if z u >LEN PROC-ENV-ENTRY+ then
   idx IDX>N 1+ >IDX ;

: PROC-ENV-INHERIT-DEFAULT-ONE ( idx -- idx ) {: idx:idx :}
   idx PROC-ENV-DEF-SLOT @ {: z:ptr :}
   z z ZLEN >LEN PROC-ENV-CHECK-ENTRY
   z z ZLEN >LEN PROC-ENV-HAS-NAME? 0= if z z ZLEN >LEN PROC-ENV-ENTRY+ then
   idx IDX>N 1+ >IDX ;

: PROC-ENV-INHERIT-DEFAULTS ( -- )
   0 >IDX begin dup IDX>N PROC-ENV-DEF-N @ COUNT>N < while
      PROC-ENV-INHERIT-DEFAULT-ONE
   repeat drop ;

: PROC-ENV-INHERIT-MISSING ( -- )
   PROC-ENV-INHERIT-DEFAULTS
   0 >IDX begin dup IDX>N ENVP 0= 0= while
      PROC-ENV-INHERIT-ONE
   repeat drop ;

: PROC-ARGV-ENV-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: PROC-SPAWN-ARGV-ENV-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PROC-ARGV-PREPARE PROC-ENV-PREPARE infd outfd errfd
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: PROC-RUN-ARGV-ENV-IO-RC ( ptr u8 len fd fd fd -- result<n,n> )   \ ok = clean exit (0), err = nonzero completion rc
   PROC-SPAWN-ARGV-ENV-IO PROC-WAIT-RC ;

: PROC-SPAWN-ARGV-ENV-CAPTURE ( ptr u8 ptr a ptr a -- ) {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp -1 >FD PROC-OUT-W @ >FD PROC-ERR-W @ >FD PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-CAPTURE-PID!
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 ptr a ptr a -- ) {: pathz:ptr argv:ptr envp:ptr :}
   pathz argv envp PROC-IN-R @ >FD PROC-OUT-W @ >FD PROC-ERR-W @ >FD
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-CAPTURE-PID!
   PROC-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: RUN-ARGV-ENV-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

: RUN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

: PROC-HAS-SLASH? ( ptr u8 len -- bool )
   LEN>N STR:LENGTH PROC-PATH-SLASH STR:INDEX-OF MATCH option
     none OF PROC-ENV-FALSE ENDOF
     some OF drop PROC-ENV-TRUE ENDOF
   ;MATCH ;

: PROC-EXECUTABLE? ( ptr u8 len -- bool )
   LEN>N EXECUTABLE? ;

: PROC-COPY-PATH ( ptr u8 len ptr u8 -- len ) {: a:ptr u dst:ptr :}
   u LEN>N 0 < if E-PROC-PATH throw then
   u LEN>N FS-PATH-CAP > if E-PROC-PATH throw then
   a dst u LEN>N BYTE-COPY
   u ;

: PROC-JOIN-PATH-SEG ( ptr u8 len ptr u8 len ptr u8 -- len )
   {: seg:ptr segu cmd:ptr cmdu dst:ptr :}
   segu LEN>N 0= if
      s" ." cmd cmdu LEN>N dst JOIN-PATH >LEN
   else
      seg segu LEN>N cmd cmdu LEN>N dst JOIN-PATH >LEN
   then ;

: PROC-TRY-PATH-SEG ( ptr u8 len ptr u8 len ptr u8 -- option<len> )   \ SOME resolved length if executable, else NONE
   {: seg:ptr segu cmd:ptr cmdu dst:ptr :}
   seg segu cmd cmdu dst PROC-JOIN-PATH-SEG {: gotu :}
   dst gotu PROC-EXECUTABLE? if gotu OPTION:SOME exit then
   OPTION:NONE ;

: FIND-EXECUTABLE-IN-PATH ( ptr u8 len ptr u8 len ptr u8 -- option<len> )   \ SOME resolved length in dst, else NONE
   {: cmd:ptr cmdu path:ptr pathu dst:ptr :}
   cmd cmdu PROC-HAS-SLASH? if
      cmd cmdu PROC-EXECUTABLE? if
         cmd cmdu dst PROC-COPY-PATH OPTION:SOME exit
      then
      OPTION:NONE exit
   then
   0 >OFF PROC-PATH-I !
   begin path pathu LEN>N STR:LENGTH PROC-PATH-SEP PROC-PATH-I @ OFF>N STR:OFFSET STR:SPLIT-NEXT MATCH option
     none OF STR-FALSE ENDOF                        \ no more PATH segments: end the scan
     some OF STR-SPLIT:UNMAKE {: seg:ptr segl:NUM:byte-len nx:NUM:byte-off :}
        nx PROC-CURSOR>OFF PROC-PATH-I !
        seg segl PROC-SEG>LEN cmd cmdu dst PROC-TRY-PATH-SEG MATCH option
          none OF ENDOF                             \ segment miss: try the next one
          some OF OPTION:SOME exit ENDOF            \ resolved: re-wrap and return
        ;MATCH
        STR-TRUE ENDOF
     ;MATCH
   while repeat
   OPTION:NONE ;

: FIND-EXECUTABLE ( ptr u8 len ptr u8 -- option<len> ) {: cmd:ptr cmdu:len dst:ptr :}   \ SOME resolved length via $PATH, else NONE
   s" PATH" GETENV {: path:ptr pathu :}
   pathu 0= if OPTION:NONE exit then
   cmd cmdu path pathu >LEN dst FIND-EXECUTABLE-IN-PATH ;

: RESOLVE-EXECUTABLE ( ptr u8 len ptr u8 -- len )
   FIND-EXECUTABLE MATCH option
     none OF E-PROC-PATH throw ENDOF
     some OF ENDOF
   ;MATCH ;
