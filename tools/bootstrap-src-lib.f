\ bootstrap-src-lib.f - what tools/bootstrap.sh compiles, read by field role.
\
\ The no-binary recovery script is the audited launcher (docs/bootstrap.md), and
\ two lints have to know which sources one of its engine emissions is built from:
\ tools/bootstrap-mirror-lint.f, which needs the set EVERY emission compiles, and
\ tools/stdin-closure-lint.f, which needs the set the STDIN emission compiles.
\ Both questions are the same parse with a different driver, so the parse lives
\ here once instead of twice - and never as a CONTAINS? over the script text,
\ which is what stdin-closure used to do: a path named in a `#` comment or inside
\ a printf string satisfied it, so commenting a real `cat` row out left the gate
\ green.
\
\ WHAT A ROW IS. The script names its sources in two places and this reads both:
\   ROLE-ARRAY  an entry of the `SRC_COMMON=( ... )` array, which emit_src
\               splices in with `for f in "${SRC_COMMON[@]}"`.
\   ROLE-EMIT   a `cat <path> >> "$out"` row inside `emit_src() { ... }` that
\               THIS driver reaches.
\ Every field is matched by its ROLE, so a path in a comment (`#` leads), inside a
\ string (`printf` leads), with another redirect target, or with no redirect at
\ all is not a row - and neither is a `cat` row outside emit_src, where nothing
\ runs it. The four block openers and closers are compared against the WHOLE line,
\ so they hold only at column 0 where the script writes them; a script that stops
\ matching collects no rows and fails closed.
\
\ THE DRIVER. emit_src takes the driver as its second argument and emits it last
\ (`cat "$driver" >> "$out"`), and it guards rows on which driver it was given:
\   if [[ "$driver" == "src/habu/stdin.f" ]]; then
\ PARSE takes the driver to answer for. `$driver` expands to it, and a guarded
\ block's rows count only when the guard names it. PARSE with an EMPTY driver is
\ the driver-independent question: no guard can match, so its rows are exactly the
\ ones in EVERY emission, and `cat "$driver"` expands to nothing. That is the set
\ bootstrap-mirror-lint has always asked for, and the reason is recorded in
\ LESSONS: a `cat` row inside a driver conditional is in at most one emission, so
\ it is not in the seed.
\
\ A CONDITION THIS READER DOES NOT UNDERSTAND IS NOT TAKEN, and so is an `else`.
\ That is the loud direction for both consumers: a row wrongly left out reads as a
\ source the seed does not compile (bootstrap-mirror-lint reports a missing
\ boot-prefix row) or as a closure entry the launcher does not cat (stdin-closure
\ reports drift). Neither can pass in silence, which a row wrongly let IN could.
\
\ The script is read through LINT-SLAB, once per PARSE. It used to be read twice
\ into two fixed $8000 buffers - one for the array walk, one for the seed list -
\ over a file already ~16K.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, and
\ tools/lint/text.f.

package BOOTSTRAP-SRC
using LINT-SPLIT

private

$0A constant LF
$24 constant DOLLAR
$3B constant SEMI
$80 constant MAX-ROWS               \ ~64 today across both lists
512 constant PATH-CAP

create SLAB LINT-SLAB:CELLS cells allot
create ROW-A MAX-ROWS cells allot
create ROW-U MAX-ROWS cells allot
create ROW-R MAX-ROWS cells allot
create DRV PATH-CAP allot           \ the driver PARSE was asked about
create SCRIPT PATH-CAP allot        \ the script's label, for a consumer's report

variable ROW-N
variable DRV-U
variable SCRIPT-U
variable TEXT-A
variable TEXT-U
variable CUR                        \ line cursor into the script text
variable IN-ARRAY
variable IN-EMIT
variable IF-N                       \ conditional nesting inside emit_src
variable SKIP-N                     \ depth of the innermost branch NOT taken, 0 = none
variable SPLICED                    \ emit_src really expands SRC_COMMON
variable CALLED                     \ some emit_src call site passes this driver

: TEXT-A-FIELD ( -- ptr ptr u8 )
   TEXT-A 0 ptr-field ;

: TEXT-A@ ( -- ptr u8 )
   TEXT-A-FIELD @ ;

: ROW-A-FIELD ( n -- ptr ptr u8 )
   ROW-A swap ptr-field ;

: DRV$ ( -- ptr u8 n )
   DRV DRV-U @ ;

: HAVE-DRV? ( -- bool )
   DRV-U @ 0 <> ;

: ROW+ ( ptr u8 n n -- ) {: a:ptr u:n role:n :}
   ROW-N @ MAX-ROWS >= if
      s" bootstrap-src: row table full" 1 die
   then
   u ROW-N @ cells ROW-U + !
   a ROW-N @ ROW-A-FIELD !
   role ROW-N @ cells ROW-R + !
   ROW-N @ 1+ ROW-N ! ;

\ ---- shell word helpers ------------------------------------------------------
: FIELD= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k SN# @ >= if LINT-FALSE exit then
   k S@ a u LINT-STR= ;

: PEEK-LAST ( ptr u8 n -- ptr u8 n n )     \ last byte of a non-empty span
   2dup 1 - + c@ ;

: UNQUOTE ( ptr u8 n -- ptr u8 n )         \ a shell word without its "..."
   dup 2 < if exit then
   over c@ DQUOTE <> if exit then
   PEEK-LAST DQUOTE <> if exit then
   swap 1 + swap 2 - ;

: STRIP-SEMI ( ptr u8 n -- ptr u8 n )      \ a shell word without its trailing `;`
   dup 0= if exit then
   PEEK-LAST SEMI <> if exit then
   1 - ;

: SHELL-VAR? ( ptr u8 n -- bool )
   dup 0= if 2drop LINT-FALSE exit then
   drop c@ DOLLAR = ;

: FIELD-WORD ( n -- ptr u8 n )             \ field k, unquoted
   S@ UNQUOTE ;

public

0 constant ROLE-ARRAY
1 constant ROLE-EMIT

\ Neither this script nor tools/boot-pin.f spells the two target-selected sources:
\ the script reaches them through the $OS_* variables its `case` sets, boot-pin
\ through the BP-OS-*$ words. Both sides normalise to one key, so those rows still
\ take part. The keys live here because this is the side that mints them.
: OS-TARGET-KEY ( -- ptr u8 n )
   s" src/os/*/target.f" ;

: OS-LAYOUT-KEY ( -- ptr u8 n )
   s" src/os/*/layout.f" ;

\ A shell word that names a source: it ends `.f` and carries a directory. The
\ suffix test is what keeps `"$OS_SYS"` and a bare `then` out.
: PATH-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" LINT-SUFFIX? 0= if LINT-FALSE exit then
   a u s" /" LINT-CONTAINS? ;

private

\ The three variables that name a source. `$driver` is the argument PARSE was
\ asked about; with no driver it expands to nothing, exactly as an emission that
\ has not chosen one compiles no driver. Every other $OS_* entry names a file
\ neither list keys on, so it contributes no row.
: VAR-ROW ( ptr u8 n n -- ) {: a:ptr u:n role:n :}
   a u s" $OS_TARGET" LINT-STR= if OS-TARGET-KEY role ROW+ exit then
   a u s" $OS_LAYOUT" LINT-STR= if OS-LAYOUT-KEY role ROW+ exit then
   a u s" $driver" LINT-STR= 0= if exit then
   HAVE-DRV? 0= if exit then
   DRV$ role ROW+ ;

: PATH-ROW+ ( ptr u8 n n -- ) {: a:ptr u:n role:n :}
   a u SHELL-VAR? if a u role VAR-ROW exit then
   a u PATH-WORD? 0= if exit then
   a u role ROW+ ;

: ARRAY-LINE ( -- )                        \ one line of the SRC_COMMON array
   SN# @ 1 <> if exit then
   0 FIELD-WORD ROLE-ARRAY PATH-ROW+ ;

\ `cat <path> >> "$out"` is the only row shape emit_src writes.
: CAT-ROW? ( -- bool )
   SN# @ 4 <> if LINT-FALSE exit then
   0 s" cat" FIELD= 0= if LINT-FALSE exit then
   2 s" >>" FIELD= 0= if LINT-FALSE exit then
   3 S\" \"$out\"" FIELD= ;

\ The array reaches a stage source only where emit_src expands it; bash splices
\ the entries in at this line, so without it no array entry is compiled at all.
: SPLICE-LINE? ( -- bool )
   SN# @ 5 <> if LINT-FALSE exit then
   0 s" for" FIELD= 0= if LINT-FALSE exit then
   3 S@ STRIP-SEMI S\" \"${SRC_COMMON[@]}\"" LINT-STR= ;

: IF-OPEN? ( -- bool )
   0 s" if" FIELD= ;

: IF-CLOSE? ( -- bool )
   SN# @ 1 <> if LINT-FALSE exit then
   0 s" fi" FIELD= ;

: ELSE-LINE? ( -- bool )
   SN# @ 1 <> if LINT-FALSE exit then
   0 s" else" FIELD= ;

\ The ONE condition shape this reader understands:
\   if [[ "$driver" == "<path>" ]]; then
\ Seven fields, each in its role. Anything else answers false, which is the
\ not-taken direction every consumer fails loud on.
: IF-TAKEN? ( -- bool )
   HAVE-DRV? 0= if LINT-FALSE exit then
   SN# @ 7 <> if LINT-FALSE exit then
   1 s" [[" FIELD= 0= if LINT-FALSE exit then
   2 FIELD-WORD s" $driver" LINT-STR= 0= if LINT-FALSE exit then
   3 s" ==" FIELD= 0= if LINT-FALSE exit then
   5 S\" ]];" FIELD= 0= if LINT-FALSE exit then
   6 s" then" FIELD= 0= if LINT-FALSE exit then
   4 FIELD-WORD DRV$ LINT-STR= ;

: SKIPPING? ( -- bool )
   SKIP-N @ 0 <> ;

: OPEN-IF ( -- )
   IF-N @ 1+ IF-N !
   SKIPPING? if exit then
   IF-TAKEN? if exit then
   IF-N @ SKIP-N ! ;

: CLOSE-IF ( -- )
   SKIP-N @ IF-N @ = if 0 SKIP-N ! then
   IF-N @ 1- IF-N ! ;

\ An `else` branch is never taken. Which half of a conditional runs is a question
\ about the condition, and this reader models exactly one condition; guessing the
\ other half is how a row that is in no emission gets counted as being in every
\ one.
: OPEN-ELSE ( -- )
   IF-N @ 0= if exit then
   SKIPPING? if IF-N @ SKIP-N ! exit then
   IF-N @ SKIP-N ! ;

: EMIT-LINE ( -- )                         \ one line of emit_src
   IF-OPEN? if OPEN-IF exit then
   IF-CLOSE? if CLOSE-IF exit then
   ELSE-LINE? if OPEN-ELSE exit then
   SKIPPING? if exit then
   SPLICE-LINE? if LINT-TRUE SPLICED ! exit then
   CAT-ROW? 0= if exit then
   1 FIELD-WORD ROLE-EMIT PATH-ROW+ ;

\ `emit_src <out> <driver>` - a call site that builds an emission for this driver.
\ Read outside both blocks, because that is where the script calls it.
: CALL-LINE ( -- )
   HAVE-DRV? 0= if exit then
   SN# @ 3 <> if exit then
   0 s" emit_src" FIELD= 0= if exit then
   2 FIELD-WORD DRV$ LINT-STR= 0= if exit then
   LINT-TRUE CALLED ! ;

: SCRIPT-LINE ( ptr u8 n -- )
   2dup s" SRC_COMMON=(" LINT-STR= if 2drop LINT-TRUE IN-ARRAY ! exit then
   2dup s" emit_src() {" LINT-STR= if
      2drop LINT-TRUE IN-EMIT ! 0 IF-N ! 0 SKIP-N ! exit then
   IN-ARRAY @ if
      2dup s" )" LINT-STR= if 2drop LINT-FALSE IN-ARRAY ! exit then
      SPLIT-WHITESPACE ARRAY-LINE exit
   then
   IN-EMIT @ if
      2dup s" }" LINT-STR= if 2drop LINT-FALSE IN-EMIT ! exit then
      SPLIT-WHITESPACE EMIT-LINE exit
   then
   SPLIT-WHITESPACE CALL-LINE ;

\ Lines are walked on this package's own cursor: LINT-SPLIT holds ONE result
\ table, and the field split of each line would overwrite a line split.
: LINE-END ( -- n )                        \ offset of the LF ending the line at the cursor
   CUR @
   begin dup TEXT-U @ < while
      TEXT-A@ over + c@ LF = if exit then
      1 +
   repeat ;

: NEXT-LINE ( -- ptr u8 n )                \ the line at the cursor; cursor moves past it
   LINE-END {: e:n :}
   TEXT-A@ CUR @ +
   e CUR @ -
   e 1 + CUR ! ;

: DRV! ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if s" bootstrap-src: driver name too long" 1 die then
   a DRV u LINT-BMOVE  u DRV-U ! ;

: SCRIPT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if s" bootstrap-src: script name too long" 1 die then
   a SCRIPT u LINT-BMOVE  u SCRIPT-U ! ;

public

\ The script's label, so a consumer's finding names the file it read.
: SCRIPT$ ( -- ptr u8 n )
   SCRIPT SCRIPT-U @ ;

\ Read one script text as the emission built for `driver`. An EMPTY driver asks
\ the driver-independent question. Clears all prior state first.
: PARSE ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n da:ptr du:n :}
   da du DRV!
   0 ROW-N !
   LINT-FALSE IN-ARRAY !  LINT-FALSE IN-EMIT !
   0 IF-N !  0 SKIP-N !
   LINT-FALSE SPLICED !  LINT-FALSE CALLED !
   a TEXT-A-FIELD !  u TEXT-U !  0 CUR !
   begin CUR @ TEXT-U @ < while
      NEXT-LINE SCRIPT-LINE
   repeat ;

\ The same, reading the script from disk through this package's slab.
: LOAD ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n da:ptr du:n :}
   pa pu SCRIPT!
   pa pu SLAB LINT-SLAB:LOAD
   SLAB LINT-SLAB:TEXT da du PARSE ;

: ROWS ( -- n )
   ROW-N @ ;

: ROW$ ( n -- ptr u8 n ) {: i:n :}
   i ROW-A-FIELD @
   i cells ROW-U + @ ;

: ROW-ROLE ( n -- n ) {: i:n :}
   i cells ROW-R + @ ;

\ does this emission compile that source
: HAS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup ROWS < while
      dup ROW$ a u LINT-STR= if drop LINT-TRUE exit then
      1 +
   repeat
   drop LINT-FALSE ;

\ emit_src really splices SRC_COMMON in; without that line no array entry is
\ compiled at all and every ROLE-ARRAY row is a fiction.
: ARRAY-USED? ( -- bool )
   SPLICED @ ;

\ some emit_src call site really builds an emission for this driver
: DRIVER-CALLED? ( -- bool )
   CALLED @ ;

;package
