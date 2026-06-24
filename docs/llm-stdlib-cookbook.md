# LLM Stdlib Cookbook

Use this file for prompt-sized checked examples. The authoritative contract is
`docs/stdlib.md`; the machine-readable signature index is `lib/std.manifest`.
The snippets below name the existing focused tests that already cover the
published words.

Examples assume the needed `lib/*.f` files are loaded before the driver, e.g.
`bin/hb --load lib/errors.f lib/test.f lib/array.f my-driver.f -- args...`. Do
not write `include` in examples; source lists are passed to `hb`.
Project words stay UPPER-CASE, built-ins stay lower-case, and every definition
keeps an explicit checked effect.

## Arrays

Load `lib/errors.f lib/test.f lib/array.f`. The published signatures include
`A@ ( ptr a len idx -- a )`, `A! ( a ptr a len idx -- )`,
`A-SUM ( ptr n len -- n )`, `A-MAP! ( ptr a len [ a -- a ] -- )`,
`A-FOLDI ( ptr a len b [ b idx a -- b ] -- b )`, and
`A-FIND-INDEX ( ptr a len [ a -- bool ] -- n )`. Convert numeric lengths and
indexes with `>LEN` and `>IDX` at public API boundaries. They are covered by
`lib/array-test.f` and `examples/array.f`.

```forth
4 constant AE-LEN

create AE-DATA AE-LEN cells allot

: AE-LOAD ( -- )
   1 AE-DATA AE-LEN >LEN 0 >IDX A!
   2 AE-DATA AE-LEN >LEN 1 >IDX A!
   3 AE-DATA AE-LEN >LEN 2 >IDX A!
   4 AE-DATA AE-LEN >LEN 3 >IDX A! ;

: AE-WEIGHTED ( -- n )
   AE-DATA AE-LEN >LEN 0 [: swap IDX>N * + ;] A-FOLDI ;

: AE-DOUBLE! ( -- )
   AE-DATA AE-LEN >LEN [: 2 * ;] A-MAP! ;

: AE-TEST ( -- )
   T-RESET
   AE-LOAD
   AE-DATA AE-LEN >LEN A-SUM 10 T=
   AE-WEIGHTED 20 T=
   AE-DOUBLE!
   AE-DATA AE-LEN >LEN [: 6 > ;] A-FIND-INDEX 3 T=
   T-REPORT ;
```

## Strings

Load `lib/errors.f lib/string.f lib/test.f`. The published signatures include
`TRIM ( ptr u8 n -- ptr u8 n )`, `STARTS-WITH? ( ptr u8 n ptr u8 n -- bool )`,
`COUNT-CHAR ( ptr u8 n n -- n )`, `STR>NUMBER? ( ptr u8 n -- n bool )`,
`SB-APPEND ( ptr u8 n -- )`, and `SB$ ( -- ptr u8 n )`. They are covered by
`lib/string-test.f` and `examples/string-regex.f`.

```forth
$2D constant SE-DASH

: SE-SUFFIX-NUMBER ( ptr u8 n -- n bool ) {: a:ptr u :}
   a u SE-DASH INDEX-OF {: ix :}
   ix 0 < if 0 STR-FALSE exit then
   a ix 1 + + u ix 1 + - STR>NUMBER? ;

: SE-BUILD-LABEL ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SB-RESET
   s" item-" SB-APPEND
   a u SB-APPEND
   SB$ ;

: SE-TEST ( -- )
   T-RESET
   s"   Habu-2026  " TRIM s" Habu-2026" T$=
   s" Habu-2026" s" Habu" STARTS-WITH? TTRUE
   s" Habu-2026" SE-DASH COUNT-CHAR 1 T=
   s" Habu-2026" SE-SUFFIX-NUMBER TTRUE 2026 T=
   s" 42" SE-BUILD-LABEL s" item-42" T$=
   T-REPORT ;
```

## Maps

Load `lib/errors.f lib/string.f lib/test.f lib/map.f`. The public map storage is
caller-owned `ptr a`, sized by `MAP-CELLS ( count -- count )`, and keys are
counted byte strings whose lengths enter the API as `len`. The published
signatures include `MAP-INIT ( ptr a count -- )`,
`MAP-SET ( n ptr a count ptr u8 len -- )`,
`MAP-GET ( ptr a count ptr u8 len -- n bool )`, and
`MAP-EACH ( ptr a count [ ptr u8 len n -- ] -- )`. Convert numeric capacities
and key lengths with `>COUNT` and `>LEN`. They are covered by
`lib/map-test.f` and `examples/file-map.f`.

```forth
8 constant ME-CAP

create ME-MAP ME-CAP >COUNT MAP-CELLS COUNT>N cells allot

: ME-INIT ( -- )
   ME-MAP ME-CAP >COUNT MAP-INIT ;

: ME-INC ( ptr u8 n -- ) {: key:ptr len :}
   ME-MAP ME-CAP >COUNT key len >LEN MAP-GET if
      1+
   else
      drop 1
   then
   ME-MAP ME-CAP >COUNT key len >LEN MAP-SET ;

: ME-COUNT ( ptr u8 n -- n ) {: key:ptr len :}
   ME-MAP ME-CAP >COUNT key len >LEN MAP-GET if exit then ;

: ME-TEST ( -- )
   T-RESET
   ME-INIT
   s" forth" ME-INC
   s" text" ME-INC
   s" forth" ME-INC
   s" forth" ME-COUNT 2 T=
   s" missing" ME-COUNT 0 T=
   T-REPORT ;
```

## Regex

Load `lib/errors.f lib/string.f lib/test.f lib/regex.f`. Regex bytecode is
caller-owned `ptr u8 len` storage. The published signatures include
`RX-COMPILE ( ptr u8 len ptr u8 len -- len )`,
`RX-MATCH? ( ptr u8 len ptr u8 len -- bool )`,
`RX-FIND ( ptr u8 len ptr u8 len -- off len bool )`, and
`RX-COUNT ( ptr u8 len ptr u8 len -- count )`. They are covered by
`lib/regex-test.f` and `examples/string-regex.f`.

```forth
64 constant RE-RX-CAP

create RE-RX RE-RX-CAP allot
variable RE-RX-U

: RE-RX-PTR ( -- ptr u8 )
   RE-RX ;

: RE-SLUG-RX! ( -- )
   s" ^[a-z]+-[0-9]+$" >LEN RE-RX-PTR RE-RX-CAP >LEN RX-COMPILE LEN>N RE-RX-U ! ;

: RE-SLUG? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u >LEN RE-RX-PTR RE-RX-U @ >LEN RX-MATCH? ;

: RE-TEST ( -- )
   T-RESET
   RE-SLUG-RX!
   s" habu-2026" RE-SLUG? TTRUE
   s" Habu-2026" RE-SLUG? TFALSE
   s" habu-2026" >LEN RE-RX-PTR RE-RX-U @ >LEN RX-FIND TTRUE LEN>N 9 T= OFF>N 0 T=
   T-REPORT ;
```

## Files

Load `lib/errors.f lib/string.f lib/test.f lib/fs.f`; add `lib/fs-mutate.f` only
when the example mutates paths. The published signatures include
`READ-ALL ( ptr u8 n ptr u8 n -- n )`,
`WRITE-ALL ( ptr u8 n ptr u8 n -- )`,
`JOIN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n )`, and
`WALK-FILES ( ptr u8 n [ ptr u8 n -- ] -- )`. They are covered by
`lib/fs-test.f`, `lib/fs-mutate-test.f`, and
`examples/file-map.f`.

```forth
1024 constant FE-BUF-CAP

create FE-BUF FE-BUF-CAP allot
variable FE-FILE#

: FE-CONTAINS? ( ptr u8 n ptr u8 n -- bool ) {: path:ptr pathu needle:ptr needleu :}
   path pathu FE-BUF FE-BUF-CAP READ-ALL {: gotu :}
   FE-BUF gotu needle needleu CONTAINS? ;

: FE-COUNT-FILE ( ptr u8 n -- )
   2drop
   FE-FILE# @ 1+ FE-FILE# ! ;

: FE-COUNT-TREE ( ptr u8 n -- n )
   0 FE-FILE# !
   [: FE-COUNT-FILE ;] WALK-FILES
   FE-FILE# @ ;
```

## Processes

Load `lib/errors.f lib/test.f lib/process.f`; add `lib/process-argv.f` when a
real argv vector is needed. Use checked wrappers such as
`SPAWN-IO ( ptr u8 len fd fd fd -- pid )`, `WAIT-RC ( pid -- rc )`,
`RUN-IO-RC ( ptr u8 len fd fd fd -- rc )`, and
`RUN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )`. They are covered by
`lib/process-test.f`, `lib/process-argv-test.f`, and `docs/process-pty.md`.

```forth
32 constant PE-OUT-CAP
32 constant PE-ERR-CAP

create PE-OUT PE-OUT-CAP allot
create PE-ERR PE-ERR-CAP allot

: PE-TRUE-RC ( -- n )
   s" /usr/bin/true" >LEN -1 >FD -1 >FD -1 >FD SPAWN-IO WAIT-RC RC>N ;

: PE-CAPTURE-TRUE ( -- )
   s" /usr/bin/true" >LEN PE-OUT PE-OUT-CAP >LEN PE-ERR PE-ERR-CAP >LEN 1000 >MS RUN-CAPTURE
   {: outu erru rc :}
   rc RC>N 0 T=
   erru LEN>N 0 T=
   outu LEN>N 0 T= ;

: PE-TEST ( -- )
   T-RESET
   PE-TRUE-RC 0 T=
   PE-CAPTURE-TRUE
   T-REPORT ;
```

## Property Tests

Load `lib/errors.f lib/string.f lib/test.f lib/property.f`. The published
signatures include `PROP-RUN-RESET ( n n -- )`, `PROP-RND% ( n -- n )`,
`PROP-BUF+ ( ptr u8 n -- )`, `PROP-BUF$ ( -- ptr u8 n )`, and
`PROP-SHRINK ( [ -- bool ] -- )`. They are covered by `lib/property-test.f`,
`examples/property-test.f`, and the checker soundness smoke in `test/prop-test.f`.

```forth
17 constant PTE-SEED
32 constant PTE-COUNT
100 constant PTE-BOUND

: PTE-SMALL ( -- n )
   PTE-BOUND PROP-RND% ;

: PTE-SQUARE-PROPERTY ( -- )
   PTE-SEED PTE-COUNT PROP-RUN-RESET
   0 begin dup PROP-COUNT@ < while
      PTE-SMALL dup * 0 >= TTRUE
      1+
   repeat drop ;

: PTE-KEEP-FIRST? ( -- bool )
   PROP-BUF$ nip 4 >= ;

: PTE-SHRINK-EXAMPLE ( -- )
   PROP-BUF-RESET
   s" 123 456 789 " PROP-BUF+
   [: PTE-KEEP-FIRST? ;] PROP-SHRINK
   PROP-BUF$ s" 123 " T$= ;
```

## Builds

Load `lib/errors.f lib/string.f lib/test.f lib/fs.f lib/process.f lib/build.f`.
Build policy belongs in checked Habu. The published signatures include
`BUILD-CHECK ( ptr u8 n -- )`, `BUILD-EXPECT ( ptr u8 n -- )`,
`BUILD-ARTIFACT ( ptr u8 n ptr u8 n -- ptr u8 n )`,
`BUILD-STEP ( ptr u8 n [ -- n ] -- )`, and
`BUILD-RUN ( ptr u8 n ptr u8 n -- n )`. They are covered by `lib/build-test.f`,
`tools/build-fixpoint-test.f`, and the `hb-build` checks inside `test/run.f`.

```forth
create BE-STEP BUILD-STEP-CELLS cells allot

: BE-OK-RC ( -- n )
   0 ;

: BE-CHECK-SOURCE ( ptr u8 n -- )
   BUILD-CHECK ;

: BE-EXPECT-ARTIFACT ( ptr u8 n ptr u8 n -- ) {: root:ptr rootu name:ptr nameu :}
   root rootu name nameu BUILD-ARTIFACT BUILD-EXPECT ;

: BE-RUN-CHECKED-STEP ( -- )
   s" check" [: BE-OK-RC ;] BUILD-STEP ;

: BE-PREPARE-STEP ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: cmd:ptr cmdu tmp:ptr tmpu art:ptr artu :}
   BE-STEP BUILD-STEP-CLEAR
   s" compile" BE-STEP BUILD-STEP-NAME!
   cmd cmdu BE-STEP BUILD-STEP-COMMAND!
   tmp tmpu BE-STEP BUILD-STEP-TMP!
   art artu BE-STEP BUILD-STEP-ARTIFACT! ;
```

## Gate Checklist

Run the focused gate beside the module you used, then the doc-signature gate
when signatures or stdlib prose changed:

```sh
bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/examples-test.f
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/process.f lib/process-argv.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/stdlib-manifest-test.f
Run the full native gate command from `docs/bootstrap.md`.
```
