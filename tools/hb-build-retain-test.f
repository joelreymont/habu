\ hb-build-retain-test.f - what hb-build leaves in the build cache goes once
\ nothing has used it for a day (src/lib/build-cache.f), through real builds.
\
\ hb-build publishes an artifact hb-build-out-<key>, an object <key>.hbo with
\ its index <key>.idx (src/lib/object-resolve.f), an observed-input manifest
\ hb-build-inputs-<key>, paired artifact/object maps, a maker-support record,
\ and a keyed AOT builder image hb-aot-builder-<key>
\ (tools/aot-builder-image.f). A publish prunes its own family;
\ a hit dates what it found. So this builds two programs into a private root,
\ dates their program entries to 2020, past the window, and then
\ - builds the second program again, which restores its artifact;
\ - builds it with --json-errors, which misses the artifact (the flag is in the
\   artifact's key) and loads the object (it is not in the object's key), and
\   publishes a third artifact;
\ - builds a third program from nothing, publishing an object and an artifact.
\
\ What a wrong retention does, and the check that sees it:
\ - an artifact publish that prunes nothing: the first program's artifact is
\   still there after the third artifact is published;
\ - an object publish that prunes nothing: the first program's object or index
\   is still there after the third program's object is published;
\ - an artifact hit that does not date the artifact: it is gone;
\ - an object hit that does not date both the index and the object it read: one
\   of them is gone;
\ - a prune that takes a recent entry, or the one just published: it is gone;
\ - a publish that leaves its work directory, or a prune its claim: the root
\   holds a name that is none of the entries;
\ - a prune that fails its build: the build throws.
\ The builds run in this process, as tools/hb-build-test-lib.f says a row builds
\ where the CLI is not the subject; src/lib/build-cache-retain-test.f holds what a
\ prune reports.
\ Run: bin/hb --load tools/hb-build-retain-test.f

require src/lib/string.f
require tools/hb-build-test-lib.f
require test/preloaded-engine.f

using BUILD-FIXPOINT                     \ the build tmp root

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

128 constant HBR-NAME-CAP

\ The entries, by the build that published them. Each artifact and object has
\ its map in the following slot, so a pair's two paths have one owner.
0 constant HBR-A-ART
2 constant HBR-A-OBJ
4 constant HBR-A-IDX
5 constant HBR-B-ART
7 constant HBR-B-OBJ
9 constant HBR-B-IDX
10 constant HBR-B2-ART
12 constant HBR-C-ART
14 constant HBR-C-OBJ
16 constant HBR-C-IDX
17 constant HBR-BUILDER
18 constant HBR-SUPPORT
19 constant HBR-A-INPUTS
20 constant HBR-B-INPUTS
21 constant HBR-C-INPUTS
22 constant HBR-SLOTS
-1 constant HBR-NONE

\ hb-build-out-<key>, hb-aot-builder-<key>, <key>.hbo and <key>.idx use
\ 64-digit keys.
77 constant HBR-ART-LEN
79 constant HBR-BUILDER-LEN
68 constant HBR-OBJ-LEN

create HBR-ROOT-BUF FS-PATH-CAP allot
create HBR-SRC-BUF FS-PATH-CAP allot
create HBR-OUT-BUF FS-PATH-CAP allot
create HBR-AT-BUF FS-PATH-CAP allot
create HBR-NAMES HBR-SLOTS HBR-NAME-CAP * allot
HBR-SLOTS TYPED-BUFFER HBR-NAME-LEN n

variable HBR-ROOT-U
variable HBR-SRC-U
variable HBR-OUT-U
variable HBR-AT-U
variable HBR-ART-SLOT
variable HBR-OBJ-SLOT
variable HBR-IDX-SLOT
variable HBR-INPUT-SLOT
variable HBR-FRESH
variable HBR-STRAY

: HBR-ROOT$ ( -- ptr u8 n )
   HBR-ROOT-BUF HBR-ROOT-U @ ;

: HBR-SRC$ ( -- ptr u8 n )
   HBR-SRC-BUF HBR-SRC-U @ ;

: HBR-OUT$ ( -- ptr u8 n )
   HBR-OUT-BUF HBR-OUT-U @ ;

\ The cache root entry with this name.
: HBR-AT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   HBR-ROOT$ a u HBR-AT-BUF JOIN-PATH HBR-AT-U !
   HBR-AT-BUF HBR-AT-U @ ;

\ ---- the entries ---------------------------------------------------------------

: HBR-SLOT-A ( n -- ptr u8 ) {: s:n :}
   HBR-NAMES s HBR-NAME-CAP * + ;

: HBR-SLOT$ ( n -- ptr u8 n ) {: s:n :}
   s HBR-SLOT-A s HBR-NAME-LEN @ ;

: HBR-SLOT! ( ptr u8 n n -- ) {: a:ptr u:n s:n :}
   u HBR-NAME-CAP > if E-STR-CAPACITY throw then
   a s HBR-SLOT-A u BYTE-COPY
   u s HBR-NAME-LEN ! ;

: HBR-SLOTS-RESET ( -- )
   HBR-SLOTS 0 ?do 0 i HBR-NAME-LEN ! loop ;

: HBR-HELD? ( n -- bool )
   HBR-SLOT$ HBR-AT$ FS-TRY-LSTAT ;

: HBR-KNOWN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   HBR-SLOTS 0 ?do
      i HBR-NAME-LEN @ 0 > if
         a u i HBR-SLOT$ STR= if true unloop exit then
      then
   loop
   false ;

: HBR-ART? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" hb-build-out-" STARTS-WITH? u HBR-ART-LEN = and ;

: HBR-OBJ? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .hbo" ENDS-WITH? u HBR-OBJ-LEN = and ;

: HBR-IDX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .idx" ENDS-WITH? u HBR-OBJ-LEN = and ;

: HBR-BUILDER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" hb-aot-builder-" STARTS-WITH? u HBR-BUILDER-LEN = and ;

: HBR-INPUTS? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" hb-build-inputs-" STARTS-WITH? u 80 = and ;

: HBR-SUPPORT? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" hb-maker-support-" STARTS-WITH? u 81 = and ;

\ A definition entry, hb-def-<64 hex>.hbd, which the maker writes per compiled
\ definition; it is neither a kind the build publishes nor a stray.
: HBR-DEF? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" hb-def-" STARTS-WITH? a u s" .hbd" ENDS-WITH? and u 75 = and ;

: HBR-MAP-SLOT ( n -- n )
   dup HBR-NONE <> if 1+ then ;

\ A new entry of a kind the build publishes fills that kind's slot, once.
: HBR-TAKE ( ptr u8 n n -- ) {: a:ptr u:n s:n :}
   s HBR-NONE = if 1 HBR-STRAY +! exit then
   s HBR-NAME-LEN @ 0 > if 1 HBR-STRAY +! exit then
   a u s HBR-SLOT!
   1 HBR-FRESH +! ;

: HBR-NOTE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u HBR-KNOWN? if exit then
   a u HBR-DEF? if exit then
   a u s" .map" ENDS-WITH? if
      a u 4 - HBR-ART? if
         a u HBR-ART-SLOT @ HBR-MAP-SLOT HBR-TAKE exit
      then
      a u 4 - HBR-OBJ? if
         a u HBR-OBJ-SLOT @ HBR-MAP-SLOT HBR-TAKE exit
      then
   then
   a u HBR-ART? if a u HBR-ART-SLOT @ HBR-TAKE exit then
   a u HBR-OBJ? if a u HBR-OBJ-SLOT @ HBR-TAKE exit then
   a u HBR-IDX? if a u HBR-IDX-SLOT @ HBR-TAKE exit then
   a u HBR-BUILDER? if a u HBR-BUILDER HBR-TAKE exit then
   a u HBR-SUPPORT? if a u HBR-SUPPORT HBR-TAKE exit then
   a u HBR-INPUTS? if a u HBR-INPUT-SLOT @ HBR-TAKE exit then
   1 HBR-STRAY +! ;

\ A hit or prune must preserve the map exactly as long as its executable or
\ object, including pairs recorded by an earlier build that have since aged.
: HBR-PAIRS? ( -- bool )
   HBR-SLOTS 0 ?do
      i HBR-SLOT$ s" .map" ENDS-WITH? if
         i HBR-SLOT$ {: map:ptr mapu:n :}
         map mapu 4 - i 1- HBR-SLOT$ STR= 0= if false unloop exit then
         i HBR-HELD? i 1- HBR-HELD? xor if false unloop exit then
      then
   loop
   true ;

\ Name what a build added to the root: the slots for its artifact, object and
\ index, HBR-NONE for a kind it must not publish, and how many it publishes.
\ The first build also records one maker image and its support record.
\ Another name of either kind is a stray. Definition entries the maker writes
\ are counted as neither, so `want` leaves them out. Any other name is a stray.
: HBR-RECORD ( n n n n n -- )
   {: art:n obj:n idx:n inputs:n want:n :}
   art HBR-ART-SLOT !
   obj HBR-OBJ-SLOT !
   idx HBR-IDX-SLOT !
   inputs HBR-INPUT-SLOT !
   0 HBR-FRESH !
   0 HBR-STRAY !
   HBR-ROOT$ [: HBR-NOTE ;] FS-LIST:EACH
   s" the build publishes one entry of each kind it missed" T-LABEL
   HBR-FRESH @ want T=
   s" the root holds nothing but entries: no work directory, no claim" T-LABEL
   HBR-STRAY @ 0 T=
   s" map sidecars follow their artifact or object" T-LABEL
   HBR-PAIRS? TTRUE ;

\ ---- the builds ----------------------------------------------------------------

: HBR-SOURCE! ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n text:ptr textu:n :}
   HBT-ROOT name nameu HBR-SRC-BUF HBR-SRC-U HBT-PATH!
   HBR-SRC$ text textu WRITE-ALL ;

\ One build of the current source into the cache root; json? is --json-errors.
: HBR-BUILD ( ptr u8 n bool -- ) {: out:ptr outu:n json:bool :}
   HBT-ROOT out outu HBR-OUT-BUF HBR-OUT-U HBT-PATH!
   HBR-SRC$ HBR-OUT$ HBT-HBB-PREPARE-AOT
   json if -1 HBB-JSON ! then
   HBT-HBB-BUILD-OUT ;

: HBR-AGE ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" -t" >LEN PROC-ARGV+
   s" 202001010000" >LEN PROC-ARGV+
   HBR-SLOTS 0 ?do
      i HBR-BUILDER <> i HBR-SUPPORT <> and
      i HBR-NAME-LEN @ 0 > and if
         i HBR-SLOT$ HBR-AT$ >LEN PROC-ARGV+
      then
   loop
   s" /usr/bin/touch" >LEN HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rc:n :}
   s" touch dates the two programs' entries to 2020" T-LABEL
   rc 0 T= ;

: HBR-FIRST-BUILDS ( -- )
   s" ra.f" S\" : MAIN ( -- ) 1 . cr ;\n" HBR-SOURCE!
   s" ra" false HBR-BUILD
   HBR-A-ART HBR-A-OBJ HBR-A-IDX HBR-A-INPUTS 8 HBR-RECORD
   s" rb.f" S\" : MAIN ( -- ) 2 . cr ;\n" HBR-SOURCE!
   s" rb" false HBR-BUILD
   HBR-B-ART HBR-B-OBJ HBR-B-IDX HBR-B-INPUTS 6 HBR-RECORD
   HBR-AGE ;

: HBR-HITS ( -- )
   s" rb" false HBR-BUILD
   s" the second build again restores its artifact" T-LABEL
   HBB-ARTIFACT-HIT @ 0 <> TTRUE
   HBR-NONE HBR-NONE HBR-NONE HBR-NONE 0 HBR-RECORD
   s" rb2" true HBR-BUILD
   s" --json-errors misses the artifact and loads the object" T-LABEL
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBR-B2-ART HBR-NONE HBR-NONE HBR-NONE 2 HBR-RECORD
   s" the artifact publish takes the unused artifact" T-LABEL
   HBR-A-ART HBR-HELD? TFALSE
   s" and keeps the one restored since" T-LABEL
   HBR-B-ART HBR-HELD? TTRUE
   s" and the one it published" T-LABEL
   HBR-B2-ART HBR-HELD? TTRUE ;

: HBR-OBJECT-PUBLISH ( -- )
   s" rc.f" S\" : MAIN ( -- ) 3 . cr ;\n" HBR-SOURCE!
   s" rc" false HBR-BUILD
   s" the third build runs its maker" T-LABEL
   HBB-MAKER-RUN @ 0 <> TTRUE
   HBR-C-ART HBR-C-OBJ HBR-C-IDX HBR-C-INPUTS 6 HBR-RECORD
   s" the input publish takes the unused manifest and keeps the reused one" T-LABEL
   HBR-A-INPUTS HBR-HELD? TFALSE
   HBR-B-INPUTS HBR-HELD? TTRUE
   HBR-C-INPUTS HBR-HELD? TTRUE
   s" the object publish takes the unused object" T-LABEL
   HBR-A-OBJ HBR-HELD? TFALSE
   s" and the unused index" T-LABEL
   HBR-A-IDX HBR-HELD? TFALSE
   s" and keeps the object loaded since" T-LABEL
   HBR-B-OBJ HBR-HELD? TTRUE
   s" and the index read since" T-LABEL
   HBR-B-IDX HBR-HELD? TTRUE
   s" and the object and index it published" T-LABEL
   HBR-C-OBJ HBR-HELD? TTRUE
   HBR-C-IDX HBR-HELD? TTRUE
   s" the artifact publish keeps a recent artifact nothing restored" T-LABEL
   HBR-B2-ART HBR-HELD? TTRUE
   s" and the one restored a build ago" T-LABEL
   HBR-B-ART HBR-HELD? TTRUE
   s" and the one it published" T-LABEL
   HBR-C-ART HBR-HELD? TTRUE ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBR-RETAIN-MAIN ( -- )
   T-RESET
   PRELOADED-ENGINE:LINKER$ HBT-KEYED!
   HBT-PREPARE
   HBR-SLOTS-RESET
   HBT-ROOT s" cache" HBR-ROOT-BUF HBR-ROOT-U HBT-PATH!
   HBR-ROOT$ MAKE-DIR
   \ A private root by claim: HBR-BUILDER? counts the builder entry this root
   \ publishes, which the run's shared root would already hold.
   HBR-ROOT$ BUILD-CACHE:ROOT!
   HBR-FIRST-BUILDS
   HBR-HITS
   HBR-OBJECT-PUBLISH
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-retain-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBR-RETAIN-MAIN
