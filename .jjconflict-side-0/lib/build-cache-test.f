\ build-cache-test.f - canonical cache-root selection and report fixtures.
\ Run: bin/hb --load lib/build-cache-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-root.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/build-cache.f
require lib/json-write.f
require lib/float.f
require lib/json-read.f
require lib/test/mapped.f
require tools/hb-build-report.f

package BUILD-CACHE-TEST

$4000 constant CAP
$400 constant MAX-ROOT-U
$401 constant OVER-ROOT-U
$92 constant MODE-0222
$16D constant MODE-0555
$1C0 constant MODE-0700
120000 constant TIMEOUT-MS

create ROOT-BUF FS-PATH-CAP allot
create PATH-A FS-PATH-CAP allot
create PATH-B FS-PATH-CAP allot
create PATH-C FS-PATH-CAP allot
create PATH-D FS-PATH-CAP allot
create PATH-E FS-PATH-CAP allot
create PATH-F FS-PATH-CAP allot
create PATH-G FS-PATH-CAP allot
create PATH-H FS-PATH-CAP allot
create EXPECT-BUF FS-PATH-CAP allot
create OUT CAP allot
create ERR CAP allot
create REPORT-BYTES
   65 c, 32 c, 9 c, 34 c, 92 c, 10 c, 66 c,
create REPORT-OUT FS-PATH-CAP allot
here CELL 1- and CELL swap - CELL 1- and allot
create REPORT-JSON-STATE JR:STORAGE-BYTES allot
create MAX-ROOT MAX-ROOT-U allot
create OVER-ROOT OVER-ROOT-U allot

variable ROOT-U
variable A-U
variable B-U
variable C-U
variable D-U
variable E-U
variable F-U
variable G-U
variable H-U
variable EXPECT-U

$7E constant ROOT-C

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: A$ ( -- ptr u8 n )
   PATH-A A-U @ ;

: B$ ( -- ptr u8 n )
   PATH-B B-U @ ;

: C$ ( -- ptr u8 n )
   PATH-C C-U @ ;

: D$ ( -- ptr u8 n )
   PATH-D D-U @ ;

: E$ ( -- ptr u8 n )
   PATH-E E-U @ ;

: F$ ( -- ptr u8 n )
   PATH-F F-U @ ;

: G$ ( -- ptr u8 n )
   PATH-G G-U @ ;

: H$ ( -- ptr u8 n )
   PATH-H H-U @ ;

: EXPECT$ ( -- ptr u8 n )
   EXPECT-BUF EXPECT-U @ ;

: ROOT-FILL ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      ROOT-C over a + c!
      1+
   repeat drop ;

: PATH-ERROR$ ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: source:ptr sourceu:n root:ptr rootu:n cause:ptr causeu:n :}
   SB-RESET
   s" E-BUILD-PATH" SB-APPEND
   9 SB-APPEND-C
   source sourceu SB-APPEND
   9 SB-APPEND-C
   root rootu SB-APPEND
   9 SB-APPEND-C
   cause causeu SB-APPEND
   10 SB-APPEND-C
   SB$ ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: base:ptr baseu:n name:ptr nameu:n dst:ptr up:ptr :}
   base baseu name nameu dst JOIN-PATH up ! ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-build-cache" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" explicit cache [x]" PATH-A A-U PATH!
   ROOT$ s" xdg cache" PATH-B B-U PATH!
   ROOT$ s" home cache" PATH-C C-U PATH!
   ROOT$ s" tmp cache" PATH-D D-U PATH!
   ROOT$ s" forbidden fallback" PATH-E E-U PATH!
   ROOT$ REPORT-BYTES 7 PATH-F F-U PATH!
   ROOT$ s" blocked parent" PATH-G G-U PATH!
   G$ s" child" PATH-H H-U PATH!
   MAX-ROOT MAX-ROOT-U ROOT-FILL
   OVER-ROOT OVER-ROOT-U ROOT-FILL ;

: ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/build-cache-probe.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+ ;

: QUIET-ARGV ( -- )
   ARGV
   s" --quiet" >LEN PROC-ARGV+ ;

: ERROR-ARGV ( -- )
   ARGV
   s" --expect-path-error" >LEN PROC-ARGV+ ;

: ENV-EMPTY ( ptr u8 n -- )
   >LEN s" " >LEN PROC-ENV+ ;

: ENV-EXPLICIT ( -- )
   PROC-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN A$ >LEN PROC-ENV+
   s" XDG_CACHE_HOME" >LEN B$ >LEN PROC-ENV+
   s" HOME" >LEN C$ >LEN PROC-ENV+
   s" TMPDIR" >LEN D$ >LEN PROC-ENV+ ;

: ENV-XDG ( -- )
   PROC-ENV-RESET
   s" HABU_BUILD_CACHE" ENV-EMPTY
   s" XDG_CACHE_HOME" >LEN B$ >LEN PROC-ENV+
   s" HOME" >LEN C$ >LEN PROC-ENV+
   s" TMPDIR" >LEN D$ >LEN PROC-ENV+ ;

: ENV-HOME ( -- )
   PROC-ENV-RESET
   s" HABU_BUILD_CACHE" ENV-EMPTY
   s" XDG_CACHE_HOME" ENV-EMPTY
   s" HOME" >LEN C$ >LEN PROC-ENV+
   s" TMPDIR" >LEN D$ >LEN PROC-ENV+ ;

: ENV-TMP ( -- )
   PROC-ENV-RESET
   s" HABU_BUILD_CACHE" ENV-EMPTY
   s" XDG_CACHE_HOME" ENV-EMPTY
   s" HOME" ENV-EMPTY
   s" TMPDIR" >LEN D$ >LEN PROC-ENV+ ;

: ENV-NONE ( -- )
   PROC-ENV-RESET
   s" HABU_BUILD_CACHE" ENV-EMPTY
   s" XDG_CACHE_HOME" ENV-EMPTY
   s" HOME" ENV-EMPTY
   s" TMPDIR" ENV-EMPTY ;

: ENV-BAD ( -- )
   PROC-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN A$ >LEN PROC-ENV+
   s" XDG_CACHE_HOME" >LEN E$ >LEN PROC-ENV+
   s" HOME" ENV-EMPTY
   s" TMPDIR" ENV-EMPTY ;

: ENV-CREATE-FAIL ( -- )
   PROC-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN H$ >LEN PROC-ENV+
   s" XDG_CACHE_HOME" ENV-EMPTY
   s" HOME" ENV-EMPTY
   s" TMPDIR" ENV-EMPTY ;

: ENV-ADVERSARIAL ( -- )
   PROC-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN F$ >LEN PROC-ENV+
   s" XDG_CACHE_HOME" ENV-EMPTY
   s" HOME" ENV-EMPTY
   s" TMPDIR" ENV-EMPTY ;

: RUN ( -- n n n )
   s" bin/hb" >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: CHECK-PATH-ERROR ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n root:ptr rootu:n cause:ptr causeu:n :}
   RUN {: outu:n erru:n rc:n :}
   rc 0 T=
   erru 0 T=
   OUT outu source sourceu root rootu cause causeu PATH-ERROR$ T$= ;

: EXPECTED! ( ptr u8 n ptr u8 n -- ) {: source:ptr sourceu:n root:ptr rootu:n :}
   sourceu 1 + rootu + 1 + FS-PATH-CAP > if E-FS-CAPACITY throw then
   source EXPECT-BUF sourceu BYTE-COPY
   9 EXPECT-BUF sourceu + c!
   root EXPECT-BUF sourceu 1 + + rootu BYTE-COPY
   10 EXPECT-BUF sourceu 1 + rootu + + c!
   sourceu 1 + rootu + 1 + EXPECT-U ! ;

: CHECK-RUN ( ptr u8 n ptr u8 n -- )
   EXPECTED!
   RUN {: outu:n erru:n rc:n :}
   rc 0 T=
   erru 0 T=
   OUT outu EXPECT$ T$= ;

: CHECK-EXPLICIT ( -- )
   ARGV ENV-EXPLICIT
   s" explicit" A$ CHECK-RUN
   A$ DIR? TTRUE
   B$ EXISTS? TFALSE ;

: CHECK-XDG ( -- )
   ARGV ENV-XDG
   B$ s" habu-build" PATH-C C-U PATH!
   s" xdg" C$ CHECK-RUN
   C$ DIR? TTRUE
   ROOT$ s" home cache" PATH-C C-U PATH! ;

: CHECK-HOME ( -- )
   ARGV ENV-HOME
   C$ s" .cache/habu-build" PATH-B B-U PATH!
   s" home" B$ CHECK-RUN
   B$ DIR? TTRUE
   ROOT$ s" xdg cache" PATH-B B-U PATH! ;

: CHECK-TMP ( -- )
   ARGV ENV-TMP
   D$ s" habu-build" PATH-B B-U PATH!
   s" tmp" B$ CHECK-RUN
   B$ DIR? TTRUE
   ROOT$ s" xdg cache" PATH-B B-U PATH! ;

: CHECK-ADVERSARIAL ( -- )
   ARGV ENV-ADVERSARIAL
   s" explicit" F$ CHECK-RUN
   F$ DIR? TTRUE ;

: WRITE-BAD-ROOT ( -- )
   A$ s" not a directory" WRITE-ALL ;

: CHECK-NO-FALLBACK ( -- )
   A$ EXISTS? if A$ REMOVE-TREE then
   WRITE-BAD-ROOT
   ERROR-ARGV ENV-BAD
   s" explicit" A$ s" E-FS-DIR" CHECK-PATH-ERROR
   E$ EXISTS? TFALSE
   A$ REMOVE-FILE ;

: CHECK-NO-TIER ( -- )
   ERROR-ARGV ENV-NONE
   s" none" s" " s" E-FS-PATH" CHECK-PATH-ERROR ;

: CHECK-FS-WRITABLE ( -- )
   A$ MAKE-DIRS
   A$ FS:WRITABLE-ROOT? TTRUE
   E$ FS:WRITABLE-ROOT? TFALSE
   A$ MODE-0222 CHMOD-MODE
   A$ FS:WRITABLE-ROOT? TFALSE
   A$ MODE-0700 CHMOD-MODE
   A$ MODE-0555 CHMOD-MODE
   A$ FS:WRITABLE-ROOT? TFALSE
   A$ MODE-0700 CHMOD-MODE ;

: CHECK-UNWRITABLE-REJECT ( -- )
   A$ MODE-0555 CHMOD-MODE
   ERROR-ARGV ENV-EXPLICIT
   s" explicit" A$ s" E-FS-IO" CHECK-PATH-ERROR
   A$ MODE-0700 CHMOD-MODE ;

: CHECK-NO-SEARCH-REJECT ( -- )
   A$ MODE-0222 CHMOD-MODE
   ERROR-ARGV ENV-EXPLICIT
   s" explicit" A$ s" E-FS-IO" CHECK-PATH-ERROR
   A$ MODE-0700 CHMOD-MODE ;

: CHECK-CREATE-FAIL ( -- )
   G$ s" parent file" WRITE-ALL
   ERROR-ARGV ENV-CREATE-FAIL
   s" explicit" H$ s" E-FS-IO" CHECK-PATH-ERROR
   G$ REMOVE-FILE ;

: PREPARE-CONCURRENT ( -- )
   A$ EXISTS? if A$ REMOVE-TREE then
   QUIET-ARGV ENV-EXPLICIT ;

: WAIT-OK ( pid -- )
   PROC-WAIT-RC MATCH result
      ok OF 0 T= ENDOF
      err OF drop 1 0 T= ENDOF
   ;MATCH ;

: CHECK-CONCURRENT-CREATE ( -- )
   PREPARE-CONCURRENT
   s" bin/hb" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-ENV-IO {: a:pid :}
   PREPARE-CONCURRENT
   s" bin/hb" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-ENV-IO {: b:pid :}
   a WAIT-OK
   b WAIT-OK
   A$ DIR? TTRUE ;

: CHECK-OVERRIDE ( -- )
   BUILD-CACHE:RESET
   A$ BUILD-CACHE:ROOT!
   BUILD-CACHE:RESOLVE BUILD-CACHE:SOURCE$ s" explicit" T$=
   {: root:ptr rootu:n :}
   root rootu A$ T$=
   BUILD-CACHE:SOURCE BUILD-CACHE:SOURCE$ s" explicit" T$=
   BUILD-CACHE:ROOT$ A$ T$= ;

: CHECK-REPORT ( -- )
   BUILD-CACHE:RESET
   F$ BUILD-CACHE:ROOT!
   BUILD-CACHE:ROOT$ BUILD-CACHE:SOURCE
   0 0= 0 0= 0= 0 0= 0 0= 0 0= 0= 42 HB-BUILD:CAPTURE
   HB-BUILD:REPORT$ {: a:ptr u:n :}
   a u s\" \qschema\q:\qhb-build-report\q" CONTAINS? TTRUE
   a u s\" \qcache_root\q" CONTAINS? TTRUE
   a u s\" \qcache_source\q:\qexplicit\q" CONTAINS? TTRUE
   a u s\" \qartifact_hit\q:true" CONTAINS? TTRUE
   a u s\" \qobject_hit\q:false" CONTAINS? TTRUE
   a u s\" \qmaker_hit\q:true" CONTAINS? TTRUE
   a u s\" \qmaker_built\q:true" CONTAINS? TTRUE
   a u s\" \qmaker_ran\q:false" CONTAINS? TTRUE
   a u s\" \qelapsed_ns\q:42" CONTAINS? TTRUE
   HB-BUILD:CACHE-ROOT$ F$ T$=
   HB-BUILD:CACHE-SOURCE BUILD-CACHE:SOURCE$ s" explicit" T$=
   HB-BUILD:ARTIFACT-HIT? TTRUE
   HB-BUILD:OBJECT-HIT? TFALSE
   HB-BUILD:MAKER-HIT? TTRUE
   HB-BUILD:MAKER-BUILT? TTRUE
   HB-BUILD:MAKER-RAN? TFALSE
   HB-BUILD:ELAPSED-NS 42 T=
   REPORT-JSON-STATE JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT JR:T-OBJ T=
   s" cache_root" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-STR T=
   REPORT-OUT FS-PATH-CAP JR:STR {: rootu:n :}
   REPORT-OUT rootu F$ T$=
   JR:CLOSE ;

\ A longer root grows the select buffer; each growth must leave exactly ONE live
\ span. Runs first, while the buffer is still unallocated, so 16 -> 128 -> 384
\ are three genuine growth steps; the `<>` assertions fail loudly rather than
\ passing vacuously if a reorder ever leaves a larger buffer behind.
: CHECK-GROWTH-RELEASE ( -- )
   BUILD-CACHE:RESET
   MAX-ROOT 16 BUILD-CACHE:ROOT!
   BUILD-CACHE:SELECTED-ROOT$ drop {: p0:ptr :}
   MAX-ROOT 128 BUILD-CACHE:ROOT!
   BUILD-CACHE:SELECTED-ROOT$ drop {: p1:ptr :}
   p0 p1 <> TTRUE
   p0 MAPPED:LIVE? TFALSE
   p1 MAPPED:LIVE? TTRUE
   MAX-ROOT 384 BUILD-CACHE:ROOT!
   BUILD-CACHE:SELECTED-ROOT$ drop {: p2:ptr :}
   p1 p2 <> TTRUE
   p1 MAPPED:LIVE? TFALSE
   p2 MAPPED:LIVE? TTRUE ;

: CHECK-ERROR-REPORT ( -- )
   BUILD-CACHE:RESET
   A$ BUILD-CACHE:ROOT!
   A$ MODE-0222 CHMOD-MODE
   [: BUILD-CACHE:RESOLVE drop 2drop ;] catch E-BUILD-PATH T=
   BUILD-CACHE:SELECTED? TTRUE
   BUILD-CACHE:SELECTED-ROOT$ A$ T$=
   BUILD-CACHE:SELECTED-SOURCE BUILD-CACHE:SOURCE$ s" explicit" T$=
   BUILD-CACHE:CAUSE E-FS-IO T=
   HB-BUILD:PATH-ERROR$ {: json:ptr jsonu:n :}
   json jsonu s\" \qcode\q:\qE-BUILD-PATH\q" CONTAINS? TTRUE
   json jsonu s\" \qcache_source\q:\qexplicit\q" CONTAINS? TTRUE
   json jsonu s\" \qcause\q:\qE-FS-IO\q" CONTAINS? TTRUE
   HB-BUILD:PATH-ERROR-TEXT$ s" hb-build: schema=hb-build-error version=1 code=E-BUILD-PATH cache_selected=true" CONTAINS? TTRUE
   A$ MODE-0700 CHMOD-MODE ;

: CHECK-RETRY-CLEARS-CAUSE ( -- )
   BUILD-CACHE:RESET
   A$ BUILD-CACHE:ROOT!
   A$ MODE-0222 CHMOD-MODE
   [: BUILD-CACHE:RESOLVE drop 2drop ;] catch E-BUILD-PATH T=
   BUILD-CACHE:CAUSE E-FS-IO T=
   A$ MODE-0700 CHMOD-MODE
   BUILD-CACHE:RESOLVE {: root:ptr rootu:n source:BUILD-CACHE:source :}
   root rootu A$ T$=
   source BUILD-CACHE:SOURCE$ s" explicit" T$=
   BUILD-CACHE:CAUSE 0 T=
   BUILD-CACHE:CAUSE$ s" none" T$= ;

: CHECK-ADVERSARIAL-TEXT ( -- )
   BUILD-CACHE:RESET
   F$ BUILD-CACHE:ROOT!
   F$ MODE-0222 CHMOD-MODE
   [: BUILD-CACHE:RESOLVE drop 2drop ;] catch E-BUILD-PATH T=
   HB-BUILD:PATH-ERROR-TEXT$ {: text:ptr textu:n :}
   text textu STR-LF COUNT-CHAR 0 T=
   text textu STR-TAB COUNT-CHAR 0 T=
   text textu s" cache_selected=true" CONTAINS? TTRUE
   JSON-WRITE:RESET F$ JSON-WRITE:STRING
   text textu JSON-WRITE:$ CONTAINS? TTRUE
   F$ MODE-0700 CHMOD-MODE ;

: CHECK-OVER-ROOT-EVIDENCE ( -- )
   BUILD-CACHE:RESET
   [: OVER-ROOT OVER-ROOT-U BUILD-CACHE:ROOT! ;] catch E-BUILD-PATH T=
   BUILD-CACHE:SELECTED? TTRUE
   BUILD-CACHE:SELECTED-ROOT$ OVER-ROOT OVER-ROOT-U T$=
   BUILD-CACHE:SELECTED-SOURCE BUILD-CACHE:SOURCE$ s" explicit" T$=
   BUILD-CACHE:CAUSE E-FS-CAPACITY T= ;

: CHECK-LONG-ERROR-TEXT ( -- )
   BUILD-CACHE:RESET
   MAX-ROOT MAX-ROOT-U BUILD-CACHE:ROOT!
   [: BUILD-CACHE:RESOLVE drop 2drop ;] catch E-BUILD-PATH T=
   BUILD-CACHE:SELECTED-ROOT$ MAX-ROOT MAX-ROOT-U T$=
   HB-BUILD:PATH-ERROR-TEXT$ {: text:ptr textu:n :}
   textu MAX-ROOT-U > TTRUE
   text textu ROOT-C COUNT-CHAR MAX-ROOT-U T=
   text textu s" cause=E-FS-IO" CONTAINS? TTRUE ;

: CHECK-REPORT-LIFECYCLE ( -- )
   HB-BUILD:RESET
   HB-BUILD:VALID? TFALSE
   [: HB-BUILD:CACHE-ROOT$ 2drop ;] catch E-BUILD-STATUS T=
   BUILD-CACHE:RESET
   F$ BUILD-CACHE:ROOT!
   F$ BUILD-CACHE:SOURCE
   0 0= 0 0= 0= 0 0= 0 0= 0 0= 0= 42 HB-BUILD:CAPTURE
   HB-BUILD:VALID? TTRUE
   HB-BUILD:RESET
   HB-BUILD:VALID? TFALSE
   [: HB-BUILD:REPORT$ 2drop ;] catch E-BUILD-STATUS T= ;

: MAIN ( -- )
   T-RESET
   PREPARE
   CHECK-GROWTH-RELEASE
   CHECK-EXPLICIT
   CHECK-XDG
   CHECK-HOME
   CHECK-TMP
   CHECK-ADVERSARIAL
   CHECK-NO-FALLBACK
   CHECK-NO-TIER
   CHECK-FS-WRITABLE
   CHECK-UNWRITABLE-REJECT
   CHECK-NO-SEARCH-REJECT
   CHECK-CREATE-FAIL
   CHECK-CONCURRENT-CREATE
   CHECK-OVERRIDE
   CHECK-REPORT
   CHECK-ERROR-REPORT
   CHECK-RETRY-CLEARS-CAUSE
   CHECK-ADVERSARIAL-TEXT
   CHECK-OVER-ROOT-EVIDENCE
   CHECK-LONG-ERROR-TEXT
   CHECK-REPORT-LIFECYCLE
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" build-cache-test: ok" type cr ;

MAIN

;package
