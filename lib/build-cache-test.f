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
require tools/hb-build-report.f

package BUILD-CACHE-TEST

$4000 constant CAP
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
create EXPECT-BUF FS-PATH-CAP allot
create OUT CAP allot
create ERR CAP allot
create REPORT-BYTES
   65 c, 32 c, 9 c, 34 c, 92 c, 10 c, 66 c,
create REPORT-OUT FS-PATH-CAP allot

variable ROOT-U
variable A-U
variable B-U
variable C-U
variable D-U
variable E-U
variable F-U
variable EXPECT-U

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

: EXPECT$ ( -- ptr u8 n )
   EXPECT-BUF EXPECT-U @ ;

: PATH-ERROR$ ( -- ptr u8 n )
   SB-RESET
   s" E-BUILD-PATH" SB-APPEND
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
   ROOT$ REPORT-BYTES 7 PATH-F F-U PATH! ;

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

: ENV-ADVERSARIAL ( -- )
   PROC-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN F$ >LEN PROC-ENV+
   s" XDG_CACHE_HOME" ENV-EMPTY
   s" HOME" ENV-EMPTY
   s" TMPDIR" ENV-EMPTY ;

: RUN ( -- n n n )
   s" bin/hb" >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: CHECK-PATH-ERROR ( -- )
   RUN {: outu:n erru:n rc:n :}
   rc 0 T=
   erru 0 T=
   OUT outu PATH-ERROR$ T$= ;

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
   CHECK-PATH-ERROR
   E$ EXISTS? TFALSE
   A$ REMOVE-FILE ;

: CHECK-NO-TIER ( -- )
   ERROR-ARGV ENV-NONE
   CHECK-PATH-ERROR ;

: CHECK-FS-WRITABLE ( -- )
   A$ MAKE-DIRS
   A$ FS:WRITABLE-ROOT? TTRUE
   E$ FS:WRITABLE-ROOT? TFALSE
   A$ MODE-0555 CHMOD-MODE
   A$ FS:WRITABLE-ROOT? TFALSE
   A$ MODE-0700 CHMOD-MODE ;

: CHECK-UNWRITABLE-REJECT ( -- )
   A$ MODE-0555 CHMOD-MODE
   ERROR-ARGV ENV-EXPLICIT
   CHECK-PATH-ERROR
   A$ MODE-0700 CHMOD-MODE ;

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
   0 0= 0 0= 0= 0 0= 42 HB-BUILD:RENDER {: a:ptr u:n :}
   a u s\" \qschema\q:\qhb-build-report\q" CONTAINS? TTRUE
   a u s\" \qcache_root\q" CONTAINS? TTRUE
   a u s\" \qcache_source\q:\qexplicit\q" CONTAINS? TTRUE
   a u s\" \qartifact_hit\q:true" CONTAINS? TTRUE
   a u s\" \qobject_hit\q:false" CONTAINS? TTRUE
   a u s\" \qmaker_hit\q:true" CONTAINS? TTRUE
   a u s\" \qelapsed_ns\q:42" CONTAINS? TTRUE
   a u JR-INIT
   JR-NEXT JT-OBJ T=
   s" cache_root" JR-FIND-KEY TTRUE
   JR-TOKEN JT-STR T=
   REPORT-OUT FS-PATH-CAP JR-STR {: rootu:n :}
   REPORT-OUT rootu F$ T$= ;

: MAIN ( -- )
   T-RESET
   PREPARE
   CHECK-EXPLICIT
   CHECK-XDG
   CHECK-HOME
   CHECK-TMP
   CHECK-ADVERSARIAL
   CHECK-NO-FALLBACK
   CHECK-NO-TIER
   CHECK-FS-WRITABLE
   CHECK-UNWRITABLE-REJECT
   CHECK-CONCURRENT-CREATE
   CHECK-OVERRIDE
   CHECK-REPORT
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" build-cache-test: ok" type cr ;

MAIN

;package
