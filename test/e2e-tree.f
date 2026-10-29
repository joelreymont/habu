\ e2e-tree.f - a scratch source tree for an end-to-end row: the checkout's src/
\ and tools/ linked in, a test/ directory for the sources the row copies, and
\ children run in it with their stdout, stderr and exit code kept for the
\ row's asserts and logs. The c2 acceptance rows and test/structure-opaque-e2e.f
\ share it; a row still names the images it needs (test/c2-image-pair.f) itself,
\ so this file requires none.

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require test/suite-budget.f              \ CHILD-MS, every child's hang guard
require test/pcap-result.f

package E2E-TREE
private

$8000 constant IO-CAP

create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot
create TARGET PATH-CAP allot variable TARGET-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC-CELL

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   PCAP-RESULT:>N RC-CELL ! ERR-U ! OUT-U ! ;

public

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
: RC@ ( -- n ) RC-CELL @ ;

: AT-A ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH PATH swap ;

: LINK ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET FS:JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A FS:MAKE-SYMLINK ;

: COPY-TEST ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET FS:JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A FS:COPY-FILE-STREAM ;

\ The tree named n under TMPDIR, with src and tools linked and test/ made.
: TREE ( ptr u8 n -- )
   FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" src" LINK s" tools" LINK
   s" test" AT-A FS:MAKE-DIRS ;

: ARGS ( ptr u8 n -- ) {: source:ptr size:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   source size >LEN PROC-ARGV+ ;

: RUN-ON ( ptr u8 n ptr u8 n -- )
   {: engine:ptr engineu:n cwd:ptr cwdu:n :}
   PROC-ENV-INHERIT-MISSING
   engine engineu >LEN cwd cwdu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: RUN-IMAGE ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n source:ptr sourceu:n :}
   source sourceu ARGS
   image imageu AT-A ROOT$ RUN-ON ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n input:ptr inputu:n :}
   PROC-ENV-INHERIT-MISSING
   image imageu AT-A >LEN ROOT$ >LEN input inputu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT ;

: SAVE-LOG ( ptr u8 n ptr u8 n -- )
   {: outpath:ptr outu:n errpath:ptr erru:n :}
   outpath outu AT-A OUT$ FS:WRITE-ALL
   errpath erru AT-A ERR$ FS:WRITE-ALL ;

: NEED-OK ( -- )
   RC@ 0<> if OUT$ type ERR$ type then
   RC@ 0 T= ;

: EXPECT-OK ( ptr u8 n -- ) {: marker:ptr markeru:n :}
   NEED-OK
   OUT$ marker markeru CONTAINS? TTRUE ;

;package
