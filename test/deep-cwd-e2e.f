\ An engine started from a long working directory within PATH-CAP loads its
\ entry, and one started from a working directory that does not resolve, here
\ one removed before the engine starts, refuses it by name on one line. It does
\ not show a working directory past PATH-CAP or FS:REMOVE-TREE over such paths:
\ no test makes a path past PATH-CAP (test/AGENTS.md). The children's logs are
\ kept in the tree, which the process removes at exit.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package DEEP-CWD-E2E
private

$4000 constant IO-CAP
30000 constant TIMEOUT-MS
create ROOT PATH-CAP allot variable ROOT-U
create ENTRY PATH-CAP allot variable ENTRY-U
create ENGINE PATH-CAP allot variable ENGINE-U
create MID PATH-CAP allot variable MID-U
create GONE PATH-CAP allot variable GONE-U
create PATH PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUT-U
variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: ENTRY$ ( -- ptr u8 n ) ENTRY ENTRY-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;
: MID$ ( -- ptr u8 n ) MID MID-U @ ;
: GONE$ ( -- ptr u8 n ) GONE GONE-U @ ;

\ TAIL is three 200-byte segments, and MID is ROOT$/deep/TAIL.
200 constant SEG-BYTES
SEG-BYTES 1 + 3 * 1 - constant TAIL-BYTES
create TAIL TAIL-BYTES allot

: TAIL$ ( -- ptr u8 n ) TAIL TAIL-BYTES ;

: TAIL! ( -- )
   TAIL-BYTES 0 ?do
      [char] d i 1 + SEG-BYTES 1 + mod 0= if drop [char] / then TAIL i + c!
   loop ;

: SETUP ( -- )
   s" habu-deep-cwd" FS:MKTEMP-DIR {: a:ptr u:n :}
   a u FS:CLEANUP-TREE+
   a ROOT u BYTE-COPY u ROOT-U !
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CANONICAL TTRUE {: e:ptr eu:n :}
   e ENGINE eu BYTE-COPY eu ENGINE-U !
   ROOT$ s" entry.f" ENTRY FS:JOIN-PATH ENTRY-U !
   ENTRY$ S\" package DEEP-CWD-CHILD\npublic\n: RUN ( -- ) s\q deep cwd: ok\q type cr ;\n;package\nDEEP-CWD-CHILD:RUN\n" FS:WRITE-ALL
   ROOT$ s" gone" GONE FS:JOIN-PATH GONE-U !
   GONE$ FS:MAKE-DIR
   TAIL!
   ROOT$ s" deep" PATH FS:JOIN-PATH {: pathu:n :}
   PATH pathu TAIL$ MID FS:JOIN-PATH MID-U !
   MID$ FS:MAKE-DIRS ;

: SAVE ( ptr u8 n ptr u8 n -- ) {: rel:ptr relu:n data:ptr size:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH {: pathu:n :}
   PATH pathu data size FS:WRITE-ALL ;

: ENGINE-ARGV+ ( -- )
   ENGINE$ >LEN PROC-ARGV+
   s" --load" >LEN PROC-ARGV+
   ENTRY$ >LEN PROC-ARGV+ ;

: RUN-CHILD ( ptr u8 n ptr u8 n -- )
   {: prog:ptr progu:n cwd:ptr cwdu:n :}
   PROC-ENV-INHERIT-MISSING
   prog progu >LEN cwd cwdu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

: KEEP ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n err:ptr erru:n :}
   out outu OUT OUT-U @ SAVE
   err erru ERR ERR-U @ SAVE ;

: FITS ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-ARGV+
   s" /usr/bin/env" MID$ RUN-CHILD
   s" fits.out" s" fits.err" KEEP
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   s" the entry loads from a long directory within PATH-CAP" T-LABEL
   RC @ 0 T=
   OUT OUT-U @ s" deep cwd: ok" CONTAINS? TTRUE ;

: REMOVED ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" -c" >LEN PROC-ARGV+
   S\" rmdir \q$PWD\q && exec \q$0\q --load \q$1\q" >LEN PROC-ARGV+
   ENGINE$ >LEN PROC-ARGV+ ENTRY$ >LEN PROC-ARGV+
   s" /bin/sh" GONE$ RUN-CHILD
   s" gone.out" s" gone.err" KEEP
   s" a removed working directory exits 74" T-LABEL
   RC @ 74 T=
   s" and prints nothing on stdout" T-LABEL
   OUT-U @ 0 T=
   s" and names itself on one line" T-LABEL
   ERR ERR-U @ S\" source root: the working directory does not resolve to a searchable directory within PATH-CAP bytes\n" T$= ;

public

: RUN ( -- )
   T-RESET SETUP FITS REMOVED T-REPORT ;

;package

DEEP-CWD-E2E:RUN
