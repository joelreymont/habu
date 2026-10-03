\ native-builder-image-lib.f - the fixture the saved native builder rows share.
\ tools/native-builder-image.f is a qualified donor with the native build
\ closure loaded, saved as an application image: run in a tree, it builds that
\ tree as tools/native-build.f does without compiling the closure again. The
\ rows run one such builder, the keyed image test/saved-builder.f saves once per
\ tree, and each owns one claim:
\   test/native-builder-image-e2e.f       a private tree's product runs a word
\                                         the builder's tree lacks and loads C2
\                                         programs
\   test/native-builder-image-whitebox.f  `-- out whitebox` writes the source
\                                         path's unsealed engine, byte for byte
\   test/native-builder-image-refusals.f  bad arguments and bad source publish
\                                         nothing
\ Byte parity with the source path is asserted for the unsealed class only; the
\ sealed product is checked by running it (e2e), so a seal that the saved
\ builder's restored checker rows write differently from the source path's is
\ not caught by bytes. No row checks sealed parity: that row would run two
\ engine builds, the saved builder's and tools/native-build.f's from one tree,
\ measured at 54 s and 76 s CPU (157 s and 161 s wall) on one loaded host,
\ past the 180 s a row may take in the pool (test/gate-stdlib-cases.f).
\ test/gate-stdlib-cases.f registers each file as a row of its own: at most one
\ engine build each keeps a row inside the pool's row deadline. Each row prints
\ the builder's path and its private directory, which keeps the products and
\ their name maps.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/time.f
require test/saved-builder.f
require test/tree-copy-lib.f

package NATIVE-BUILDER-IMAGE-TEST

$4000 constant CAP
1800000 constant BUILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot          variable ROOT-U
create TREE FS-PATH-CAP allot          variable TREE-U
create TMP FS-PATH-CAP allot           variable TMP-U
create IMAGE FS-PATH-CAP allot         variable IMAGE-U
create REPL FS-PATH-CAP allot          variable REPL-U
create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: REPL$ ( -- ptr u8 n ) REPL REPL-U @ ;

: ROOT-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   ROOT$ a u dst JOIN-PATH up ! ;

: TREE-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   TREE$ a u dst JOIN-PATH up ! ;

: ELAPSED ( n ptr u8 n -- ) {: start:n label:ptr size:n :}
   s" native-builder-image ms " type label size type s" : " type
   TIME:MONO-NS start - 1000000 / . ;

\ The row's private root under HB_TMP, its scratch and the keyed builder's path,
\ taken before any child's argv is staged (test/saved-builder.f). The builder
\ builds the checkout until the row makes a private tree.
: SETUP ( ptr u8 n -- ) {: tag:ptr tagu:n :}
   tag tagu HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   SOURCE-ROOT:CWD$ {: cwd:ptr cwdu:n :}
   cwd TREE cwdu BYTE-COPY cwdu TREE-U !
   s" tmp" TMP TMP-U ROOT-PATH! TMP$ MAKE-DIRS
   tag tagu type s"  artifacts: " type ROOT$ type cr
   TIME:MONO-NS {: start:n :}
   SAVED-BUILDER:PATH$ {: img:ptr imgu:n :}
   img IMAGE imgu BYTE-COPY imgu IMAGE-U !
   start s" saved-builder" ELAPSED
   tag tagu type s"  builder: " type IMAGE$ type cr ;

\ A private copy of the build's sources makes an edit observable without
\ touching the checkout. REPL$ is the prefix file the e2e row edits: the build
\ compiles it under the checker, after everything an edit there may use.
: PRIVATE-TREE ( -- )
   s" tree" TREE TREE-U ROOT-PATH! TREE$ MAKE-DIRS
   TREE$ TREE-COPY:BUILD-SOURCES
   s" src/habu/repl.f" REPL REPL-U TREE-PATH! ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc RC>N RC ! ENDOF
   ;MATCH ;

: SUCCESS ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ;

: ARGV+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ENV! ( bool -- ) {: white:bool :}
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_WHITEBOX_IMAGE" >LEN
   white if s" 1" else NULL$ then >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ The saved builder run in TREE$; `--` before the output path is optional for
\ an application image, and each row passes it one way.
: SAVED-BUILD ( ptr u8 n bool bool -- )
   {: path:ptr pathu:n white:bool separator:bool :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   separator if s" --" ARGV+ then
   path pathu ARGV+
   white if s" whitebox" ARGV+ then
   white ENV!
   IMAGE$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

;package
