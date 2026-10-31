\ KEYED-IMAGE:PUBLISH under a key already fresh keeps the image and key a running
\ process may hold; a publish under another key replaces both.

require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/os/script-argv.f
require tools/build-out.f
require tools/keyed-image.f

package KEYED-IMAGE-PUBLISH

$100 constant READ-CAP

create ROOT PATH-CAP allot variable ROOT-U
create OUT-ROOT PATH-CAP allot variable OUT-ROOT-U
create SRC PATH-CAP allot variable SRC-U
create IMAGE PATH-CAP allot variable IMAGE-U
create KEY-PATH PATH-CAP 4 + allot variable KEY-PATH-U
create KEY-A KEYED-IMAGE:KEY-HEX-LEN allot
create KEY-B KEYED-IMAGE:KEY-HEX-LEN allot
create ENGINE-KEY KEYED-IMAGE:KEY-HEX-LEN allot
create READ-BUF READ-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: OUT-ROOT$ ( -- ptr u8 n ) OUT-ROOT OUT-ROOT-U @ ;
: SRC$ ( -- ptr u8 n ) SRC SRC-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: KEY-PATH$ ( -- ptr u8 n ) KEY-PATH KEY-PATH-U @ ;
: FAMILY$ ( -- ptr u8 n ) s" image" ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U ! ;

: KEY! ( n ptr u8 -- ) {: c:n dst:ptr :}
   KEYED-IMAGE:KEY-HEX-LEN 0 ?do c dst i + c! loop ;

: PATHS! ( -- )
   FAMILY$ ENGINE-KEY IMAGE IMAGE-U KEYED-IMAGE:PATH!
   IMAGE KEY-PATH IMAGE-U @ BYTE-COPY
   s" .key" drop KEY-PATH IMAGE-U @ + 4 BYTE-COPY
   IMAGE-U @ 4 + KEY-PATH-U ! ;

\ Publish the bytes from a new executable named under the root.
: PUBLISH ( ptr u8 n ptr u8 n ptr u8 -- ) {: name:ptr nameu:n a:ptr u:n key:ptr :}
   ROOT$ name nameu SRC FS:JOIN-PATH SRC-U !
   SRC$ a u FS:WRITE-ALL
   SRC$ FS:CHMOD-X
   key FAMILY$ ENGINE-KEY SRC$ KEYED-IMAGE:PUBLISH ;

: IMAGE= ( ptr u8 n -- ) {: a:ptr u:n :}
   IMAGE$ READ-BUF READ-CAP FS:READ-ALL {: got:n :}
   READ-BUF got a u T$= ;

: KEY= ( ptr u8 -- ) {: key:ptr :}
   KEY-PATH$ READ-BUF READ-CAP FS:READ-ALL {: got:n :}
   READ-BUF got key KEYED-IMAGE:KEY-HEX-LEN T$= ;

: CASES ( -- )
   T-RESET
   0 SCRIPT-ARGV$ ROOT!
   [char] a KEY-A KEY!
   [char] b KEY-B KEY!
   [char] e ENGINE-KEY KEY!
   PATHS!
   s" a" s" image A" KEY-A PUBLISH
   s" a same-key publish keeps the image and its key" T-LABEL
   s" b" s" image B" KEY-A PUBLISH
   s" image A" IMAGE=
   KEY-A KEY=
   s" a publish under another key replaces the image and its key" T-LABEL
   s" c" s" image C" KEY-B PUBLISH
   s" image C" IMAGE=
   KEY-B KEY=
   T-REPORT ;

\ BUILD-OUT takes its out root from HABU_OUT or the working directory, so the
\ cases run in a child with HABU_OUT set to a private directory, the artifact.
: RUN ( -- )
   T-RESET
   s" hb-keyed-image-publish" FS:MKTEMP-DIR ROOT!
   ROOT$ s" out" OUT-ROOT FS:JOIN-PATH OUT-ROOT-U !
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/keyed-image-publish.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ROOT$ >LEN PROC-ARGV+
   s" HABU_OUT" >LEN OUT-ROOT$ >LEN PROC-ENV-SET
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-ENV-IO-RC
   MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH
   T-REPORT
   s" keyed-image publish artifact: " type ROOT$ type cr ;

public

: MAIN ( -- )
   SCRIPT-ARGC 0= if RUN exit then
   CASES ;

;package

KEYED-IMAGE-PUBLISH:MAIN
