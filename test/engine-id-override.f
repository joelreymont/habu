\ A relative HABU_UNDER_TEST makes ENGINE-CANDIDATE:PATH$ answer an absolute
\ path to the same file. It runs in process: the override is read from the
\ child-env default table, so no child engine is needed to observe it.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-identity.f
require src/lib/engine-candidate.f

package ENGINE-ID-OVERRIDE
private

create ROOT PATH-CAP allot variable ROOT-U
create ENGINE PATH-CAP allot variable ENGINE-U
create REL PATH-CAP allot variable REL-U
create LOG PATH-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;
: REL$ ( -- ptr u8 n ) REL REL-U @ ;

: SETUP ( -- )
   s" hb-engine-id-override" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" bin" LOG FS:JOIN-PATH {: binu:n :}
   LOG binu FS:MAKE-DIRS
   ROOT$ s" bin/hb" ENGINE FS:JOIN-PATH ENGINE-U !
   ENGINE-CANDIDATE:PATH$ ENGINE$ FS:COPY-FILE-STREAM
   ENGINE$ FS:CHMOD-X ;

\ ENGINE$ from the working directory: one ../ per separator in CWD$ reaches /.
: RELATIVE-ENGINE ( -- )
   SOURCE-ROOT:CWD$ {: cwd:ptr cwdu:n :}
   0 REL-U !
   cwdu 0 ?do
      cwd i + c@ [char] / = if
         REL-U @ 3 + PATH-CAP > if E-FS-CAPACITY throw then
         s" ../" drop REL REL-U @ + 3 BYTE-COPY 3 REL-U +!
      then
   loop
   REL-U @ ENGINE-U @ + 1- PATH-CAP > if E-FS-CAPACITY throw then
   ENGINE 1+ REL REL-U @ + ENGINE-U @ 1- BYTE-COPY ENGINE-U @ 1- REL-U +! ;

: RELATIVE-OVERRIDE ( -- )
   RELATIVE-ENGINE
   s" HABU_UNDER_TEST" >LEN REL$ >LEN PROC-ENV-DEFAULT+
   ENGINE-CANDIDATE:PATH$ {: p:ptr pu:n :}
   PROC-ENV-DEFAULT-RESET
   s" relative override: the answer is absolute" T-LABEL p c@ [char] / = TTRUE
   s" relative override: the answer names the engine" T-LABEL
   p pu ENGINE$ FS:SAMEFILE TTRUE ;

public

: RUN ( -- )
   T-RESET SETUP
   RELATIVE-OVERRIDE
   ROOT$ FS:REMOVE-TREE
   T-REPORT ;

;package

ENGINE-ID-OVERRIDE:RUN
