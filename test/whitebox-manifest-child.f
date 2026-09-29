\ Run by whitebox-engine-key-test.f inside its private invocation root.
require lib/test.f
require lib/fs-mutate.f
require test/whitebox-engine.f

package WHITEBOX-MANIFEST-CHILD

create PATH-A FS-PATH-CAP allot
create PATH-B FS-PATH-CAP allot
create PATH-C FS-PATH-CAP allot
create BOUNDARY-A 32 allot
create BOUNDARY-B 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot
variable PATH-A-U
variable PATH-B-U
variable PATH-C-U

: DEP$ ( -- ptr u8 n ) s" whitebox-manifest-private.f" ;

: A$ ( -- ptr u8 n ) PATH-A PATH-A-U @ ;
: B$ ( -- ptr u8 n ) PATH-B PATH-B-U @ ;
: C$ ( -- ptr u8 n ) PATH-C PATH-C-U @ ;

: KEY! ( ptr u8 ptr n -- ) {: dst:ptr up:ptr :}
   0 SCRIPT-ARGV$ dst up WHITEBOX-ENGINE:ENTRY-PATH! ;

: BOUNDARY-DIGEST! ( ptr u8 -- ) {: dst:ptr :}
   FSHA-CTX 0 SCRIPT-ARGV$ dst SHA256-FILE-IN dup 0 <> if throw then drop ;

: RUN ( -- )
   T-RESET
   0 SCRIPT-ARGV$ DTM:KNOWN? TTRUE
   0 SCRIPT-ARGV$ DISCOVER:RUN
   EVENT-COUNT 1 T=
   BOUNDARY-A BOUNDARY-DIGEST!
   PATH-A PATH-A-U KEY!
   DEP$ s\" \\ changed private dependency\n" APPEND-FILE
   PATH-B PATH-B-U KEY!
   s" editing the private dependency changes the key" T-LABEL
   A$ B$ T$<>
   BOUNDARY-B BOUNDARY-DIGEST!
   s" the boundary file stays byte-identical" T-LABEL
   BOUNDARY-A 32 BOUNDARY-B 32 STR= TTRUE
   DEP$ s\" \\ private dependency\n" WRITE-ALL
   PATH-C PATH-C-U KEY!
   s" restoring the private dependency restores the key" T-LABEL
   A$ C$ T$=
   T-REPORT ;

RUN
;package
