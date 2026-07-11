\ vjp.f - the VJP: paired-word table for the M6 forward primitives.
\
\ Decomposes ad-reverse (docs/autograd.md "VJP registration" / "Full VJP table"):
\ each forward primitive registers its adjoint EXPANSION (the token string the
\ reverse pass substitutes) plus its saves count (how many forward values the
\ backward consumes - the tape replacement). lib/ptx/ad.f consumes this table
\ through VJP-ADJOINT$/VJP-SAVES#; the entry text is what AD-REVERSE splices
\ into generated backward bodies. Review-corrected entries: OVER is FAN-OUT
\ (SUM of the copied value's two cotangents via +.), NOT a permutation; DROP
\ pushes a typed zero (ZERO.), never leaks the incoming cotangent. Each entry
\ carries a unit test (src/arch/ptx/vjp-test.f) and a device gradcheck fixture
\ before it is trusted. Load after lib/errors.f and lib/string.f.

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<n> STR>NUMBER? consumer (switchover wave A)

32 constant VJP-MAX
48 constant VJP-NAME-CAP
128 constant VJP-EXP-CAP

create VJP-NAMES VJP-MAX VJP-NAME-CAP * allot
create VJP-NAME-US VJP-MAX cells allot
create VJP-EXPS VJP-MAX VJP-EXP-CAP * allot
create VJP-EXP-US VJP-MAX cells allot
create VJP-SAVE-NS VJP-MAX cells allot
variable VJP-N

: VJP-ID-CHECK ( n -- ) {: id:n :}
   id 0 < if E-PTX-NOVJP throw then
   id VJP-N @ >= if E-PTX-NOVJP throw then ;

: VJP-NAME-BUF ( n -- ptr u8 ) {: id:n :}
   VJP-NAMES id VJP-NAME-CAP * + ;

: VJP-EXP-BUF ( n -- ptr u8 ) {: id:n :}
   VJP-EXPS id VJP-EXP-CAP * + ;

: VJP-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id VJP-ID-CHECK
   id VJP-NAME-BUF  VJP-NAME-US id cells + @ ;

: VJP-EXP$ ( n -- ptr u8 n ) {: id:n :}
   id VJP-ID-CHECK
   id VJP-EXP-BUF  VJP-EXP-US id cells + @ ;

: VJP-SAVE@ ( n -- n ) {: id:n :}
   id VJP-ID-CHECK
   VJP-SAVE-NS id cells + @ ;

: VJP-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}   \ entry id, or -1 when absent
   0 begin dup VJP-N @ < while
      dup VJP-NAME$ a u STR= if exit then
      1+
   repeat drop -1 ;

: VJP-LOOKUP ( ptr u8 n -- n )
   VJP-FIND dup 0 < if E-PTX-NOVJP throw then ;

: VJP-ADJOINT$ ( ptr u8 n -- ptr u8 n )   \ forward name -> adjoint expansion
   VJP-LOOKUP VJP-EXP$ ;

: VJP-SAVES# ( ptr u8 n -- n )   \ forward name -> saved-value count
   VJP-LOOKUP VJP-SAVE@ ;

: VJP-NAME-STORE ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   u VJP-NAME-CAP > if E-PTX-SYNTAX throw then
   a id VJP-NAME-BUF u BYTE-COPY
   u VJP-NAME-US id cells + ! ;

: VJP-EXP-STORE ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   u VJP-EXP-CAP > if E-PTX-SYNTAX throw then
   a id VJP-EXP-BUF u BYTE-COPY
   u VJP-EXP-US id cells + ! ;

\ Register one entry. Duplicate names and a full table are strict errors.
: VJP-REGISTER ( ptr u8 n n ptr u8 n -- )
   {: name:ptr nameu:n saves:n exp:ptr expu:n :}
   name nameu VJP-FIND 0 < 0= if E-PTX-SYNTAX throw then
   saves 0 < if E-PTX-SYNTAX throw then
   VJP-N @ VJP-MAX >= if E-PTX-SYNTAX throw then
   VJP-N @ {: id:n :}
   name nameu id VJP-NAME-STORE
   exp expu id VJP-EXP-STORE
   saves VJP-SAVE-NS id cells + !
   id 1+ VJP-N ! ;

\ ---- the VJP: definer ----------------------------------------------------------
\ VJP: <fwd-name> <saves> ( cotangent effect ) <expansion tokens...> ;
\ The ( ... ) comment documents the adjoint's cotangent stack effect; the
\ expansion tokens are stored single-space joined for the reverse pass.

create VJP-DEF-NAME VJP-NAME-CAP allot
variable VJP-DEF-NAME-U
variable VJP-DEF-SAVES
variable VJP-DEF-DONE

: VJP-TOKEN ( -- ptr u8 n )
   parse-name dup 0= if 2drop E-PTX-SYNTAX throw then ;

: VJP-DEF-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u VJP-NAME-CAP > if E-PTX-SYNTAX throw then
   a VJP-DEF-NAME u BYTE-COPY
   u VJP-DEF-NAME-U ! ;

: VJP-DEF-SAVES! ( ptr u8 n -- )
   STR>NUMBER? MATCH option
     none OF E-PTX-SYNTAX throw ENDOF
     some OF ENDOF
   ;MATCH
   VJP-DEF-SAVES ! ;

: VJP-SKIP-COMMENT ( -- )   \ consume tokens through the closing )
   begin
      VJP-TOKEN s" )" STR=
   until ;

: VJP-EXP-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   SB$ nip 0 > if $20 SB-APPEND-C then
   a u SB-APPEND ;

: VJP-PARSE-EXP ( -- )   \ expansion tokens into SB until ;
   SB-RESET
   0 VJP-DEF-DONE !
   begin VJP-DEF-DONE @ 0= while
      VJP-TOKEN
      2dup s" (" STR= if 2drop VJP-SKIP-COMMENT else
      2dup s" ;" STR= if 2drop -1 VJP-DEF-DONE ! else
         VJP-EXP-TOKEN
      then then
   repeat ;

: VJP: ( -- )
   VJP-TOKEN VJP-DEF-NAME!
   VJP-TOKEN VJP-DEF-SAVES!
   VJP-PARSE-EXP
   VJP-DEF-NAME VJP-DEF-NAME-U @ VJP-DEF-SAVES @ SB$ VJP-REGISTER ;

\ ---- the M6 table (docs/autograd.md "Full VJP table") ---------------------------

VJP: +. 0 ( dz -- dx dy ) DUP ;
VJP: -. 0 ( dz -- dx dy ) DUP NEG ;
VJP: *. 2 ( dz -- dx dy ) DUP SAVED-Y *. SWAP SAVED-X *. ;
VJP: /. 2 ( dz -- dx dy ) DUP SAVED-Y /. SWAP SAVED-Z *. SAVED-Y /. NEG ;
VJP: SCALE 2 ( dz -- dx da ) DUP SAVED-A SCALE SWAP SAVED-X *. BLOCK-SUM ;
VJP: FMA. 2 ( dz -- da dx dy ) DUP DUP SAVED-X *. BLOCK-SUM ROT SAVED-A SCALE ROT ;
VJP: PTX:B- 0 ( dz -- dt ds ) DUP BLOCK-SUM NEG ;
VJP: PTX:B/ 2 ( dz -- dx ds ) DUP SAVED-S PTX:B/ SWAP SAVED-Z *. BLOCK-SUM NEG SAVED-S PTX:U/ ;
VJP: EXP. 1 ( dz -- dx ) SAVED-Y *. ;
VJP: BLOCK-SUM 0 ( ds -- dtile ) BROADCAST ;
VJP: BROADCAST 0 ( dtile -- ds ) BLOCK-SUM ;
VJP: BLOCK-MAX 2 ( ds -- dx ) SAVED-X SAVED-MX BLOCK-MAX-SELECT ;
VJP: NEG 0 ( dz -- dx ) NEG ;
VJP: DUP 0 ( dz1 dz2 -- dz ) +. ;
VJP: OVER 0 ( da1 db da2 -- da db ) ROT +. SWAP ;
VJP: DROP 0 ( -- dt ) ZERO. ;
VJP: SWAP 0 ( db da -- da db ) SWAP ;
VJP: ROT 0 ( db dc da -- da db dc ) ROT ROT ;
VJP: LOAD 0 ( dt -- ) SCATTER-ADD ;
VJP: STORE 0 ( -- dt ) LOAD ;
VJP: LOAD-ONCE 0 ( dt -- ) STORE-ONCE ;
VJP: STORE-ONCE 0 ( -- dt ) LOAD-ONCE ;
VJP: ROW-LOAD 0 ( dt -- ) ROW-SCATTER-ADD ;
VJP: ROW-STORE 0 ( -- dt ) ROW-LOAD ;
VJP: ROW-LOAD-ONCE 0 ( dt -- ) ROW-STORE-ONCE ;
VJP: ROW-STORE-ONCE 0 ( -- dt ) ROW-LOAD-ONCE ;
