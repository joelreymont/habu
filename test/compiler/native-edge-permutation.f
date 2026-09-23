\ Loop edges transfer their values in parallel, including permuted/shared inputs.
\ Tier 1 first: values transfer in parallel across a block argument, which is
\ the optimizing compiler's loop edge; the sha256 body below is included to be
\ compiled by it.
1 set-tier

require lib/test.f
require lib/string.f

package NATIVE-EDGE-PERMUTATION
private

: SWAP-TURNS ( n n n -- n n ) 0 ?DO swap LOOP ;
: DUPLICATE-TURNS ( n n n -- n n ) 0 ?DO nip dup LOOP ;
: SHARED-TURNS ( n n n -- n n ) 0 ?DO over + swap LOOP ;

: ROTATE-TURNS ( n n n n n n n n n -- n n n n n n n n )
   0 ?DO
      {: a:n b:n c:n d:n e:n f:n g:n h:n :}
      h 1+ a b c d e f g
   LOOP ;

public
: RUN ( -- )
   T-RESET
   91 1 2 0 SWAP-TURNS 2 T= 1 T= 91 T=
   91 1 2 1 SWAP-TURNS 1 T= 2 T= 91 T=
   91 1 2 2 SWAP-TURNS 2 T= 1 T= 91 T=
   91 1 2 3 SWAP-TURNS 1 T= 2 T= 91 T=
   1 2 0 DUPLICATE-TURNS 2 T= 1 T=
   1 2 3 DUPLICATE-TURNS 2 T= 2 T=
   1 2 0 SHARED-TURNS 2 T= 1 T=
   1 2 3 SHARED-TURNS 4 T= 7 T=
   1 2 3 4 5 6 7 8 0 ROTATE-TURNS
   8 T= 7 T= 6 T= 5 T= 4 T= 3 T= 2 T= 1 T=
   1 2 3 4 5 6 7 8 1 ROTATE-TURNS
   7 T= 6 T= 5 T= 4 T= 3 T= 2 T= 1 T= 9 T=
   1 2 3 4 5 6 7 8 8 ROTATE-TURNS
   9 T= 8 T= 7 T= 6 T= 5 T= 4 T= 3 T= 2 T= ;
;package

NATIVE-EDGE-PERMUTATION:RUN

\ Compile the actual prefix source through the active optimizing compiler.
package NATIVE-SHA-EDGE-FIXTURE
public
include src/core/sha256.f
;package

package NATIVE-SHA-EDGE-TEST
private
create DIGEST-BUF 32 allot
create SHA-CTX SHA256-CTX-BYTES allot   \ a context for the freshly compiled copy
create HEX-BUF 64 allot

: HASH= ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n expected:ptr eu:n :}
   SHA-CTX a u DIGEST-BUF NATIVE-SHA-EDGE-FIXTURE:SHA256-IN
   DIGEST-BUF HEX-BUF NATIVE-SHA-EDGE-FIXTURE:SHA256>HEX
   HEX-BUF 64 expected eu T$= ;

public
: RUN ( -- )
   s" "
   s" e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" HASH=
   s" abc"
   s" ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" HASH=
   s" abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
   s" 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1" HASH=
   T-REPORT ;
;package

NATIVE-SHA-EDGE-TEST:RUN
