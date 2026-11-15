\ wasm runtime/build-script target flag and the path capacity.

: HB-TARGET-LINUX? ( -- bool )
   0 0= 0= ;

: HB-TARGET-MACOS? ( -- bool )
   0 0= 0= ;

: HB-TARGET-LINUX-X86-64? ( -- bool )
   0 0= 0= ;

: HB-TARGET-LINUX-KERNEL? ( -- bool )
   0 0= 0= ;

: HB-TARGET-WASM? ( -- bool )
   0 0= ;

: HB-TARGET$ ( -- ptr u8 n )
   s" wasm" ;

\ Wasm has no OS path limit: it takes Linux's, PATH_MAX less its NUL.
4095 constant PATH-CAP
