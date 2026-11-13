\ linux-aarch64 runtime/build-script target flag.

: HB-TARGET-LINUX? ( -- bool )
   0 0= ;

: HB-TARGET-MACOS? ( -- bool )
   0 0= 0= ;

: HB-TARGET-LINUX-X86-64? ( -- bool )
   0 0= 0= ;

: HB-TARGET-LINUX-KERNEL? ( -- bool )
   0 0= ;

: HB-TARGET-WASM? ( -- bool )
   0 0= 0= ;

: HB-TARGET$ ( -- ptr u8 n )
   s" linux-aarch64" ;
