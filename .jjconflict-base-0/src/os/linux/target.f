\ linux-aarch64 runtime/build-script target flag.

: HB-TARGET-LINUX? ( -- bool )
   0 0= ;

: HB-TARGET-MACOS? ( -- bool )
   0 0= 0= ;

: HB-TARGET-KNOWN? ( -- bool )
   HB-TARGET-LINUX? HB-TARGET-MACOS? or ;
