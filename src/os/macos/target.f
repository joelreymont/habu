\ macos-aarch64 runtime/build-script target flag and the OS's path capacity.

: HB-TARGET-LINUX? ( -- bool )
   0 0= 0= ;

: HB-TARGET-MACOS? ( -- bool )
   0 0= ;

: HB-TARGET-LINUX-X86-64? ( -- bool )
   0 0= 0= ;

: HB-TARGET-LINUX-KERNEL? ( -- bool )
   0 0= 0= ;

: HB-TARGET-WASM? ( -- bool )
   0 0= 0= ;

: HB-TARGET$ ( -- ptr u8 n )
   s" macos-aarch64" ;

\ The OS's PATH_MAX (sys/syslimits.h, 1024) less its NUL.
1023 constant PATH-CAP
