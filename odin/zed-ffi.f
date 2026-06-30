\ zed-ffi.f - reach the ZED SDK from checked Habu through ffi-call-n.
\
\ Demonstrates the SDK-bound core is reachable: dlopen the real ZED SDK shared
\ library, resolve the extern-"C" entry getZEDSDKRuntimeVersion_C, and call it via
\ lib/ffi.f FFI-CALLN (ffi-call-n). The function is
\   int getZEDSDKRuntimeVersion_C(int* major, int* minor, int* patch)
\ - non-invasive (no camera/driver state), so it is safe to run on the ZED Box.
\ Output should match the installed SDK (5.2.3). This is the FFI path the capture/
\ detector core would use through a thin C-ABI wrapper.
\ Load: bin/hb --load src/os/linux/layout.f lib/errors.f lib/string.f lib/memory.f lib/ffi.f odin/zed-ffi.f

create ZED-PATH 64 allot                 \ NUL-terminated .so path
create ZED-SYM  48 allot                 \ NUL-terminated symbol name
create ZED-VBUF 3 cells allot            \ out: major / minor / patch (int written low 4 bytes)
variable ZED-H   variable ZED-FN   variable ZED-RC

: ZED-SDK-VERSION ( -- )
   s" /usr/local/zed/lib/libsl_zed.so" ZED-PATH >CSTR
   ZED-PATH RTLD-NOW DLOPEN ZED-H !
   ZED-H @ 0 = if ." zed: dlopen failed" cr exit then
   s" getZEDSDKRuntimeVersion_C" ZED-SYM >CSTR
   ZED-H @ ZED-SYM DLSYM ZED-FN !
   ZED-FN @ 0 = if ." zed: dlsym failed" cr exit then
   0 ZED-VBUF 0 cells + !  0 ZED-VBUF 1 cells + !  0 ZED-VBUF 2 cells + !
   ZED-VBUF 0 cells + P>N 0 FFI-ARG!
   ZED-VBUF 1 cells + P>N 1 FFI-ARG!
   ZED-VBUF 2 cells + P>N 2 FFI-ARG!
   3 ZED-FN @ FFI-CALLN ZED-RC !
   ." ZED SDK runtime version: "
   ZED-VBUF 0 cells + @ .  ." ."  ZED-VBUF 1 cells + @ .  ." ."  ZED-VBUF 2 cells + @ .  cr
   ." (call rc=" ZED-RC @ . ." )" cr ;

ZED-SDK-VERSION
