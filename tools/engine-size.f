\ engine-size.f - the command line over tools/image-size-lib.f.
\
\ Run: <engine> --load tools/engine-size.f -- <image>
\
\ The image may be a baked engine, a --repl snapshot application or a stripped
\ application; the tool reads which one it is out of the file. The measurement
\ lives in the library because tools/hb-build-lib.f requires it too, to size
\ what a build has just written, and a file that measures argv[0] the moment it
\ loads cannot be required by a program that has arguments of its own.
\ docs/engine-size.md is this tool's document.

require tools/image-size-lib.f

\ Loading with no argument defines the tool without measuring anything, so a
\ test can drive it; tools/imgdump.f has the same entry shape.
: ENGINE-SIZE-MAIN? ( -- )
   SCRIPT-ARGC 0 > if IMAGE-SIZE:RUN then ;

ENGINE-SIZE-MAIN?
undefine ENGINE-SIZE-MAIN?
