/* dispatch.c — the dispatch-bound byte-mix as C, for an LLVM (clang -O3)
   baseline. Same algorithm/seed as dispatch.s. This is the bar a native Forth
   backend must rival. */
#include <stdint.h>
static uint8_t buf[65536];
int main(void){
    for (int i=0;i<65536;i++) buf[i]=i&0xff;
    uint64_t h=1;
    for (int p=0;p<15000;p++)
        for (int i=0;i<65536;i++){
            h += buf[i];
            h ^= h<<13; h ^= h>>7; h ^= h<<17;
        }
    return (int)(h & 0xff);
}
