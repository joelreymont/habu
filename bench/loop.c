/* loop.c — xorshift accumulator loop (latency-bound serial chain) for a native
   backend vs clang -O3 comparison.
   Pass iteration count as argv[1] (default 1e8). */
#include <stdint.h>
#include <stdlib.h>
int main(int c,char**v){ long n=c>1?atol(v[1]):100000000; uint64_t h=1;
  for(long i=0;i<n;i++){ h^=h<<13; h^=h>>7; h^=h<<17; } return (int)(h&0xff); }
