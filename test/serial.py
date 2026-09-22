#!/usr/bin/env python3
"""Exercise native Habu serial streams against Linux kernel pseudoterminals."""

import argparse
from concurrent.futures import ThreadPoolExecutor
import fcntl
import os
from pathlib import Path
import pty
import select
import struct
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]
SETUP = '''
require lib/aio.f
require lib/serial.f
package SERIAL-TEST
CAST: BLEN>N ( NUM:byte-len -- n )
create BODY 1048578 allot
: OK ( SERIAL:status -- )
   MATCH SERIAL:status
      ok OF ENDOF
      failed OF SERIAL:ERRNO>N throw ENDOF
   ;MATCH ;


: OPEN ( ptr u8 n -- SERIAL:handle )
   115200 SERIAL:BAUD SERIAL:OPEN8N1 MATCH SERIAL:open-result
      opened OF ENDOF
      failed OF SERIAL:ERRNO>N throw ENDOF
      unsupported OF -1 throw ENDOF
   ;MATCH ;


: MOVED ( SERIAL:io-result -- n )
   MATCH SERIAL:io-result
      transferred OF BLEN>N ENDOF
      timeout OF -2 throw ENDOF
      closed OF -3 throw ENDOF
      failed OF SERIAL:ERRNO>N throw ENDOF
   ;MATCH ;


: EXACT-READ ( SERIAL:handle ptr u8 n -- ) {: handle:SERIAL:handle bytes size:n :}
   0 begin dup size < while
      dup {: offset:n :}
      handle bytes offset + size offset - SERIAL:BYTES 2000 >MS SERIAL:READ MOVED +
   repeat drop ;


: READY ( SERIAL:handle -- )
   s" ready" SERIAL:BYTES 1000 >MS SERIAL:WRITE MOVED 5 <> if -1 throw then ;


: REPORT ( SERIAL:io-result -- )
   MATCH SERIAL:io-result
      transferred OF s" moved " type BLEN>N . ENDOF
      timeout OF s" timeout " type ENDOF
      closed OF s" closed " type ENDOF
      failed OF s" error " type SERIAL:ERRNO>N . ENDOF
   ;MATCH ;


'''


def source(output, body):
    path = output / 'case.f'
    path.write_text(SETUP + body + '\n;package\n')
    return path


def native(habu, output, body, refused=False):
    result = subprocess.run([str(habu), '--load', str(source(output, body))],
                            cwd=ROOT, stdin=subprocess.DEVNULL, text=True,
                            capture_output=True, timeout=15)
    assert result.returncode == (70 if refused else 0), (result.stdout, result.stderr)
    if refused:
        assert 'non-certified' in result.stderr, result.stderr
    return result.stdout.split()


def read_exact(fd, size, timeout=5):
    result = b''
    end = time.monotonic() + timeout
    while len(result) < size:
        assert select.select([fd], [], [], max(0, end-time.monotonic()))[0], len(result)
        block = os.read(fd, size-len(result))
        assert block
        result += block
    return result


def exchange(habu, output, body, peer):
    master, slave = pty.openpty()
    try:
        path = os.ttyname(slave)
        body = body.replace('DEVICE', path)
        with subprocess.Popen([str(habu), '--load', str(source(output, body))],
                              cwd=ROOT, stdin=subprocess.DEVNULL, text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE) as process:
            with ThreadPoolExecutor(max_workers=1) as pool:
                future = pool.submit(peer, master, slave, process.pid)
                try:
                    stdout, stderr = process.communicate(timeout=15)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.communicate()
                    raise
                assert process.returncode == 0, (stdout, stderr)
                result = future.result(timeout=6)
        return stdout.split(), result
    finally:
        os.close(slave)
        os.close(master)


def check_abi(output):
    path = output / 'abi.c'
    path.write_text('''
#include <asm/termbits.h>
#include <sys/ioctl.h>
#include <stddef.h>
#include <fcntl.h>
#include <stdio.h>
int main(void) {
 printf("%zu %zu %zu %zu %zu %zu %zu %zu %zu %lx %lx %x %x %x %x\\n",
 sizeof(struct termios2), offsetof(struct termios2,c_iflag),offsetof(struct termios2,c_oflag),
 offsetof(struct termios2,c_cflag),offsetof(struct termios2,c_lflag),offsetof(struct termios2,c_line),
 offsetof(struct termios2,c_cc),offsetof(struct termios2,c_ispeed),offsetof(struct termios2,c_ospeed),
 (unsigned long)TCGETS2,(unsigned long)TCSETS2,O_RDWR|O_NONBLOCK|O_NOCTTY|O_CLOEXEC,
 BOTHER,CLOCAL|CREAD|CS8,CBAUD|CIBAUD);
}
''')
    binary = output / 'abi'
    subprocess.run(['cc', '-Wall', '-Wextra', '-Werror', str(path), '-o', str(binary)], check=True)
    assert subprocess.check_output([str(binary)], text=True).split() == [
        '44', '0', '4', '8', '12', '16', '17', '36', '40', '802c542a', '402c542b',
        '80902', '1000', '8b0', '100f100f']


def binary_io(habu, output):
    body = '''
: RUN ( -- )
   AIO:START
   s" /dev/null" 115200 SERIAL:BAUD SERIAL:OPEN8N1 MATCH SERIAL:open-result
      opened OF SERIAL:CLOSE OK -1 throw ENDOF
      failed OF SERIAL:ERRNO>N 25 <> if -1 throw then ENDOF
      unsupported OF -1 throw ENDOF
   ;MATCH
   s" DEVICE" OPEN {: handle:SERIAL:handle :}
   handle READY
   $A5 BODY 256 + c! $5A BODY 257 + c!
   handle BODY 256 EXACT-READ
   256 0 do BODY i + c@ i <> if -1 throw then loop
   BODY 256 + c@ . BODY 257 + c@ .
   handle BODY 256 SERIAL:BYTES 1000 >MS SERIAL:WRITE MOVED .
   handle BODY 7 SERIAL:BYTES 2000 >MS SERIAL:READ MOVED .
   handle BODY 57 EXACT-READ
   57 0 do BODY i + c@ i 7 + <> if -1 throw then loop
   mono-ns {: started:n :}
   handle BODY 1 SERIAL:BYTES 40 >MS SERIAL:READ REPORT
   mono-ns started - 1000000 / .
   handle BODY 1 SERIAL:BYTES 0 >MS SERIAL:READ REPORT
   handle SERIAL:CLOSE OK
   handle BODY 1 SERIAL:BYTES 0 >MS SERIAL:READ REPORT
   handle BODY 1 SERIAL:BYTES 0 >MS SERIAL:WRITE REPORT
   handle SERIAL:CLOSE MATCH SERIAL:status
      ok OF -1 throw ENDOF failed OF SERIAL:ERRNO>N . ENDOF
   ;MATCH
   AIO:STOP ;
RUN
'''

    def peer(master, slave, pid):
        assert read_exact(master, 5) == b'ready'
        attributes = bytearray(44)
        fcntl.ioctl(slave, 0x802c542a, attributes)
        inf, outf, control, local, line, chars, ibaud, obaud = struct.unpack('=IIIIB19sII', attributes)
        assert (inf, outf, local, line, chars[5], chars[6], ibaud, obaud) == (0, 0, 0, 0, 0, 1, 115200, 115200)
        assert control & ~(0x100F100F | 0x400) == 0x8B0, hex(control)
        devices = []
        for fd in (Path('/proc') / str(pid) / 'fd').iterdir():
            target = fd.readlink().as_posix()
            if target.startswith('/dev/pts/') or target == '/dev/null':
                devices.append((fd.name, target))
                if target.startswith('/dev/pts/'):
                    info = (fd.parent.parent/'fdinfo'/fd.name).read_text()
                    flags = next(int(x.split()[1], 8) for x in info.splitlines() if x.startswith('flags:'))
                    assert flags & os.O_NONBLOCK and flags & os.O_CLOEXEC, flags
        # stdin is /dev/null; the unsuccessful terminal open leaked no fd.
        assert sorted(target for _, target in devices) == ['/dev/null', os.ttyname(slave)], devices
        os.write(master, bytes(range(256)))
        assert read_exact(master, 256) == bytes(range(256))
        os.write(master, bytes(range(64)))

    values, _ = exchange(habu, output, body, peer)
    assert values[:5] == ['165', '90', '256', '7', 'timeout'], values
    assert 30 <= int(values[5]) < 500, values
    assert values[6:] == ['timeout', 'error', '9', 'error', '9', '9'], values


def partial_write(habu, output):
    body = '''
: RUN ( -- )
   AIO:START
   1048576 0 do i $FF and BODY i + c! loop
   s" DEVICE" OPEN {: handle:SERIAL:handle :}
   handle READY
   handle BODY 1 EXACT-READ
   handle BODY 1048576 SERIAL:BYTES 0 >MS SERIAL:WRITE MOVED .
   0 begin
      handle BODY 1048576 SERIAL:BYTES 20 >MS SERIAL:WRITE MATCH SERIAL:io-result
         transferred OF BLEN>N + false ENDOF
         timeout OF true ENDOF
         closed OF -1 throw ENDOF
         failed OF SERIAL:ERRNO>N throw ENDOF
      ;MATCH
   until
   . handle SERIAL:CLOSE OK
   AIO:STOP ;
RUN
'''
    def peer(master, slave, pid):
        assert read_exact(master, 5) == b'ready'
        os.write(master, b'\0')
        # Keep the peer open without consuming its finite kernel queue.
    values, _ = exchange(habu, output, body, peer)
    assert 0 < int(values[0]) < 1048576, values
    assert 0 <= int(values[1]) < 1048576, values


def disconnect(habu, output):
    body = '''
: RUN ( -- )
   AIO:START
   s" DEVICE" OPEN {: handle:SERIAL:handle :}
   handle READY
   handle BODY 1 SERIAL:BYTES 2000 >MS SERIAL:READ REPORT
   handle BODY 1 SERIAL:BYTES 0 >MS SERIAL:WRITE REPORT
   handle SERIAL:CLOSE OK
   AIO:STOP ;
RUN
'''
    # Close the master while Habu waits, to exercise real terminal hangup.
    master, slave = pty.openpty()
    try:
        body = body.replace('DEVICE', os.ttyname(slave))
        with subprocess.Popen([str(habu), '--load', str(source(output, body))],
                              cwd=ROOT, stdin=subprocess.DEVNULL, text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE) as process:
            assert read_exact(master, 5) == b'ready'
            os.close(master)
            master = None
            stdout, stderr = process.communicate(timeout=10)
            assert process.returncode == 0, (stdout, stderr)
        assert stdout.split() == ['closed', 'error', '5'], stdout
    finally:
        if master is not None:
            os.close(master)
        os.close(slave)


def concurrent(habu, output):
    terminals = [pty.openpty(), pty.openpty()]
    try:
        paths = [os.ttyname(slave) for _, slave in terminals]
        speeds = [115200, 123457]
        body = f'''
here FFI:>CELL 7 and 8 swap - 7 and allot
variable ENTERED
variable DONE
create BODY0 16 allot
create BODY1 16 allot
TASK:MIN-STACK TASK:TASK WORKER0
TASK:MIN-STACK TASK:TASK WORKER1
: WORK ( ptr u8 n n ptr u8 n n -- )
   {{: path size:n speed:n bytes identity:n expected:n :}}
   1 ENTERED atomic-add drop
   begin ENTERED atomic@ 2 < while TASK:PAUSE repeat
   path size speed SERIAL:BAUD SERIAL:OPEN8N1 MATCH SERIAL:open-result
      opened OF ENDOF
      failed OF SERIAL:ERRNO>N throw ENDOF
      unsupported OF -1 throw ENDOF
   ;MATCH {{: handle:SERIAL:handle :}}
   identity bytes c!
   16 0 do
      handle bytes 1 SERIAL:BYTES 1000 >MS SERIAL:WRITE MOVED 1 <> if -1 throw then
      handle bytes expected EXACT-READ
      expected 0 do bytes i + c@ identity <> if -1 throw then loop
   loop
   handle SERIAL:CLOSE OK
   1 DONE atomic-add drop ;


: WORK0 ( -- ) s" {paths[0]}" {speeds[0]} BODY0 160 3 WORK ;
: WORK1 ( -- ) s" {paths[1]}" {speeds[1]} BODY1 177 11 WORK ;


: WAIT-DONE ( ptr n -- ) {{: worker:ptr :}}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat ;


: RUN ( -- )
   AIO:START
   ['] WORK0 WORKER0 TASK:ACTIVATE
   ['] WORK1 WORKER1 TASK:ACTIVATE
   WORKER0 WAIT-DONE WORKER1 WAIT-DONE
   WORKER0 TASK:KILL WORKER1 TASK:KILL
   DONE atomic@ .
   AIO:STOP ;
RUN
'''
        def serve():
            for _ in range(16):
                for (master, slave), identity, speed in zip(terminals, (160, 177), speeds):
                    assert read_exact(master, 1) == bytes([identity])
                    attributes = bytearray(44)
                    fcntl.ioctl(slave, 0x802c542a, attributes)
                    assert struct.unpack_from('=II', attributes, 36) == (speed, speed)
                for (master, _), identity, size in reversed(list(zip(terminals, (160, 177), (3, 11)))):
                    os.write(master, bytes([identity]) * size)
        with ThreadPoolExecutor(max_workers=1) as pool:
            future = pool.submit(serve)
            assert native(habu, output, body) == ['2']
            future.result(timeout=6)
    finally:
        for master, slave in terminals:
            os.close(master)
            os.close(slave)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--habu', type=Path, default=ROOT / 'bin/hb')
    args = parser.parse_args()
    habu = args.habu.resolve()
    output = ROOT / 'tmp/test-serial'
    output.mkdir(parents=True, exist_ok=True)
    check_abi(output)
    binary_io(habu, output)
    partial_write(habu, output)
    disconnect(habu, output)
    concurrent(habu, output)
    checks = 5
    for expr in ('0 SERIAL:BAUD', '-1 SERIAL:BAUD', '$100000000 SERIAL:BAUD',
                 '0 SERIAL:BYTES', '-1 SERIAL:BYTES', '$7FFFF001 SERIAL:BYTES'):
        assert native(habu, output, f': RUN ( -- ) [: {expr} drop ;] catch . ; RUN') == ['-9110']
        checks += 1
    for length, timeout, handle in ((0, 0, 0), (0x7FFFF001, 0, 0), (1, -1, 0), (1, 0x80000000, 0),
                                     (1, 0, -1), (1, 0, 0x80000000)):
        for operation in ('READ', 'WRITE'):
            body = f'''
: RUN ( -- )
   [: {handle} SERIAL:>HANDLE BODY {length} NUM:BYTE-LEN MATCH NUM:numeric-result
      ok OF ENDOF negative OF -1 throw ENDOF zero OF -1 throw ENDOF
      overflow OF -1 throw ENDOF underflow OF -1 throw ENDOF
      bad-alignment OF -1 throw ENDOF misaligned OF -1 throw ENDOF
   ;MATCH {timeout} >MS SERIAL:{operation} REPORT ;] catch . ; RUN
'''
            assert native(habu, output, body) == ['-9110']
            checks += 1
    for path, expected in (('/no/such/habu-serial-port', '2'), ('/dev/null', '25')):
        body = f'''
: RUN ( -- )
 s" {path}" 115200 SERIAL:BAUD SERIAL:OPEN8N1 MATCH SERIAL:open-result
 opened OF SERIAL:CLOSE OK -1 throw ENDOF
 failed OF SERIAL:ERRNO>N . ENDOF unsupported OF -1 throw ENDOF ;MATCH ; RUN
'''
        assert native(habu, output, body) == [expected]
        checks += 1
    for text, size in (('s" "', None), ('BODY 4096', None), ('BODY 3', 3)):
        prep = '0 BODY 1 + c!' if size else ''
        body = f'''
: RUN ( -- )
   {prep} [: {text} 115200 SERIAL:BAUD SERIAL:OPEN8N1 MATCH SERIAL:open-result
      opened OF SERIAL:CLOSE OK ENDOF failed OF drop ENDOF unsupported OF ENDOF
   ;MATCH ;] catch . ; RUN
'''
        assert native(habu, output, body) == ['-9110']
        checks += 1
    for signature in ('n -- SERIAL:handle', 'SERIAL:baud -- SERIAL:handle', 'SERIAL:handle -- SERIAL:errno'):
        native(habu, output, f': BAD ( {signature} ) ;', refused=True)
        checks += 1
    print(f'serial: {checks} native/ABI cases passed, including kernel pseudoterminal I/O')


if __name__ == '__main__':
    main()
