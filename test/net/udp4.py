#!/usr/bin/env python3
"""Exercise native Habu IPv4 UDP against independent Python sockets."""

import argparse
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import socket
import subprocess


ROOT = Path(__file__).resolve().parents[2]
SETUP = '''
require lib/net/udp4.f
package UDP4-TEST
using UDP4
CAST: BLEN>N ( NUM:byte-len -- n )
create BODY 65509 allot
: OK ( UDP4:status -- )
   MATCH UDP4:status
      ok OF ENDOF
      failed OF ERRNO>N throw ENDOF
   ;MATCH ;
: OPEN ( -- socket )
   $7F000001 UDP4:ADDRESS 0 UDP4:PORT UDP4:BIND MATCH UDP4:open-result
      opened OF ENDOF
      failed OF ERRNO>N throw ENDOF
   ;MATCH ;
: REPORT ( NUM:byte-len address port n -- )
   {: size:NUM:byte-len address:address port:port capacity:n :}
   size BLEN>N . address ADDRESS>N . port PORT>N .
   0 size BLEN>N capacity min 0 ?do BODY i + c@ + loop .
   BODY capacity + c@ . BODY capacity 1 + + c@ . ;
: RECEIVE-ONE ( socket n -- ) {: socket:socket capacity:n :}
   $A5 BODY capacity + c! $5A BODY capacity 1 + + c!
   socket BODY capacity PAYLOAD-BYTES 2000 >MS UDP4:RECEIVE
   MATCH UDP4:receive-result
      packet OF s" packet " type capacity REPORT ENDOF
      truncated OF s" truncated " type capacity REPORT ENDOF
      timeout OF s" timeout " type ENDOF
      failed OF s" error " type ERRNO>N . ENDOF
   ;MATCH ;
: NO-PACKET ( socket ms -- ) {: socket:socket timeout:ms :}
   socket BODY 64 PAYLOAD-BYTES timeout UDP4:RECEIVE MATCH UDP4:receive-result
      packet OF drop drop drop -1 throw ENDOF
      truncated OF drop drop drop -1 throw ENDOF
      timeout OF s" timeout " type ENDOF
      failed OF ERRNO>N throw ENDOF
   ;MATCH ;
'''


def check_process(result, expected=0):
    assert result.returncode == expected, (result.stdout, result.stderr)
    return result.stdout.split()


def native(habu, output, body, *, refused=False):
    path = output / 'case.f'
    path.write_text(SETUP + body + '\n;package\n')
    result = subprocess.run(
        [str(habu), '--load', str(path)], cwd=ROOT,
        input='', text=True, capture_output=True, timeout=15,
    )
    values = check_process(result, 70 if refused else 0)
    if refused:
        assert 'non-certified' in result.stderr, result.stderr
    return values


def exchange(habu, output):
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
        peer.bind(('127.0.0.1', 0))
        peer.settimeout(5)
        port = peer.getsockname()[1]
        path = output / 'exchange.f'
        path.write_text(SETUP + f'''
: RUN ( -- )
   OPEN {{: socket:socket :}}
   socket UDP4:LOCAL MATCH UDP4:endpoint-result
      endpoint OF PORT>N . ADDRESS>N . ENDOF
      failed OF ERRNO>N throw ENDOF
   ;MATCH
   $7F000001 UDP4:ADDRESS {port} UDP4:PORT UDP4:BIND MATCH UDP4:open-result
      opened OF UDP4:CLOSE OK -1 throw ENDOF
      failed OF ERRNO>N 98 <> if -1 throw then ENDOF
   ;MATCH
   socket $7F000001 UDP4:ADDRESS {port} UDP4:PORT s" first" PAYLOAD-BYTES UDP4:SEND OK
   socket 64 RECEIVE-ONE
   socket 8 RECEIVE-ONE
   socket 64 RECEIVE-ONE
   socket 65507 RECEIVE-ONE
   socket 20 >MS NO-PACKET
   socket 0 >MS NO-PACKET
   socket $7F000001 UDP4:ADDRESS {port} UDP4:PORT BODY 65507 PAYLOAD-BYTES UDP4:SEND OK
   socket $7F000001 UDP4:ADDRESS {port} UDP4:PORT BODY 0 PAYLOAD-BYTES UDP4:SEND OK
   socket UDP4:CLOSE OK
   socket BODY 1 PAYLOAD-BYTES 0 >MS UDP4:RECEIVE MATCH UDP4:receive-result
      packet OF drop drop drop -1 throw ENDOF
      truncated OF drop drop drop -1 throw ENDOF
      timeout OF -1 throw ENDOF
      failed OF ERRNO>N . ENDOF
   ;MATCH
   socket UDP4:LOCAL MATCH UDP4:endpoint-result
      endpoint OF drop drop -1 throw ENDOF
      failed OF ERRNO>N . ENDOF
   ;MATCH
   socket $7F000001 UDP4:ADDRESS {port} UDP4:PORT BODY 1 PAYLOAD-BYTES UDP4:SEND MATCH UDP4:status
      ok OF -1 throw ENDOF
      failed OF ERRNO>N . ENDOF
   ;MATCH
   socket UDP4:CLOSE MATCH UDP4:status
      ok OF -1 throw ENDOF
      failed OF ERRNO>N . ENDOF
   ;MATCH ;
RUN
;package
''')
        packets = [b'hello\0UDP', bytes(range(64)), b'', b'Z' * 65507]

        def serve(pid):
            data, remote = peer.recvfrom(65535)
            assert data == b'first', data
            assert remote[0] == '127.0.0.1', remote
            # Inspect the actual socket descriptor while Habu waits for data.
            flags = []
            for descriptor in (Path('/proc') / str(pid) / 'fd').iterdir():
                if descriptor.readlink().as_posix().startswith('socket:'):
                    info = (descriptor.parent.parent / 'fdinfo' / descriptor.name).read_text()
                    flags.extend(int(line.split()[1], 8) for line in info.splitlines() if line.startswith('flags:'))
            assert len(flags) == 1, flags
            assert flags[0] & 0o4000 and flags[0] & 0o2000000, flags
            for data in packets:
                peer.sendto(data, remote)
            data, sender = peer.recvfrom(65535)
            assert data == packets[-1] and sender == remote, (len(data), sender)
            data, sender = peer.recvfrom(65535)
            assert data == b'' and sender == remote, (data, sender)
            return remote

        with subprocess.Popen(
            [str(habu), '--load', str(path)], cwd=ROOT,
            stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
        ) as process, ThreadPoolExecutor(max_workers=1) as pool:
            future = pool.submit(serve, process.pid)
            try:
                stdout, stderr = process.communicate(timeout=15)
            except subprocess.TimeoutExpired:
                process.kill()
                process.communicate()
                raise
            assert process.returncode == 0, (stdout, stderr)
            remote = future.result(timeout=6)

        expected = [str(remote[1]), str(0x7F000001)]
        for payload, capacity in zip(packets, (64, 8, 64, 65507)):
            expected += ['truncated' if len(payload) > capacity else 'packet',
                         str(len(payload)), str(0x7F000001), str(port),
                         str(sum(payload[:capacity])), '165', '90']
        expected += ['timeout', 'timeout', '9', '9', '9', '9']  # Linux EBADF after close.
        assert stdout.split() == expected, (stdout.split(), expected)

        # A failed bind reports the original errno after closing its new socket.
        assert native(habu, output, f'''
: RUN ( -- )
   $7F000001 UDP4:ADDRESS {port} UDP4:PORT UDP4:BIND MATCH UDP4:open-result
      opened OF UDP4:CLOSE OK -1 throw ENDOF
      failed OF ERRNO>N . ENDOF
   ;MATCH ; RUN
''') == ['98']  # Linux EADDRINUSE.


def concurrent(habu, output):
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as first, socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as second:
        peers = (first, second)
        for peer in peers:
            peer.bind(('127.0.0.1', 0))
            peer.settimeout(5)
        ports = [peer.getsockname()[1] for peer in peers]
        lengths = (3, 11)
        identities = (0xA0, 0xB1)

        def serve():
            remotes = [None, None]
            for _ in range(32):
                for i, peer in enumerate(peers):
                    data, remote = peer.recvfrom(64)
                    assert data == bytes([identities[i]]), data
                    if remotes[i] is None:
                        remotes[i] = remote
                    assert remote == remotes[i]
                assert remotes[0] != remotes[1], remotes
                for i in (1, 0):
                    peers[i].sendto(bytes([identities[i]]) * lengths[i], remotes[i])

        source = f'''
here FFI:>CELL 7 and 8 swap - 7 and allot
variable ENTERED
variable DONE
create BODY0 16 allot
create BODY1 16 allot
TASK:MIN-STACK TASK:TASK WORKER0
TASK:MIN-STACK TASK:TASK WORKER1
: WORK ( ptr u8 n n n -- ) {{: bytes identity:n expected:n peer:n :}}
   1 ENTERED atomic-add drop
   begin ENTERED atomic@ 2 < while TASK:PAUSE repeat
   OPEN {{: socket:socket :}}
   identity bytes c!
   32 0 do
      socket $7F000001 UDP4:ADDRESS peer UDP4:PORT bytes 1 PAYLOAD-BYTES UDP4:SEND OK
      socket bytes expected PAYLOAD-BYTES 2000 >MS UDP4:RECEIVE
      MATCH UDP4:receive-result
         packet OF
            {{: size:NUM:byte-len address:address port:port :}}
            size BLEN>N expected <> if -1 throw then
            address ADDRESS>N $7F000001 <> if -1 throw then
            port PORT>N peer <> if -1 throw then
            expected 0 do bytes i + c@ identity <> if -1 throw then loop
         ENDOF
         truncated OF drop drop drop -1 throw ENDOF
         timeout OF -1 throw ENDOF
         failed OF ERRNO>N throw ENDOF
      ;MATCH
   loop
   socket UDP4:CLOSE OK
   1 DONE atomic-add drop ;
: WORK0 ( -- ) BODY0 160 3 {ports[0]} WORK ;
: WORK1 ( -- ) BODY1 177 11 {ports[1]} WORK ;
: WAIT-DONE ( ptr n -- ) {{: worker:ptr :}}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat ;
: RUN ( -- )
   ['] WORK0 WORKER0 TASK:ACTIVATE
   ['] WORK1 WORKER1 TASK:ACTIVATE
   WORKER0 WAIT-DONE WORKER1 WAIT-DONE
   WORKER0 TASK:KILL WORKER1 TASK:KILL
   DONE atomic@ . ;
RUN
'''
        with ThreadPoolExecutor(max_workers=1) as pool:
            future = pool.submit(serve)
            assert native(habu, output, source) == ['2']
            future.result(timeout=6)


def address_strings(habu, output):
    checks = 0
    for address, expected in [('0.0.0.0', 0), ('255.255.255.255', 0xFFFFFFFF),
                              ('127.0.0.1', 0x7F000001), ('192.0.2.37', 0xC0000225)]:
        assert native(habu, output, f': RUN ( -- ) s" {address}" UDP4:ADDRESS$ ADDRESS>N . ; RUN') == [str(expected)]
        checks += 1
    for address in ('', '127.1', '1.2.3.4.5', '.1.2.3', '1..2.3', '1.2.3.',
                    '1.2.3.256', '1.2.3.-1', '+1.2.3.4', '01.2.3.4',
                    '1.2.3.04', ' 1.2.3.4', '1.2.3.4 ', '1.2.3.a', '1000.2.3.4'):
        assert native(habu, output, f': RUN ( -- ) [: s" {address}" UDP4:ADDRESS$ drop ;] catch . ; RUN') == ['-9100']
        checks += 1
    return checks


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--habu', type=Path, default=ROOT / 'bin/hb')
    args = parser.parse_args()
    habu = args.habu.resolve()
    output = ROOT / 'tmp/test-udp4'
    output.mkdir(parents=True, exist_ok=True)
    exchange(habu, output)
    concurrent(habu, output)
    checks = 3 + address_strings(habu, output)
    for expression in ('-1 UDP4:ADDRESS', '$100000000 UDP4:ADDRESS',
                       '-1 UDP4:PORT', '65536 UDP4:PORT',
                       '-1 PAYLOAD-BYTES', '65508 PAYLOAD-BYTES'):
        assert native(habu, output, f': RUN ( -- ) [: {expression} drop ;] catch . ; RUN') == ['-9100']
        checks += 1
    for capacity, timeout in ((0, 0), (65508, 0), (1, -1), (1, 0x80000000)):
        assert native(habu, output, f'''
: RUN ( -- )
   [: 0 >SOCKET BODY {capacity} NUM:BYTE-LEN MATCH NUM:numeric-result
      ok OF ENDOF negative OF -1 throw ENDOF zero OF -1 throw ENDOF
      overflow OF -1 throw ENDOF underflow OF -1 throw ENDOF
      bad-alignment OF -1 throw ENDOF misaligned OF -1 throw ENDOF
   ;MATCH {timeout} >MS UDP4:RECEIVE MATCH UDP4:receive-result
      packet OF drop drop drop ENDOF truncated OF drop drop drop ENDOF
      timeout OF ENDOF failed OF drop ENDOF
   ;MATCH ;] catch . ; RUN
''') == ['-9100']
        checks += 1
    assert native(habu, output, ': RUN ( -- ) 0 PAYLOAD-BYTES BLEN>N . 65507 PAYLOAD-BYTES BLEN>N . ; RUN') == ['0', '65507']
    checks += 1
    for signature in ('n -- UDP4:socket', 'UDP4:address -- UDP4:port',
                      'UDP4:socket -- UDP4:errno'):
        native(habu, output, f': BAD ( {signature} ) ;', refused=True)
        checks += 1
    print(f'udp4: {checks} native cases passed, including independent localhost exchange')


if __name__ == '__main__':
    main()
