#!/usr/bin/env python3
"""Exercise native serial XMODEM against an independent pseudoterminal peer."""

import errno
import os
from pathlib import Path
import select
import subprocess
import time

from xmodem import packet


ROOT = Path(__file__).resolve().parents[1]
SETUP = '''
require lib/aio.f
require lib/fs.f
package SXMODEM-TEST
using XMODEM
using BUF
create INPUT HDR-BYTES allot
create OUTPUT HDR-BYTES allot
create SESSION SERIAL-XMODEM:SESSION-BYTES allot
CAST: BLEN>N ( NUM:byte-len -- n )

: INPUT-READ ( ptr u8 n n -- ) {: path size:n capacity:n :}
   INPUT capacity 1 max BYTES INIT
   INPUT SPAN$ drop {: data :}
   path size data capacity READ-ALL data swap BYTES INPUT REPLACE ;


: OPEN-HANDLE ( ptr u8 n -- SERIAL:handle )
   115200 SERIAL:BAUD SERIAL:OPEN8N1 MATCH SERIAL:open-result
      opened OF ENDOF failed OF drop -1 throw ENDOF unsupported OF -2 throw ENDOF
   ;MATCH ;


: CLOSE-HANDLE ( SERIAL:handle -- )
   SERIAL:CLOSE MATCH SERIAL:status ok OF ENDOF failed OF drop ENDOF ;MATCH ;


: REPORT ( SERIAL-XMODEM:transfer-result -- )
   MATCH SERIAL-XMODEM:transfer-result
      completed OF s" completed " type BLEN>N . ENDOF
      timeout OF s" timeout" type ENDOF
      closed OF s" closed" type ENDOF
      failed OF s" failed " type SERIAL:ERRNO>N . ENDOF
      cancelled OF s" cancelled" type ENDOF
      retry-limit OF s" retry-limit" type ENDOF
      capacity OF s" capacity" type ENDOF
   ;MATCH cr ;
'''


class Port:
    def __init__(self, fd):
        self.fd = fd
        os.set_blocking(fd, False)

    def read(self, size=1, timeout=5):
        data = bytearray()
        deadline = time.monotonic() + timeout
        while len(data) < size:
            left = deadline - time.monotonic()
            assert left > 0 and select.select([self.fd], [], [], left)[0], (size, data)
            try:
                chunk = os.read(self.fd, size - len(data))
            except BlockingIOError:
                continue
            assert chunk, (size, data)
            data.extend(chunk)
        return bytes(data)

    def write(self, data):
        offset = 0
        deadline = time.monotonic() + 5
        while offset < len(data):
            left = deadline - time.monotonic()
            assert left > 0 and select.select([], [self.fd], [], left)[1]
            offset += os.write(self.fd, data[offset:])

    def expect(self, data):
        assert self.read(len(data)) == data, data

    def frame(self, size, crc=True):
        data = self.read(size + 4 + int(crc))
        assert data == packet(data[3:3 + size], data[1], size, crc), data[:3]
        return data

    def close(self):
        if self.fd >= 0:
            os.close(self.fd)
            self.fd = -1


class Checks:
    def __init__(self):
        self.out = ROOT / 'tmp/test-serial-xmodem'
        self.out.mkdir(parents=True, exist_ok=True)
        self.count = 0

    def exchange(self, name, peer, *, sending=True, data=b'payload', block=128,
                 wait=100, total=5000, maximum=65536, expected=None, received=None,
                 close_before=False, elapsed_max=None, actions=None, ack_wait=None):
        master, slave = os.openpty()
        port = Port(master)
        source = self.out / 'source.bin'
        source.write_bytes(data)
        output = self.out / 'received.bin'
        if sending:
            call = f'INPUT SPAN$ {block} BLOCK {total} >MS SESSION SERIAL-XMODEM:SEND'
            expected = expected or f'completed {len(data)}'
        else:
            call = f'{maximum} BYTES OUTPUT {total} >MS SESSION SERIAL-XMODEM:RECEIVE'
            expected = expected or f'completed {len(received)}'
        if actions is not None:
            call = actions
        code = SETUP + f'''
s" {source}" {len(data)} INPUT-READ
OUTPUT 16 BYTES INIT
: RUN ( -- )
   AIO:START
   s" {os.ttyname(slave)}" OPEN-HANDLE {{: handle:SERIAL:handle :}}
   handle {wait if ack_wait is None else ack_wait} >MS {wait} >MS SESSION SERIAL-XMODEM:INIT
   {'handle CLOSE-HANDLE' if close_before else ''}
   s" READY" type cr
   {call} REPORT
   s" {output}" OUTPUT SPAN$ BLEN>N WRITE-ALL
   SESSION SERIAL-XMODEM:DISPOSE {'handle CLOSE-HANDLE' if not close_before else ''}
   INPUT DISPOSE OUTPUT DISPOSE
   AIO:STOP ;
RUN
;package
'''
        path = self.out / 'case.f'
        path.write_text(code)
        process = subprocess.Popen([str(ROOT / 'bin/hb'), '--load', 'lib/serial-xmodem.f', str(path)],
                                   cwd=ROOT, stdin=subprocess.DEVNULL,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        try:
            # Read exactly READY without buffering a following fast result.
            ready = bytearray()
            for _ in range(6):
                assert select.select([process.stdout], [], [], 5)[0], name
                char = os.read(process.stdout.fileno(), 1)
                assert char, name
                ready.extend(char)
            assert ready == b'READY\n', (name, ready)
            os.close(slave)
            slave = -1
            start = time.monotonic()
            peer(port)
            stdout, stderr = process.communicate(timeout=8)
            elapsed = time.monotonic() - start
            assert process.returncode == 0, (name, stdout, stderr)
            assert stdout.decode().split() == expected.split(), (name, stdout, stderr)
            assert not stderr, (name, stderr)
            if received is not None:
                assert output.read_bytes() == received, (name, len(output.read_bytes()), len(received))
            if elapsed_max is not None:
                assert elapsed < elapsed_max, (name, elapsed)
            self.count += 1
        except BaseException as error:
            if process.poll() is None:
                process.terminate()
            stdout, stderr = process.communicate(timeout=3)
            raise AssertionError(name, str(error), stdout, stderr) from error
        finally:
            port.close()
            if slave >= 0:
                os.close(slave)

    def send_cases(self):
        for size, crc, data in [
            (128, True, bytes(range(256)) + b'\x1a\0\xff'),
            (1024, True, bytes(range(256)) * 8 + b'end'),
            (1024, True, bytes(range(256)) * 4),
            (128, False, bytes(range(256)) + b'tail'),
            (128, True, b''),
            (128, True, bytes(i & 255 for i in range(128 * 257))),
        ]:
            def peer(port):
                port.write(b'C' if crc else b'\x15')
                for index, start in enumerate(range(0, len(data), size), 1):
                    assert port.frame(size, crc) == packet(data[start:start + size], index & 255, size, crc)
                    port.write(b'\x06')
                port.expect(b'\x04')
                port.write(b'\x06')
            self.exchange(f'send-{size}-{crc}-{len(data)}', peer, data=data, block=1024 if not crc else size)

        def resend(port):
            port.write(b'C')
            original = port.frame(128)
            port.write(b'\x15')
            assert port.frame(128) == original
            # Drop this ACK: the next packet must be an unchanged retransmission.
            assert port.frame(128) == original
            port.write(b'\x18\x06')  # A single CAN does not cancel the ACK.
            port.expect(b'\x04')
            port.write(b'\x15')
            port.expect(b'\x04')
            port.write(b'\x06')
        self.exchange('send-retry-and-eot-nak', resend, wait=40)

        def delayed_ack(port):
            port.write(b'C')
            port.frame(128)
            time.sleep(0.08)
            port.write(b'\x06')
            port.expect(b'\x04')  # No retransmission under the shorter receive wait.
            port.write(b'\x06')
        self.exchange('send-distinct-ack-wait', delayed_ack, wait=20, ack_wait=200)

        def cancel(port):
            port.write(b'\x18\x18')
            port.expect(b'\x06')
        self.exchange('send-cancel-handshake', cancel, expected='cancelled')

        def cancel_reply(port):
            port.write(b'C')
            port.frame(128)
            cancel(port)
        self.exchange('send-cancel-reply', cancel_reply, expected='cancelled')

        def reject(port):
            port.write(b'C')
            for _ in range(10):
                port.frame(128)
                port.write(b'\x15')
            port.expect(b'\x18\x18\x18')
        self.exchange('send-retry-limit', reject, expected='retry-limit')

        def no_eot_ack(port):
            port.write(b'C')
            port.frame(128)
            port.write(b'\x06')
            port.expect(b'\x04' * 10 + b'\x18' * 3)
        self.exchange('send-missing-eot-ack', no_eot_ack, wait=20, expected='retry-limit')
        self.exchange('send-silent-timeout', lambda p: p.expect(b'\x18' * 3),
                      total=80, expected='timeout', elapsed_max=0.7)

        def noise(port):
            for _ in range(10):
                try:
                    port.write(b'x')
                except OSError as error:
                    assert error.errno == errno.EIO
                    break
                time.sleep(0.015)
        self.exchange('send-noise-deadline', noise, total=80, expected='timeout', elapsed_max=0.7)
        self.exchange('send-closed-handle', lambda p: None, close_before=True, expected='failed 9')
        self.exchange('send-disconnected', lambda p: p.close(), expected='closed')

    def receive_cases(self):
        payload = bytes(range(128))

        def fragmented(port):
            port.expect(b'C')
            frame = packet(payload, 1, 128, True)
            for begin, end in [(0, 1), (1, 4), (4, 19), (19, len(frame))]:
                port.write(frame[begin:end])
                time.sleep(0.005)
            port.expect(b'\x06')
            port.write(b'\x04')
            port.expect(b'\x06')
        self.exchange('receive-fragments', fragmented, sending=False, received=payload)

        def delayed_first(port):
            port.expect(b'C')
            time.sleep(0.08)
            port.write(packet(payload, 1, 128, True))
            port.expect(b'\x06')
            port.write(b'\x04')
            port.expect(b'\x06')
        self.exchange('receive-distinct-header-wait', delayed_first, sending=False,
                      wait=20, ack_wait=200, received=payload)

        def repaired(port):
            port.expect(b'C')
            frame = packet(payload, 1, 128, True)
            corrupt = bytearray(frame)
            corrupt[-1] ^= 1
            port.write(corrupt)
            port.expect(b'\x15')
            for _ in range(2):  # Duplicate after a lost ACK is not appended again.
                port.write(frame)
                port.expect(b'\x06')
            port.write(packet(b'next\x1a\0', 2, 1024, True))
            port.expect(b'\x06')
            port.write(b'\x04')
            port.expect(b'\x06')
        padded = b'next\x1a\0' + b'\x1a' * (1024 - 6)
        self.exchange('receive-crc-retry-duplicate-mixed', repaired, sending=False,
                      wait=30, ack_wait=1000, received=payload + padded)

        def wrong_sequence(port):
            port.expect(b'C')
            for sequence in (0, 3):
                port.write(packet(payload, sequence, 128, True))
                port.expect(b'\x15')
            port.write(packet(payload, 1, 128, True))
            port.expect(b'\x06')
            port.write(b'\x04')
            port.expect(b'\x06')
        self.exchange('receive-unexpected-sequence', wrong_sequence, sending=False, wait=25, received=payload)

        def checksum(port):
            port.expect(b'CCC\x15')
            port.write(packet(payload, 1, 128, False))
            port.expect(b'\x06')
            port.write(b'\x04')
            port.expect(b'\x06')
        self.exchange('receive-checksum-fallback', checksum, sending=False, wait=25, received=payload)

        def wrap(port):
            port.expect(b'C')
            for sequence in range(1, 258):
                port.write(packet(payload, sequence & 255, 128, True))
                port.expect(b'\x06')
            port.write(b'\x04')
            port.expect(b'\x06')
        self.exchange('receive-sequence-wrap', wrap, sending=False, received=payload * 257)

        for accepted in (0, 1):
            def capacity(port):
                port.expect(b'C')
                if accepted:
                    port.write(packet(payload, 1, 128, True))
                    port.expect(b'\x06')
                port.write(packet(payload, accepted + 1, 128, True))
                port.expect(b'\x18' * 3)
            self.exchange(f'receive-capacity-{accepted}', capacity, sending=False,
                          maximum=127 + accepted, expected='capacity', received=payload * accepted)

        def cancel(port):
            port.expect(b'C')
            port.write(packet(payload, 1, 128, True))
            port.expect(b'\x06')
            port.write(b'\x18\x18')
            port.expect(b'\x06')
        self.exchange('receive-cancelled-prefix', cancel, sending=False,
                      expected='cancelled', received=payload)

        def partial(port):
            port.expect(b'C')
            frame = packet(payload, 1, 128, True)
            port.write(frame[:19])
            port.expect(b'\x15')
            port.write(frame)
            port.expect(b'\x06')
            port.write(b'\x04')
            port.expect(b'\x06')
        self.exchange('receive-partial-timeout-retry', partial, sending=False, wait=30, received=payload)

        def drip(port):
            port.expect(b'C')
            port.write(b'\x02\x01\xfe')
            for _ in range(15):
                try:
                    port.write(b'x')
                except OSError as error:
                    assert error.errno == errno.EIO
                    break
                time.sleep(0.01)
        self.exchange('receive-drip-overall-timeout', drip, sending=False, wait=40, total=80,
                      expected='timeout', received=b'', elapsed_max=0.7)

        self.exchange('receive-silent-retry-limit', lambda p: p.expect(b'CCC' + b'\x15' * 7 + b'\x18' * 3),
                      sending=False, wait=20, expected='retry-limit', received=b'')
        self.exchange('receive-closed-handle', lambda p: None, sending=False,
                      close_before=True, expected='failed 9', received=b'')

        def disconnect(port):
            port.expect(b'C')
            port.close()
        self.exchange('receive-disconnected', disconnect, sending=False, expected='closed', received=b'')

        def empty(port):
            port.expect(b'C')
            port.write(b'\x04')
            port.expect(b'\x06')
        self.exchange('receive-empty', empty, sending=False, maximum=0, received=b'')

    def reuse(self):
        actions = '''
INPUT SPAN$ 128 BLOCK 5000 >MS SESSION SERIAL-XMODEM:SEND REPORT
128 BYTES OUTPUT 5000 >MS SESSION SERIAL-XMODEM:RECEIVE REPORT
INPUT SPAN$ 1024 BLOCK 5000 >MS SESSION SERIAL-XMODEM:SEND REPORT
1024 BYTES OUTPUT 5000 >MS SESSION SERIAL-XMODEM:RECEIVE
'''
        def peer(port):
            for crc, size in [(True, 128), (False, 1024)]:
                port.write(b'C' if crc else b'\x15')
                assert port.frame(128, crc) == packet(b'payload', 1, 128, crc)
                port.write(b'\x06')
                port.expect(b'\x04')
                port.write(b'\x06')
                port.expect(b'C')
                port.write(packet(b'reply', 1, size, True))
                port.expect(b'\x06')
                port.write(b'\x04')
                port.expect(b'\x06')
        self.exchange('reuse-both-directions', peer, actions=actions,
                      expected='completed 7 completed 128 completed 7 completed 1024',
                      received=b'reply' + b'\x1a' * 1019)

        def recover(port):
            port.write(b'\x18\x18')
            port.expect(b'\x06')
            port.write(b'C')
            assert port.frame(128) == packet(b'payload', 1, 128, True)
            port.write(b'\x06')
            port.expect(b'\x04')
            port.write(b'\x06')
        self.exchange('reuse-after-peer-cancel', recover, actions='''
INPUT SPAN$ 128 BLOCK 5000 >MS SESSION SERIAL-XMODEM:SEND REPORT
INPUT SPAN$ 128 BLOCK 5000 >MS SESSION SERIAL-XMODEM:SEND
''', expected='cancelled completed 7')

    def native(self, body, *, refused=False):
        path = self.out / 'validation.f'
        path.write_text(SETUP + body + '\n;package\n')
        result = subprocess.run([str(ROOT / 'bin/hb'), '--load', 'lib/serial-xmodem.f', str(path)],
                                cwd=ROOT, input='', text=True, capture_output=True, timeout=15)
        assert result.returncode == (70 if refused else 0), (result.stdout, result.stderr)
        if refused:
            assert 'non-certified' in result.stderr, result.stderr
        self.count += 1
        return result.stdout.split()

    def validation(self):
        for handle, ack_wait, receive_wait in [
            (0, 0, 100), (0, -1, 100), (0, 2147483648, 100),
            (0, 100, 0), (0, 100, -1), (0, 100, 2147483648),
            (-1, 100, 100), (2147483648, 100, 100),
        ]:
            code = f'''
: RUN ( -- )
   [: {handle} SERIAL:>HANDLE {ack_wait} >MS {receive_wait} >MS SESSION SERIAL-XMODEM:INIT ;] catch . ;
RUN
'''
            assert self.native(code) == ['-9122']
        for effect in ['SERIAL:baud -- XMODEM:payload-size',
                       'XMODEM:sequence -- XMODEM:payload-size',
                       'SERIAL-XMODEM:transfer-result -- SERIAL:io-result']:
            self.native(f': BAD ( {effect} ) ;\n', refused=True)

    def concurrent(self):
        terminals = [os.openpty(), os.openpty()]
        ports = [Port(master) for master, _ in terminals]
        names = [os.ttyname(slave) for _, slave in terminals]
        code = SETUP + f'''
here FFI:>CELL 7 and 8 swap - 7 and allot
variable ENTERED
variable DONE
create SESSION0 SERIAL-XMODEM:SESSION-BYTES allot
create SESSION1 SERIAL-XMODEM:SESSION-BYTES allot
create OUTPUT0 HDR-BYTES allot
create OUTPUT1 HDR-BYTES allot
create SOURCE0 8 allot
create SOURCE1 8 allot
TASK:MIN-STACK TASK:TASK WORKER0
TASK:MIN-STACK TASK:TASK WORKER1

: COUNT ( SERIAL-XMODEM:transfer-result -- n )
   MATCH SERIAL-XMODEM:transfer-result
      completed OF BLEN>N ENDOF timeout OF -1 throw ENDOF
      closed OF -2 throw ENDOF failed OF SERIAL:ERRNO>N throw ENDOF
      cancelled OF -3 throw ENDOF retry-limit OF -4 throw ENDOF capacity OF -5 throw ENDOF
   ;MATCH ;


: READY ( SERIAL:handle -- )
   s" ready" SERIAL:BYTES 1000 >MS SERIAL:WRITE MATCH SERIAL:io-result
      transferred OF BLEN>N 5 <> if -1 throw then ENDOF
      timeout OF -2 throw ENDOF closed OF -3 throw ENDOF failed OF SERIAL:ERRNO>N throw ENDOF
   ;MATCH ;


: CHECK-REPLY ( n ptr a -- ) {{: identity:n output:ptr :}}
   output SPAN$ BLEN>N {{: data length:n :}}
   length 128 <> if -1 throw then
   length 0 do data i + c@ identity <> if -1 throw then loop ;


: WORK ( ptr u8 n ptr a ptr a ptr u8 n -- )
   {{: path length:n session:ptr output:ptr data identity:n :}}
   1 ENTERED atomic-add drop
   begin ENTERED atomic@ 2 < while TASK:PAUSE repeat
   path length OPEN-HANDLE {{: handle:SERIAL:handle :}}
   handle 1000 >MS 1000 >MS session SERIAL-XMODEM:INIT output 16 BYTES INIT handle READY
   4 0 do
      identity i + data c!
      data 1 BYTES 128 BLOCK 5000 >MS session SERIAL-XMODEM:SEND COUNT 1 <> if -1 throw then
      128 BYTES output 5000 >MS session SERIAL-XMODEM:RECEIVE COUNT 128 <> if -1 throw then
      identity i + output CHECK-REPLY
   loop
   session SERIAL-XMODEM:DISPOSE output DISPOSE handle CLOSE-HANDLE
   1 DONE atomic-add drop ;


: WORK0 ( -- ) s" {names[0]}" SESSION0 OUTPUT0 SOURCE0 160 WORK ;
: WORK1 ( -- ) s" {names[1]}" SESSION1 OUTPUT1 SOURCE1 177 WORK ;


: WAIT-DONE ( ptr n -- ) {{: worker:ptr :}}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat ;


: RUN ( -- )
   AIO:START
   ['] WORK0 WORKER0 TASK:ACTIVATE ['] WORK1 WORKER1 TASK:ACTIVATE
   WORKER0 WAIT-DONE WORKER1 WAIT-DONE
   WORKER0 TASK:KILL WORKER1 TASK:KILL DONE atomic@ .
   AIO:STOP ;
RUN
;package
'''
        path = self.out / 'concurrent.f'
        path.write_text(code)
        process = subprocess.Popen([str(ROOT / 'bin/hb'), '--load', 'lib/serial-xmodem.f', str(path)],
                                   cwd=ROOT, stdin=subprocess.DEVNULL,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        try:
            for port in ports:
                port.expect(b'ready')
            for iteration in range(4):
                for port in ports:
                    port.write(b'C')
                for port, identity in zip(ports, (160, 177)):
                    assert port.frame(128) == packet(bytes([identity + iteration]), 1, 128, True)
                    port.write(b'\x06')
                for port in ports:
                    port.expect(b'\x04')
                    port.write(b'\x06')
                    port.expect(b'C')
                for port, identity in reversed(list(zip(ports, (160, 177)))):
                    port.write(packet(bytes([identity + iteration]) * 128, 1, 128, True))
                    port.expect(b'\x06')
                    port.write(b'\x04')
                    port.expect(b'\x06')
            stdout, stderr = process.communicate(timeout=8)
            assert process.returncode == 0 and stdout.split() == [b'2'] and not stderr, (stdout, stderr)
            self.count += 1
        except BaseException as error:
            if process.poll() is None:
                process.terminate()
            stdout, stderr = process.communicate(timeout=3)
            raise AssertionError('concurrent', str(error), stdout, stderr) from error
        finally:
            if process.poll() is None:
                process.terminate()
                process.communicate(timeout=3)
            for port in ports:
                port.close()
            for _, slave in terminals:
                os.close(slave)


if __name__ == '__main__':
    checks = Checks()
    checks.send_cases()
    checks.receive_cases()
    checks.reuse()
    checks.validation()
    checks.concurrent()
    print(f'serial-xmodem: {checks.count} checks passed')
