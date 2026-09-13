"""Linux-only OS boundary: deny the child's next mapping after its ready marker."""
import pathlib
import resource
import selectors
import subprocess
import sys

child_source = pathlib.Path(__file__).with_name("address-cell-oom-child.f")
child = subprocess.Popen([sys.argv[1], "--load", str(child_source)],
                         stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE, text=True)
try:
    with selectors.DefaultSelector() as ready:
        ready.register(child.stdout, selectors.EVENT_READ)
        assert ready.select(30), "child did not reach the allocation boundary"
    assert child.stdout.readline() == "address-cell-oom: ready\n"
    status = pathlib.Path(f"/proc/{child.pid}/status").read_text()
    size_kib = next(int(row.split()[1]) for row in status.splitlines()
                    if row.startswith("VmSize:"))
    resource.prlimit(child.pid, resource.RLIMIT_AS, (size_kib * 1024,) * 2)
    out, err = child.communicate("x", timeout=30)
    assert child.returncode == 96, (child.returncode, out, err)
    assert out == "", repr(out)
    assert err == "hb: address-cell storage allocation failed\n", repr(err)
finally:
    if child.poll() is None:
        child.kill()
    child.wait()
print("address-cell-oom: ok")
