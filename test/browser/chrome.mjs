// Headless Chrome for the browser peers: its launch and its teardown.
import { readFileSync } from "node:fs";

// A detached Chrome leads its own process group. Chrome's crash dumps go to the
// user's own Crashpad folder unless --breakpad-dump-location names another.
export function launch(chrome, profile, log) {
  return Bun.spawn([chrome, "--headless=new", "--no-first-run", "--no-default-browser-check",
    "--enable-unsafe-webgpu", "--use-angle=swiftshader", "--disable-gpu-sandbox",
    "--disable-updater-scheduler", `--breakpad-dump-location=${profile}`, "--remote-debugging-port=0",
    `--user-data-dir=${profile}`, "about:blank"],
    { detached: true, stdout: "ignore", stderr: Bun.file(log) });
}

// Chrome removes its code-sign clone only when the browser closes itself; what
// is left of its group after that runs nothing a test checks.
export async function stopGroup(child, socket) {
  const group = child.pid;
  let signals = ["SIGTERM", "SIGKILL"];
  if (socket?.readyState === WebSocket.OPEN) {
    socket.send(JSON.stringify({ id: 0, method: "Browser.close" }));
    await child.exited;
    signals = ["SIGKILL"];
  }
  for (const signal of signals) {
    if (!signalGroup(group, signal)) return;
    const limit = performance.now() + 5000;
    while (performance.now() < limit) {
      if (!signalGroup(group, 0)) return;
      await Bun.sleep(25);
    }
  }
  throw new Error(`Chrome process group ${group} survived SIGKILL`);
}

// Whether a member is left. Darwin answers EPERM, not ESRCH, while the group's
// last members have exited but are not yet reaped: members remain.
function signalGroup(group, signal) {
  try { process.kill(-group, signal); return true; }
  catch (error) { if (error.code === "ESRCH") return false; if (error.code === "EPERM") return true; throw error; }
}

export async function until(check) {
  const limit = performance.now() + 15000;
  while (performance.now() < limit) { if (await check()) return; await Bun.sleep(25); }
  throw new Error("browser condition timed out");
}

// Chrome creates DevToolsActivePort before it writes the port into it, so the
// port is taken only from a whole first line.
export async function devtoolsPort(active) {
  let port;
  await until(() => {
    try { port = /^(\d+)\n/.exec(readFileSync(active, "utf8"))?.[1]; }
    catch (error) { if (error.code !== "ENOENT") throw error; }
    return port !== undefined;
  });
  return port;
}
