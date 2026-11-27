// socket.js - a view's WebSocket to its server, which carries the module's
// tools/call rpcs, each sent at once and answered as its response arrives,
// matched by id, and brings the server's notifications. view.js imports it from a blob: URL; it imports nothing.
//
//   open({ request, notify, onCode, carried }) -> socket
//   request(method, params)  the host's, as view.js's start receives it: the
//                            session comes from tools/call view_socket {},
//                            whose result's _meta is {url, seconds}
//   notify(method, paramsJSON)
//                            each server message with a method and no id, its
//                            params {} when it has none, but view/code and
//                            view/alive, which are the shell's own; and
//                            view/open {} each time a socket opens
//   onCode(code)             view/code's params.code
//   carried                  the carried() of the view this one replaces, or
//                            null: its resume is kept, and its session reused
//                            until it expires
//   socket.call(method, params)
//                            -> Promise of {response, binary}: the response
//                            message, as the host's request resolves, and the
//                            bytes that followed the response's JSON in its
//                            binary message, [] when it came as text or the
//                            shell answered itself; a response with bytes is
//                            one binary message, a u64 little-endian n, the n
//                            bytes of its JSON in UTF-8, then the call's
//                            bytes. The promise rejects a response
//                            holding neither or both of result and error. A
//                            call sent and unanswered when its socket closes
//                            is answered {error: {code: -32000, message: "the
//                            view's socket closed"}}, and every call in ended
//                            {error: {code: -32000, message: <the reason>}}; a
//                            call not yet sent waits for the next socket
//   socket.resume(params)    keeps params, the module's last view/resume
//   socket.carried()         {resume, session}: the kept resume or null, and
//                            the session {url, seconds, asked}, asked its ask's
//                            Date.now(), or null. Restart hands it to the next
//                            view.js, and the next release's view.js reads it,
//                            so its shape only grows
//   socket.dispose()         ends the machine
//
// The socket is a machine over state, step(input, value) its one entry. The
// states are
//   asking    tools/call view_socket {} is with the host
//   opening   a WebSocket(url) is connecting
//   open      the socket is open; every call is sent at once
//   waiting   backing off after a close or failure: 1 s, doubling to 30 s,
//             reset to 1 s once a socket proves itself, by its first
//             answered call or its first view/alive, never by opening
//   resting   hidden with no call outstanding: no socket
//   ended     the ask failed or view.js disposed; reason answers every call
// and the inputs
//   asked        the ask's session
//   ask-failed   the ask's failure: a host error, a tool error (isError) or a
//                result whose _meta lacks url or seconds
//   opened       the socket opened
//   message      a text frame
//   binary       a binary frame, an ArrayBuffer
//   closed       the socket closed or failed to open, with its close code
//   silent       40 s open with nothing received; the server sends view/alive
//                every 25 s, so only a lost socket is silent that long
//   waited       the backoff passed
//   hidden, visible  document.visibilityState changed
//   call         the module's call
//   dispose      view.js disposes
// Idle is no call sent or waiting. Connecting asks when there is no session
// or seconds have passed since its ask, and otherwise opens. The table:
//   asking   asked: resting when hidden and idle, else opening
//            ask-failed: ended, reason the tool error's text or the host
//            error's message
//   opening  opened: open, which notifies view/open and sends every waiting
//            call
//            closed: waiting; under 4403 the session is unknown or expired,
//            so it is forgotten and the next connect asks
//   open     message: liveness, then a notification, a response or a server
//            request, which is answered -32601; a response answers the sent
//            call of its id, then rests when hidden and idle; a response to
//            a sent call or a view/alive resets the backoff to 1 s
//            binary: liveness, then a response and its call's bytes,
//            answered as a message's response is; a frame too short for its
//            n, whose JSON does not parse or that holds no response is
//            logged and dropped
//            closed: waiting, as in opening
//            silent: waiting, closing the socket (4000)
//            call: sent
//   waiting  waited: resting when hidden and idle, else connecting
//   resting  visible: connecting
//            call: queued, then connecting
//   ended    call: answered with reason
//            dispose: reason becomes "the view's socket closed"
// In asking, opening and waiting a call is queued. In opening, open and
// waiting, hidden rests when idle, closing the socket (1000). Every state but
// ended goes to ended on dispose, reason "the view's socket closed". Leaving
// open answers every sent call "the view's socket closed". Leaving a socket
// removes its handlers and closes it, and leaving a state clears its timers,
// so only the current socket's and state's inputs arrive; every other pairing
// in the table is one that cannot arrive or changes nothing, named none.
const CLOSED = -32000;
const CLOSED_TEXT = "the view's socket closed";
const NOT_FOUND = -32601;
const REJECTED = 4403; // the server's close of an unknown or expired session
const SILENT = 4000; // the shell's close of a silent socket
const SOCKET_TOOL = "view_socket";
const FIRST_DELAY = 1000;
const LAST_DELAY = 30000;
const SILENCE = 40000;
const UTF8 = new TextDecoder("utf-8", { fatal: true });

export function open({ request, notify, onCode, carried }) {
  const s = {
    state: null,
    session: carried?.session ?? null,
    resume: carried?.resume ?? null,
    ws: null,
    delay: FIRST_DELAY,
    backoff: null, // waiting's timer
    silence: null, // open's silent timer
    lastId: 0,
    waiting: [], // calls not yet sent
    sent: new Map(), // id -> the call sent and unanswered
    reason: CLOSED_TEXT, // ended's answer to every call
  };

  const hidden = () => document.visibilityState === "hidden";
  const idle = () => s.sent.size === 0 && s.waiting.length === 0;
  const failed = (id, message) => ({ jsonrpc: "2.0", id, error: { code: CLOSED, message } });
  // A callback's throw is logged; it never stops a transition.
  const tell = (fn, ...args) => { try { fn(...args); } catch (e) { console.error(e); } };

  const clear = () => {
    clearTimeout(s.backoff);
    clearTimeout(s.silence);
    s.backoff = s.silence = null;
  };
  // Leaves the socket, if any: its handlers go, it closes under code, and
  // every sent call is answered.
  const drop = (code = 1000) => {
    clear();
    if (s.ws) {
      s.ws.onopen = s.ws.onmessage = s.ws.onclose = null;
      s.ws.close(code);
      s.ws = null;
    }
    const calls = [...s.sent.values()];
    s.sent.clear();
    for (const { id, resolve } of calls) resolve({ response: failed(id, CLOSED_TEXT), binary: [] });
  };
  const listen = () => {
    clearTimeout(s.silence);
    s.silence = setTimeout(() => step("silent"), SILENCE);
  };
  const send = (message) => s.ws.send(JSON.stringify({ jsonrpc: "2.0", ...message }));
  // Every waiting call goes out.
  const pump = () => {
    for (const call of s.waiting.splice(0)) {
      const { id, method, params } = call;
      s.sent.set(id, call);
      send({ id, method, params });
    }
  };

  const asking = () => {
    s.state = "asking";
    const asked = Date.now();
    request("tools/call", { name: SOCKET_TOOL, arguments: {} }).then((r) => {
      if ("error" in r) throw new Error(String(r.error?.message));
      const result = r.result;
      if (result?.isError)
        throw new Error((result.content ?? []).filter((c) => c.type === "text").map((c) => c.text).join(" "));
      const { url, seconds } = result?._meta ?? {};
      if (typeof url !== "string" || typeof seconds !== "number" || !(seconds > 0))
        throw new Error(`${SOCKET_TOOL}: the result's _meta lacks url or seconds`);
      return { url, seconds, asked };
    }).then((session) => step("asked", session), (e) => step("ask-failed", e));
  };
  const opening = () => {
    s.state = "opening";
    let ws;
    try {
      ws = new WebSocket(s.session.url);
    } catch (e) {
      console.error(e);
      waiting();
      return;
    }
    s.ws = ws;
    ws.binaryType = "arraybuffer";
    ws.onopen = () => step("opened");
    ws.onmessage = ({ data }) => step(typeof data === "string" ? "message" : "binary", data);
    ws.onclose = ({ code }) => step("closed", code);
  };
  const connect = () => {
    const { session } = s;
    if (session === null || Date.now() >= session.asked + session.seconds * 1000) asking();
    else opening();
  };
  const opened = () => {
    s.state = "open";
    listen();
    tell(notify, "view/open", "{}");
    pump();
  };
  const waiting = (code) => {
    drop(code);
    s.state = "waiting";
    s.backoff = setTimeout(() => step("waited"), s.delay);
    s.delay = Math.min(s.delay * 2, LAST_DELAY);
  };
  const resting = () => {
    drop();
    s.state = "resting";
  };
  const ended = (reason = CLOSED_TEXT) => {
    drop();
    s.state = "ended";
    s.reason = reason;
    document.removeEventListener("visibilitychange", seen);
    for (const { id, resolve } of s.waiting.splice(0)) resolve({ response: failed(id, reason), binary: [] });
  };
  const rest = () => { if (hidden() && idle()) resting(); };
  const closed = (code) => {
    if (code === REJECTED) s.session = null;
    waiting();
  };

  // A frame's JSON-RPC message, or null, logged, when text is not one.
  const parsed = (text) => {
    let m;
    try { m = JSON.parse(text); } catch (e) { console.error(e); return null; }
    if (m === null || typeof m !== "object" || m.jsonrpc !== "2.0") {
      console.error(new Error(`the view's socket sent ${text.slice(0, 200)}`));
      return null;
    }
    return m;
  };
  // A response answers the sent call of its id with binary.
  const answered = (m, binary) => {
    const call = s.sent.get(m.id);
    if (call === undefined) {
      console.error(new Error(`the view's socket answered ${JSON.stringify(m.id)}, which is no sent call`));
      return;
    }
    s.sent.delete(m.id);
    s.delay = FIRST_DELAY;
    if (("result" in m) === ("error" in m)) call.reject(new Error(`${m.id}: the message is not a JSON-RPC response`));
    else call.resolve({ response: m, binary });
    rest();
  };
  // open's text frame: liveness, then a notification, a response or a server
  // request, which is answered -32601.
  const received = (data) => {
    listen();
    const m = parsed(data);
    if (m === null) return;
    if (typeof m.method === "string") {
      if ("id" in m) { send({ id: m.id, error: { code: NOT_FOUND, message: "Method not found" } }); return; }
      if (m.method === "view/code") tell(onCode, m.params?.code);
      else if (m.method === "view/alive") s.delay = FIRST_DELAY;
      else tell(notify, m.method, JSON.stringify(m.params ?? {}));
      return;
    }
    answered(m, []);
  };
  // open's binary frame: liveness, then a response as a u64 little-endian n,
  // the n bytes of its JSON in UTF-8, then its call's bytes, which are not
  // copied.
  const decoded = (data) => {
    listen();
    const size = data.byteLength;
    const n = size < 8 ? -1 : new DataView(data).getBigUint64(0, true);
    if (n < 0 || n > BigInt(size - 8)) {
      console.error(new Error(`the view's socket sent ${size} binary bytes, which do not hold the length they name`));
      return;
    }
    const end = 8 + Number(n);
    let text;
    try { text = UTF8.decode(new Uint8Array(data, 8, end - 8)); } catch (e) { console.error(e); return; }
    const m = parsed(text);
    if (m === null) return;
    if (typeof m.method === "string") {
      console.error(new Error(`the view's socket sent ${m.method} in a binary message, which holds only a response`));
      return;
    }
    answered(m, end === size ? [] : [new Uint8Array(data, end)]);
  };
  const queue = (call) => { s.waiting.push(call); };

  const none = () => {};
  // state -> input -> transition(value); every input in every state.
  const table = {
    asking: {
      asked: (session) => { s.session = session; if (hidden() && idle()) resting(); else opening(); },
      "ask-failed": (e) => { console.error(e); ended(e.message); },
      opened: none, message: none, binary: none, closed: none, silent: none, waited: none,
      hidden: none, // decided at asked
      visible: none,
      call: queue,
      dispose: () => ended(),
    },
    opening: {
      asked: none, "ask-failed": none,
      opened,
      message: none, // a socket sends nothing before it opens
      binary: none, // a socket sends nothing before it opens
      closed,
      silent: none, waited: none,
      hidden: rest,
      visible: none,
      call: queue,
      dispose: () => ended(),
    },
    open: {
      asked: none, "ask-failed": none, opened: none,
      message: received,
      binary: decoded,
      closed,
      silent: () => waiting(SILENT),
      waited: none,
      hidden: rest,
      visible: none,
      call: (call) => { queue(call); pump(); },
      dispose: () => ended(),
    },
    waiting: {
      asked: none, "ask-failed": none, opened: none, message: none, binary: none, closed: none,
      silent: none,
      waited: () => { if (hidden() && idle()) resting(); else connect(); },
      hidden: rest,
      visible: none,
      call: queue,
      dispose: () => ended(),
    },
    resting: {
      asked: none, "ask-failed": none, opened: none, message: none, binary: none, closed: none,
      silent: none,
      waited: none, hidden: none,
      visible: connect,
      call: (call) => { queue(call); connect(); },
      dispose: () => ended(),
    },
    ended: {
      asked: none, "ask-failed": none, opened: none, message: none, binary: none, closed: none,
      silent: none, waited: none, hidden: none, visible: none,
      call: ({ id, resolve }) => resolve({ response: failed(id, s.reason), binary: [] }),
      dispose: () => { s.reason = CLOSED_TEXT; },
    },
  };
  function step(input, value) {
    table[s.state][input](value);
  }
  function seen() {
    step(hidden() ? "hidden" : "visible");
  }

  document.addEventListener("visibilitychange", seen);
  connect();
  return {
    call(method, params) {
      return new Promise((resolve, reject) => step("call", { id: ++s.lastId, method, params, resolve, reject }));
    },
    resume(params) { s.resume = params; },
    carried() { return { resume: s.resume, session: s.session }; },
    dispose() { step("dispose"); },
  };
}
