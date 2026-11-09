// mcp.js - a module's rpcs sent to an MCP server's HTTP endpoint. The browser
// host (host.js) sends them to its page origin's /mcp; the bun runner
// (host-cli.mjs) relays them under --mcp to a server it names, with a bearer
// token and a time limit. It sets no Origin header.

let rpcId = 0;

// A tool name as an Mcp-Name value: as it is when every byte is visible ASCII,
// else =?base64?...?= of its UTF-8 bytes.
function mcpName(name) {
  return /^[!-~]*$/.test(name) ? name : `=?base64?${new TextEncoder().encode(name).toBase64()}?=`;
}

// A module's rpc for the endpoint at url: tools/call goes there as a JSON-RPC
// 2.0 request; the rest is answered here. A JSON-RPC response to it answers
// with its result or error whatever the HTTP status; any other body rejects.
// With a token the request carries it as a bearer; with ms the fetch aborts,
// and the rpc rejects, after ms milliseconds.
export function mcpRpc(url, token, ms) {
  return async (method, params) => {
    if (method === "ui/update-model-context") return '{"result":{}}';
    if (method !== "tools/call") return '{"error":{"code":-32601,"message":"Method not found"}}';
    const id = ++rpcId;
    const call = JSON.parse(params);
    call._meta = {
      ...call._meta,
      "io.modelcontextprotocol/protocolVersion": "2026-07-28",
      "io.modelcontextprotocol/clientCapabilities": {},
    };
    // MCP 2026-07-28's HTTP transport requires both Accept types, the version
    // header, Mcp-Method naming the method and, for a tools/call, Mcp-Name
    // naming params.name; params._meta repeats the version beside the client's
    // capabilities.
    const headers = {
      "Content-Type": "application/json",
      Accept: "application/json, text/event-stream",
      "MCP-Protocol-Version": "2026-07-28",
      "Mcp-Method": "tools/call",
    };
    if (typeof call.name === "string") headers["Mcp-Name"] = mcpName(call.name);
    if (token !== undefined) headers.Authorization = `Bearer ${token}`;
    const init = {
      method: "POST",
      headers,
      body: JSON.stringify({ jsonrpc: "2.0", id, method: "tools/call", params: call }),
    };
    if (ms !== undefined) init.signal = AbortSignal.timeout(ms);
    const r = await fetch(url, init);
    // A body cut off by the time limit rejects as the limit, not as a bad body.
    const reply = await r.json().catch((e) => {
      if (init.signal?.aborted) throw e;
      return null;
    });
    if (reply === null || typeof reply !== "object" || reply.jsonrpc !== "2.0" || reply.id !== id ||
        ("result" in reply) === ("error" in reply))
      throw new Error(`${url}: ${r.status}: the body is not a JSON-RPC response`);
    return JSON.stringify("result" in reply ? { result: reply.result } : { error: reply.error });
  };
}
