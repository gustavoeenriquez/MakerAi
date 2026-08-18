# Servidor A2A de referencia: TODAS las respuestas se serializan con los tipos
# protobuf del SDK oficial (a2a-sdk 1.1.2), asi que el formato de cable es el
# canonico de v1.0, no una interpretacion nuestra.
import json, sys, uuid
from http.server import BaseHTTPRequestHandler, HTTPServer
import a2a.types as T
from google.protobuf import json_format

PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8290
TASKS = {}


def card(base):
    c = T.AgentCard()
    c.name = "Agente Referencia SDK"
    c.description = "Servidor A2A v1.0 conforme, serializado con a2a-sdk"
    c.version = "1.0.0"
    i = c.supported_interfaces.add()
    i.url = base
    try:
        i.protocol_binding = T.TransportProtocol.TRANSPORT_PROTOCOL_JSONRPC
    except Exception:
        pass
    i.protocol_version = "1.0.0"
    c.default_input_modes.append("text/plain")
    c.default_output_modes.append("text/plain")
    s = c.skills.add()
    s.id = "echo"
    s.name = "Echo"
    s.description = "Devuelve el texto en mayusculas"
    return json_format.MessageToJson(c, indent=2)


def build_task(tid, ctx, state, text=None):
    t = T.Task()
    t.id = tid
    t.context_id = ctx
    t.status.state = state
    if text is not None:
        a = t.artifacts.add()
        a.artifact_id = tid + "-result"
        a.name = "result"
        p = a.parts.add()
        p.text = text
    return t


def rpc_result(rid, payload_msg):
    return {"jsonrpc": "2.0", "id": rid,
            "result": json.loads(json_format.MessageToJson(payload_msg))}


def rpc_error(rid, code, msg):
    return {"jsonrpc": "2.0", "id": rid, "error": {"code": code, "message": msg}}


class H(BaseHTTPRequestHandler):
    def log_message(self, *a):
        pass

    def _send(self, obj, code=200):
        b = json.dumps(obj, ensure_ascii=False).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(b)))
        self.end_headers()
        self.wfile.write(b)

    def do_GET(self):
        if self.path in ("/.well-known/agent-card.json", "/.well-known/agent.json"):
            b = card(f"http://localhost:{PORT}/").encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.send_header("Content-Length", str(len(b)))
            self.end_headers()
            self.wfile.write(b)
        else:
            self._send({"error": "not found"}, 404)

    def do_POST(self):
        n = int(self.headers.get("Content-Length", 0))
        raw = self.rfile.read(n).decode("utf-8")
        print("<<< PETICION RECIBIDA:", raw[:400], flush=True)
        try:
            req = json.loads(raw)
        except Exception:
            return self._send(rpc_error(None, -32700, "parse error"))

        rid = req.get("id")
        method = req.get("method", "")
        params = req.get("params") or {}

        if method in ("SendMessage", "message/send"):
            msg = params.get("message") or {}
            texto = " ".join(p.get("text", "") for p in msg.get("parts", []) if "text" in p)
            tid = msg.get("taskId") or str(uuid.uuid4())
            ctx = msg.get("contextId") or str(uuid.uuid4())
            t = build_task(tid, ctx, T.TaskState.TASK_STATE_COMPLETED, texto.upper())
            TASKS[tid] = t
            # CLAVE: SendMessage devuelve SendMessageResponse, con el Task ENVUELTO
            resp = T.SendMessageResponse()
            resp.task.CopyFrom(t)
            out = rpc_result(rid, resp)
            print(">>> RESPUESTA ENVIADA:", json.dumps(out)[:400], flush=True)
            return self._send(out)

        if method in ("GetTask", "tasks/get"):
            t = TASKS.get(params.get("id", ""))
            if t is None:
                return self._send(rpc_error(rid, -32001, "Task not found"))
            # GetTask devuelve el Task DIRECTO (no hay wrapper en el proto)
            return self._send(rpc_result(rid, t))

        if method in ("CancelTask", "tasks/cancel"):
            t = TASKS.get(params.get("id", ""))
            if t is None:
                return self._send(rpc_error(rid, -32001, "Task not found"))
            return self._send(rpc_error(rid, -32002, "Task is not cancelable"))

        return self._send(rpc_error(rid, -32601, "Method not found: " + method))


print(f"Servidor A2A de referencia (a2a-sdk {getattr(T, '__version__', '1.1.2')}) en http://localhost:{PORT}/", flush=True)
HTTPServer(("localhost", PORT), H).serve_forever()
