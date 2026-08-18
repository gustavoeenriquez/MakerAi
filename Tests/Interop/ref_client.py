# Cliente A2A conforme a v1.0: valida la Agent Card y la respuesta contra los
# tipos protobuf del SDK oficial. Si el servidor no habla v1.0, protojson falla.
import json, sys, urllib.request, uuid
import a2a.types as T
from google.protobuf import json_format

BASE = (sys.argv[1] if len(sys.argv) > 1 else "http://localhost:8280").rstrip("/")
OK, FAIL = "  [OK]  ", "  [FALLA]"


def get(url):
    with urllib.request.urlopen(url, timeout=15) as r:
        return json.loads(r.read().decode("utf-8"))


def post(url, payload):
    data = json.dumps(payload).encode("utf-8")
    req = urllib.request.Request(url, data=data, headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=120) as r:
        return json.loads(r.read().decode("utf-8"))


print(f"=== Cliente v1.0 contra {BASE} ===\n")

# ---------- 1. Agent Card ----------
print("1) Agent Card")
try:
    raw = get(BASE + "/.well-known/agent-card.json")
    print("   recibida:", json.dumps(raw)[:160], "...")
    try:
        card = json_format.ParseDict(raw, T.AgentCard())
        print(OK, "parsea como AgentCard v1.0")
    except Exception as e:
        print(FAIL, "no parsea como AgentCard v1.0 ->", str(e)[:200])
        card = json_format.ParseDict(raw, T.AgentCard(), ignore_unknown_fields=True)
        print("        (con ignore_unknown_fields si parsea)")
    if not card.supported_interfaces:
        print(FAIL, "falta supportedInterfaces: un cliente v1.0 no sabe a que URL hablar")
    else:
        print(OK, "supportedInterfaces:", card.supported_interfaces[0].url)
except Exception as e:
    print(FAIL, "no se pudo obtener la card:", e)

# ---------- 2. SendMessage ----------
print("\n2) SendMessage")
req = {"jsonrpc": "2.0", "id": 1, "method": "SendMessage",
       "params": {"message": {"messageId": str(uuid.uuid4()), "role": "ROLE_USER",
                              "parts": [{"text": "hola desde el cliente v1.0"}]}}}
try:
    resp = post(BASE + "/", req)
    if "error" in resp:
        print(FAIL, "error JSON-RPC:", resp["error"])
        sys.exit(1)
    result = resp.get("result", {})
    print("   result:", json.dumps(result)[:200], "...")
    try:
        smr = json_format.ParseDict(result, T.SendMessageResponse())
        cual = smr.WhichOneof("payload") if smr.DESCRIPTOR.oneofs else ("task" if smr.HasField("task") else "message")
        print(OK, f"parsea como SendMessageResponse (payload='{cual}')")
        if smr.HasField("task"):
            print(OK, "task.id =", smr.task.id, " estado =", T.TaskState.Name(smr.task.status.state))
    except Exception as e:
        print(FAIL, "no parsea como SendMessageResponse ->", str(e)[:200])
        # Diagnostico: el Task viene plano en vez de envuelto?
        try:
            t = json_format.ParseDict(result, T.Task(), ignore_unknown_fields=True)
            if t.id:
                print("        DIAGNOSTICO: el result es un Task PLANO (formato v0.3).")
                print("        v1.0 exige envolverlo: {'result': {'task': {...}}}")
        except Exception as e2:
            print("        tampoco parsea como Task:", str(e2)[:120])
except Exception as e:
    print(FAIL, "fallo la llamada:", e)
