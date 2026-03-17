from flask import Flask, request, jsonify

app = Flask(__name__)

# Mock data store
devices_data = {
    "STP201": {"MONITOR_X": 1.23, "CONTROL_X": 1.0, "MONITOR_Y": -0.45},
    "BND101": {"MONITOR_GAUSS": 500.0, "CONTROL_GAUSS": 500.0}
}

@app.route("/", methods=["GET"])
def read_devices():
    req_data = request.json
    results = []
    for item in req_data:
        dev = item.get("device")
        chan = item.get("chan")
        val = devices_data.get(dev, {}).get(chan, None)
        results.append({"device": dev, "chan": chan, "val": val})
    return jsonify(results)

@app.route("/", methods=["POST"])
def write_devices():
    req_data = request.json
    for item in req_data:
        dev = item.get("device")
        chan = item.get("chan")
        val = item.get("val")
        if dev not in devices_data:
            devices_data[dev] = {}
        devices_data[dev][chan] = val
    return jsonify({"status": "ok", "count": len(req_data)})

@app.route("/<device_type>/", methods=["GET"])
def list_devices(device_type):
    if device_type == "*":
        return jsonify(list(devices_data.keys()))
    # Simplified filtering
    return jsonify([d for d in devices_data.keys() if device_type in d])

if __name__ == "__main__":
    app.run(port=8042)
