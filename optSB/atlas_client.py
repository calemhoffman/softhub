import requests
from typing import List, Dict, Any, Optional

class AtlasClient:
    """
    A client to interact with the ATLAS Web Access Server.
    """
    def __init__(self, base_url: str = "http://146.139.66.20:8042/"):
        self.base_url = base_url.rstrip("/") + "/"

    def read(self, devices: List[Dict[str, str]]) -> List[Dict[str, Any]]:
        """
        Reads values from the specified devices and channels.
        
        Args:
            devices: List of dicts, e.g., [{"device": "STP201", "chan": "MONITOR_X"}]
            
        Returns:
            JSON response from the server.
        """
        try:
            response = requests.get(self.base_url, json=devices, timeout=5)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            return [{"error": str(e)}]

    def write(self, devices: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Writes values to the specified devices and channels.
        
        Args:
            devices: List of dicts, e.g., [{"device": "STP201", "chan": "CONTROL_X", "val": 1.5}]
            
        Returns:
            JSON response from the server.
        """
        try:
            response = requests.post(self.base_url, json=devices, timeout=5)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            return {"error": str(e)}

    def list_devices(self, device_type: str = "*") -> List[str]:
        """
        Lists available devices of a certain type.
        
        Args:
            device_type: e.g., 'MAGNET', 'GAUGE', or '*' for all.
            
        Returns:
            List of device names.
        """
        try:
            url = f"{self.base_url}{device_type}/"
            response = requests.get(url, timeout=5)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            return [f"Error: {e}"]

if __name__ == "__main__":
    # Quick test
    client = AtlasClient()
    print("Listing all devices:")
    print(client.list_devices("*"))
