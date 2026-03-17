import streamlit as st
import pandas as pd
import time
from atlas_client import AtlasClient

# Set page config
st.set_page_config(page_title="ATLAS Control System Access", layout="wide")

st.title("🛰️ ATLAS Control System Access")
st.markdown("---")

# Initialize Client
if "client" not in st.session_state:
    st.session_state.client = AtlasClient()

client = st.session_state.client

# Sidebar for Configuration
with st.sidebar:
    st.header("Settings")
    base_url = st.text_input("Server URL", value=client.base_url)
    if base_url != client.base_url:
        st.session_state.client = AtlasClient(base_url)
        st.rerun()
    
    st.markdown("---")
    st.info("This tool allows you to monitor, read, and write to ATLAS devices using the Web Access Server API.")

# Main Tabs
tab_monitor, tab_read, tab_write, tab_devices = st.tabs([
    "📊 Monitoring", 
    "🔍 Read Point", 
    "✍️ Write/Control", 
    "📋 Device List"
])

# Tab: Device List
with tab_devices:
    st.header("Search Devices")
    dev_type = st.selectbox("Device Type", ["*", "MAGNET", "GAUGE", "SLIT", "VALVE", "FARADAY_CUP"], index=0)
    if st.button("List Devices"):
        with st.spinner("Fetching devices..."):
            devices = client.list_devices(dev_type)
            if devices:
                st.write(f"Found {len(devices)} devices:")
                st.json(devices)
            else:
                st.warning("No devices found or error occurred.")

# Tab: Read Point
with tab_read:
    st.header("Read Channel")
    col1, col2 = st.columns(2)
    with col1:
        r_device = st.text_input("Device (e.g., STP201)", key="read_dev")
    with col2:
        r_chan = st.text_input("Channel (e.g., MONITOR_X)", key="read_chan")
    
    if st.button("Read Value"):
        if r_device and r_chan:
            res = client.read([{"device": r_device, "chan": r_chan}])
            st.write("Response:")
            st.json(res)
        else:
            st.error("Please enter both Device and Channel.")

# Tab: Write/Control
with tab_write:
    st.header("Write Channel")
    col1, col2, col3 = st.columns(3)
    with col1:
        w_device = st.text_input("Device (e.g., STP201)", key="write_dev")
    with col2:
        w_chan = st.text_input("Channel (e.g., CONTROL_X)", key="write_chan")
    with col3:
        w_val = st.number_input("Value", value=0.0)
    
    if st.button("Send Command"):
        if w_device and w_chan:
            res = client.write([{"device": w_device, "chan": w_chan, "val": w_val}])
            st.write("Response:")
            st.json(res)
        else:
            st.error("Please enter Device and Channel.")

# Tab: Monitoring
with tab_monitor:
    st.header("Active Monitoring")
    st.write("Add devices to monitor below:")
    
    if "monitored_devices" not in st.session_state:
        st.session_state.monitored_devices = []

    m_col1, m_col2, m_col3 = st.columns([2, 2, 1])
    with m_col1:
        m_dev = st.text_input("Monitor Device")
    with m_col2:
        m_chan = st.text_input("Monitor Channel")
    with m_col3:
        if st.button("Add"):
            if m_dev and m_chan:
                st.session_state.monitored_devices.append({"device": m_dev, "chan": m_chan})
            else:
                st.error("Fill both fields.")

    if st.session_state.monitored_devices:
        st.write("Current Monitor List:")
        df_mon = pd.DataFrame(st.session_state.monitored_devices)
        st.table(df_mon)
        
        if st.button("Clear History"):
            st.session_state.monitored_devices = []
            st.rerun()

        poll_interval = st.slider("Polling Interval (seconds)", 1, 60, 5)
        
        if st.toggle("Start Polling"):
            placeholder = st.empty()
            while True:
                results = client.read(st.session_state.monitored_devices)
                with placeholder.container():
                    st.write(f"Last updated: {time.strftime('%H:%M:%S')}")
                    st.json(results)
                time.sleep(poll_interval)
    else:
        st.info("Add devices to start monitoring.")
