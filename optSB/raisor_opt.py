import streamlit as st
import numpy as np
import plotly.graph_objects as go
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, ConstantKernel, WhiteKernel
import os

# ==========================================
# 1. CONFIGURATION & FUNCTIONS
# ==========================================
st.set_page_config(page_title="Active Learning Scanner", layout="wide")

CSV_FILE = "scan_backup.csv"
X_MIN, X_MAX = 0.0, 10.0
ALPHA = 1.0   # Weight for GP Uncertainty
BETA = 0.05   # Weight for Curve Sharpness

def estimated_shape(x):
    """Your known, estimated line-shape (Prior)."""
    return np.exp(-0.5 * ((x - 5.0) / 2.0)**2)

def calculate_curvature(gp, x_val, h=1e-4):
    """Calculates the 2nd derivative of the GP + Prior."""
    X_eval = np.array([[x_val - h], [x_val], [x_val + h]])
    pred_residuals = gp.predict(X_eval)
    pred_total = pred_residuals + estimated_shape(X_eval.flatten())
    curvature = (pred_total[2] - 2*pred_total[1] + pred_total[0]) / (h**2)
    return abs(curvature)

# ==========================================
# 2. MEMORY / SESSION STATE INITIALIZATION
# ==========================================
if 'initialized' not in st.session_state:
    if os.path.exists(CSV_FILE):
        # RESUME FROM SAVED DATA
        data = np.loadtxt(CSV_FILE, delimiter=",", skiprows=1)
        st.session_state.X_sampled = data[:, 0]
        st.session_state.Y_sampled = data[:, 1]
    else:
        # START FRESH: Initialize with 5 coarse points
        # (Using a dummy function here just to give you starting data)
        st.session_state.X_sampled = np.linspace(X_MIN, X_MAX, 5)
        base = np.exp(-0.5 * ((st.session_state.X_sampled - 5.0) / 2.0)**2)
        peak = 0.8 * np.exp(-0.5 * ((st.session_state.X_sampled - 3.2) / 0.3)**2)
        st.session_state.Y_sampled = base + peak
        
        # Save the initial setup to CSV immediately
        current_data = np.column_stack((st.session_state.X_sampled, st.session_state.Y_sampled))
        np.savetxt(CSV_FILE, current_data, delimiter=",", header="X_coordinate,Y_measurement", comments="")
        
    st.session_state.initialized = True

# Rebuild mesh intervals dynamically based on currently sorted X points
# This is much safer than saving the mesh to memory when loading from CSV!
sorted_X = np.sort(st.session_state.X_sampled)
mesh_intervals = [(sorted_X[i], sorted_X[i+1]) for i in range(len(sorted_X)-1)]

# ==========================================
# 3. GAUSSIAN PROCESS MATH
# ==========================================
X_train = st.session_state.X_sampled.reshape(-1, 1)
residuals = st.session_state.Y_sampled - estimated_shape(st.session_state.X_sampled)

# Train the GP
kernel = ConstantKernel(1.0) * RBF(length_scale=1.0) + WhiteKernel(noise_level=1e-5)
gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=5)
gp.fit(X_train, residuals)

# Score intervals to find the next best point
interval_scores = []
for interval in mesh_intervals:
    midpoint = (interval[0] + interval[1]) / 2.0
    _, std_dev = gp.predict(np.array([[midpoint]]), return_std=True)
    curvature = calculate_curvature(gp, midpoint)
    score = (ALPHA * std_dev[0]) + (BETA * curvature)
    interval_scores.append((score, interval, midpoint))

# Identify the target for the UI
best_score, best_interval, next_x = max(interval_scores, key=lambda item: item[0])

# ==========================================
# 4. STREAMLIT USER INTERFACE
# ==========================================
st.title("🎯 Active Learning Distribution Scanner")
st.markdown("This tool uses **Gaussian Process Regression** and **Adaptive Mesh Refinement** to suggest where to measure next, minimizing total experiments.")

# Layout: UI on the left, Plot on the right
col1, col2 = st.columns([1, 2])

with col1:
    st.subheader("Data Summary")
    st.metric("Total Points Scanned", len(st.session_state.X_sampled))
    
    st.divider()
    
    st.subheader("Next Recommended Scan")
    st.info(f"Please measure your system at:\n### X = {next_x:.4f}")
    
    # Input Form
    with st.form("measurement_form"):
        user_y = st.number_input("Enter the resulting Y value:", format="%.4f")
        submitted = st.form_submit_button("Submit Measurement", use_container_width=True)
        
        if submitted:
            # 1. Update Memory
            st.session_state.X_sampled = np.append(st.session_state.X_sampled, next_x)
            st.session_state.Y_sampled = np.append(st.session_state.Y_sampled, user_y)
            
            # 2. Save to CSV
            current_data = np.column_stack((st.session_state.X_sampled, st.session_state.Y_sampled))
            np.savetxt(CSV_FILE, current_data, delimiter=",", header="X_coordinate,Y_measurement", comments="")
            
            # 3. Force UI Refresh
            st.rerun()

# ==========================================
# 5. LIVE PLOTLY VISUALIZATION
# ==========================================
with col2:
    X_plot = np.linspace(X_MIN, X_MAX, 500)
    X_plot_reshaped = X_plot.reshape(-1, 1)
    
    Y_prior = estimated_shape(X_plot)
    gp_pred_residuals, gp_std = gp.predict(X_plot_reshaped, return_std=True)
    Y_gp_mean = gp_pred_residuals + Y_prior
    
    upper_bound = Y_gp_mean + 1.96 * gp_std
    lower_bound = Y_gp_mean - 1.96 * gp_std

    fig = go.Figure()

    # Uncertainty Band
    fig.add_trace(go.Scatter(
        x=np.concatenate([X_plot, X_plot[::-1]]),
        y=np.concatenate([upper_bound, lower_bound[::-1]]),
        fill='toself', fillcolor='rgba(0, 176, 246, 0.2)',
        line=dict(color='rgba(255,255,255,0)'),
        hoverinfo="skip", name='95% Confidence Interval'
    ))

    # Prior
    fig.add_trace(go.Scatter(
        x=X_plot, y=Y_prior, mode='lines',
        line=dict(color='gray', dash='dash', width=2), name='Estimated Prior'
    ))

    # GP Mean
    fig.add_trace(go.Scatter(
        x=X_plot, y=Y_gp_mean, mode='lines',
        line=dict(color='blue', width=2), name='GP Prediction'
    ))

    # Sampled Points
    fig.add_trace(go.Scatter(
        x=st.session_state.X_sampled, y=st.session_state.Y_sampled,
        mode='markers', marker=dict(color='red', size=10, symbol='x'),
        name='Measured Points'
    ))

    fig.update_layout(
        title="Live Surrogate Model",
        xaxis_title="X Coordinate", yaxis_title="Measurement Value (Y)",
        template="plotly_white", hovermode="x unified",
        margin=dict(l=0, r=0, t=40, b=0)
    )

    st.plotly_chart(fig, use_container_width=True)