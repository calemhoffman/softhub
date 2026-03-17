import streamlit as st
import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, ConstantKernel, WhiteKernel
import os

# ==========================================
# 1. CONFIGURATION & UI CONTROLS
# ==========================================
st.set_page_config(page_title="Active Learning Scanner", layout="wide", initial_sidebar_state="expanded")

# Custom Dark Theme CSS
st.markdown("""
    <style>
    /* Main Background */
    .stApp {
        background-color: #0b0e14;
    }
    /* Headers */
    h1, h2, h3 {
        color: #e0e0e0 !important;
        font-family: 'Inter', sans-serif;
    }
    /* Cards/Metric Backgrounds */
    [data-testid="stMetricValue"] {
        color: #00d4ff !important;
    }
    div[data-testid="stMetric"] {
        background-color: #161b22;
        border: 1px solid #30363d;
        padding: 15px;
        border-radius: 12px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.3);
    }
    /* Sidebar */
    [data-testid="stSidebar"] {
        background-color: #0d1117;
        border-right: 1px solid #30363d;
    }
    /* Forms */
    div[data-testid="stForm"] {
        border-radius: 12px;
        background-color: #161b22;
        border: 1px solid #30363d;
    }
    /* Expander */
    .streamlit-expanderHeader {
        background-color: #161b22 !important;
        border-radius: 8px !important;
    }
    </style>
    """, unsafe_allow_html=True)

CSV_FILE = "scan_backup.csv"
X_MIN, X_MAX = 0.0, 10.0

with st.sidebar:
    st.header("🎛️ Configuration")
    st.divider()
    
    st.subheader("Acquisition Weights")
    ALPHA = st.slider("Exploration (Uncertainty)", 0.0, 5.0, 1.0, 0.1, help="Higher values prioritize areas with less data.")
    BETA = st.slider("Refinement (Curvature)", 0.0, 0.5, 0.05, 0.01, help="Higher values prioritize areas where the model is changing rapidly.")
    
    st.divider()
    
    st.subheader("Prior Model")
    PRIOR_CENTER = st.slider("Prior Center", X_MIN, X_MAX, 5.0, 0.1)
    PRIOR_SCALE = st.slider("Prior Width (Scale)", 0.1, 5.0, 2.0, 0.1)
    
    st.divider()
    
    if st.button("🗑️ Reset Data", use_container_width=True, type="secondary"):
        if os.path.exists(CSV_FILE):
            os.remove(CSV_FILE)
        # Clear specific session state keys to force re-initialization
        if 'initialized' in st.session_state:
            del st.session_state.initialized
        st.rerun()

    st.divider()
    
    # 2. CONVERGENCE METRICS (Moved to Sidebar)
    if 'X_sampled' in st.session_state:
        st.subheader("📈 Convergence")
        # These are calculated later in the script, so we use placeholders or move calculations up.
        # For simplicity, we'll rely on the script rerun flow which will have these values available 
        # because the sidebar is rendered *after* initialization but *before* plotting.
        # Actually, let's move the math section *above* the sidebar once we identify dependencies.

def estimated_shape(x):
    """Your known, estimated line-shape (Prior)."""
    return np.exp(-0.5 * ((x - PRIOR_CENTER) / PRIOR_SCALE)**2)

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
# 4. DATA PREPARATION FOR UI & PLOTS
# ==========================================
X_plot = np.linspace(X_MIN, X_MAX, 500)
X_plot_reshaped = X_plot.reshape(-1, 1)

Y_prior = estimated_shape(X_plot)
gp_pred_residuals, gp_std = gp.predict(X_plot_reshaped, return_std=True)
Y_gp_mean = gp_pred_residuals + Y_prior

upper_bound = Y_gp_mean + 1.96 * gp_std
lower_bound = Y_gp_mean - 1.96 * gp_std

# Peak Position Uncertainty via Monte Carlo
n_mc_samples = 100
gp_samples = gp.sample_y(X_plot_reshaped, n_samples=n_mc_samples, random_state=42)
# Re-add prior to each sample
samples_total = gp_samples + Y_prior.reshape(-1, 1)
peak_indices = np.argmax(samples_total, axis=0)
peak_x_positions = X_plot[peak_indices]
peak_x_mean = np.mean(peak_x_positions)
peak_x_std = np.std(peak_x_positions)

# ==========================================
# 5. STREAMLIT USER INTERFACE
# ==========================================
st.title("🎯 Active Learning Distribution Scanner")
st.markdown("This tool uses **Gaussian Process Regression** and **Adaptive Mesh Refinement** to suggest where to measure next, minimizing total experiments.")

# Layout: UI on the left, Plot on the right
col1, col2 = st.columns([1, 2.5])

with col1:
    st.subheader("Quick Input")
    
    st.info(f"Target X: **{next_x:.4f}**")
    
    # Input Form
    with st.form("measurement_form", clear_on_submit=True):
        # We use st.text_input here because st.number_input has a known bug in Streamlit
        # where hitting 'Enter' doesn't always sync the hand-typed value before the form 
        # submission triggers, leading to 0.0 being submitted.
        user_y_str = st.text_input("Resulting Y value (Enter to submit):", value="0.0")
        submitted = st.form_submit_button("Submit Measurement", use_container_width=True)
        
        if submitted:
            try:
                val_y = float(user_y_str)
                st.session_state.X_sampled = np.append(st.session_state.X_sampled, next_x)
                st.session_state.Y_sampled = np.append(st.session_state.Y_sampled, val_y)
                current_data = np.column_stack((st.session_state.X_sampled, st.session_state.Y_sampled))
                np.savetxt(CSV_FILE, current_data, delimiter=",", header="X_coordinate,Y_measurement", comments="")
                st.rerun()
            except ValueError:
                st.error("Please enter a valid numeric value.")

    st.divider()
    
    st.subheader("Results")
    # Find the maximum of the GP Mean
    max_idx = np.argmax(Y_gp_mean)
    x_max_val = X_plot[max_idx]
    y_max_val = Y_gp_mean[max_idx]
    
    st.metric("Predicted Peak X", f"{x_max_val:.4f}", delta=f"±{peak_x_std:.3f}")
    st.metric("Predicted Peak Y", f"{y_max_val:.4f}")

    st.divider()
    
    st.subheader("Query Model")
    query_x = st.number_input("X query:", min_value=X_MIN, max_value=X_MAX, value=(X_MIN+X_MAX)/2.0, step=0.1)
    q_res, q_std = gp.predict(np.array([[query_x]]), return_std=True)
    q_total = q_res[0] + estimated_shape(query_x)
    st.write(f"**Y = {q_total:.4f} ± {q_std[0]:.4f}**")

# Update Sidebar with Data Management & Convergence (now that math is done)
with st.sidebar:
    st.divider()
    st.subheader("🛠️ Manage Data")
    
    # Convergence guidance
    max_uncertainty = np.max(gp_std)
    signal_range = np.max(st.session_state.Y_sampled) - np.min(st.session_state.Y_sampled)
    uncertainty_percent = (max_uncertainty / signal_range * 100) if signal_range > 0 else 100
    
    st.metric("Model Uncertainty", f"{uncertainty_percent:.1f}%", help="Threshold for convergence is < 5%")
    
    with st.expander("View/Edit Points"):
        delete_idx = st.selectbox("Point to Delete:", options=range(len(st.session_state.X_sampled)), 
                                  format_func=lambda i: f"P{i}: X={st.session_state.X_sampled[i]:.2f}")
        if st.button("❌ Remove Point", use_container_width=True):
            st.session_state.X_sampled = np.delete(st.session_state.X_sampled, delete_idx)
            st.session_state.Y_sampled = np.delete(st.session_state.Y_sampled, delete_idx)
            current_data = np.column_stack((st.session_state.X_sampled, st.session_state.Y_sampled))
            np.savetxt(CSV_FILE, current_data, delimiter=",", header="X_coordinate,Y_measurement", comments="")
            st.rerun()

# ==========================================
# 5. UNIFIED PLOTLY VISUALIZATION
# ==========================================
with col2:
    # Prepare Subplots
    fig = make_subplots(
        rows=2, cols=1, 
        shared_xaxes=True, 
        vertical_spacing=0.08,
        row_heights=[0.65, 0.35],
        subplot_titles=("Live Surrogate Model", "Acquisition Score")
    )

    # --- ROW 1: SURROGATE MODEL ---
    # Uncertainty Band
    fig.add_trace(go.Scatter(
        x=np.concatenate([X_plot, X_plot[::-1]]),
        y=np.concatenate([upper_bound, lower_bound[::-1]]),
        fill='toself', fillcolor='rgba(0, 212, 255, 0.15)',
        line=dict(color='rgba(255,255,255,0)'),
        hoverinfo="skip", name='95% Confidence'
    ), row=1, col=1)

    # Prior
    fig.add_trace(go.Scatter(
        x=X_plot, y=Y_prior, mode='lines',
        line=dict(color='rgba(255,255,255,0.3)', dash='dash', width=1.5), name='Prior'
    ), row=1, col=1)

    # GP Mean
    fig.add_trace(go.Scatter(
        x=X_plot, y=Y_gp_mean, mode='lines',
        line=dict(color='#00d4ff', width=3), name='Prediction'
    ), row=1, col=1)

    # Sampled Points
    fig.add_trace(go.Scatter(
        x=st.session_state.X_sampled, y=st.session_state.Y_sampled,
        mode='markers', marker=dict(color='#ff4b4b', size=10, symbol='circle', 
                                    line=dict(color='white', width=1)),
        name='Measured'
    ), row=1, col=1)

    # Peak Range (Shaded)
    if peak_x_std > (X_MAX - X_MIN) * 0.001:
        fig.add_vrect(
            x0=max(X_MIN, peak_x_mean - peak_x_std), 
            x1=min(X_MAX, peak_x_mean + peak_x_std),
            fillcolor="gold", opacity=0.15, layer="below", line_width=0,
            row=1, col=1
        )

    # --- ROW 2: ACQUISITION SCORE ---
    X_score = np.linspace(X_MIN, X_MAX, 300)
    _, std_dev_score = gp.predict(X_score.reshape(-1, 1), return_std=True)
    curvatures = [calculate_curvature(gp, x) for x in X_score]
    scores = (ALPHA * std_dev_score) + (BETA * np.array(curvatures))
    
    fig.add_trace(go.Scatter(
        x=X_score, y=scores, mode='lines',
        line=dict(color='#ffa500', width=2.5),
        fill='tozeroy', fillcolor='rgba(255, 165, 0, 0.15)',
        name='Acq. Score'
    ), row=2, col=1)

    # Vertical line for Next Best Point
    fig.add_vline(x=next_x, line_width=2, line_dash="dash", line_color="#00f224", 
                  annotation_text="Next Target", annotation_position="top right",
                  row="all", col=1)

    # Styling
    fig.update_layout(
        template="plotly_dark",
        height=750,
        margin=dict(l=20, r=20, t=60, b=40),
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
        hovermode="x unified",
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)'
    )
    
    fig.update_xaxes(showgrid=True, gridcolor='rgba(255,255,255,0.05)', zeroline=False)
    fig.update_yaxes(showgrid=True, gridcolor='rgba(255,255,255,0.05)', zeroline=False)

    st.plotly_chart(fig, use_container_width=True)

# ==========================================
# 6. FINAL ANALYSIS & QUERY
# ==========================================
# Removed legacy individual plots and analysis columns, now integrated above.