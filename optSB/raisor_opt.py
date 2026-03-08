import streamlit as st
import numpy as np
import plotly.graph_objects as go
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, ConstantKernel, WhiteKernel
import os

# ==========================================
# 1. CONFIGURATION & UI CONTROLS
# ==========================================
st.set_page_config(page_title="Active Learning Scanner", layout="wide")

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

    st.divider()
    
    # 2. CONVERGENCE METRICS
    st.subheader("📈 Convergence Guidance")
    max_uncertainty = np.max(gp_std)
    # Heuristic: if max uncertainty is < 5% of the signal range, it's pretty good
    signal_range = np.max(st.session_state.Y_sampled) - np.min(st.session_state.Y_sampled)
    uncertainty_percent = (max_uncertainty / signal_range * 100) if signal_range > 0 else 100
    
    st.metric("Max Model Uncertainty", f"{max_uncertainty:.4f}", 
              delta=f"{uncertainty_percent:.1f}% of range", delta_color="inverse")
    
    if uncertainty_percent < 5.0:
        st.success("✅ Model appears well-converged!")
    else:
        st.warning("⚠️ More points recommended for better stability.")

    st.divider()

    # 3. MANAGE MEASUREMENTS
    with st.expander("🛠️ Manage Measurements"):
        st.write("Review or remove specific points:")
        # Combine data for display
        data_df = np.column_stack((np.arange(len(st.session_state.X_sampled)), 
                                  st.session_state.X_sampled, 
                                  st.session_state.Y_sampled))
        
        # Selection to delete
        delete_idx = st.selectbox("Select Point Index to Remove:", options=range(len(st.session_state.X_sampled)), format_func=lambda i: f"Point {i}: X={st.session_state.X_sampled[i]:.2f}, Y={st.session_state.Y_sampled[i]:.2f}")
        
        if st.button("❌ Delete Selected Point", use_container_width=True):
            st.session_state.X_sampled = np.delete(st.session_state.X_sampled, delete_idx)
            st.session_state.Y_sampled = np.delete(st.session_state.Y_sampled, delete_idx)
            
            # Save updated data to CSV
            current_data = np.column_stack((st.session_state.X_sampled, st.session_state.Y_sampled))
            np.savetxt(CSV_FILE, current_data, delimiter=",", header="X_coordinate,Y_measurement", comments="")
            st.rerun()

# ==========================================
# 5. LIVE PLOTLY VISUALIZATION
# ==========================================
with col2:
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

    # Peak Position Uncertainty Shaded Region
    if peak_x_std > (X_MAX - X_MIN) * 0.001:  # Only show if not trivial
        fig.add_vrect(
            x0=max(X_MIN, peak_x_mean - peak_x_std), 
            x1=min(X_MAX, peak_x_mean + peak_x_std),
            fillcolor="gold", opacity=0.3, layer="below", line_width=0,
            annotation_text="Peak Range", annotation_position="top left",
            name="Peak X Uncertainty"
        )

    st.plotly_chart(fig, use_container_width=True)

    # Acquisition Score Visualization
    st.subheader("🧠 Acquisition Score (Why this point?)")
    
    # Calculate scores across the entire range
    X_score = np.linspace(X_MIN, X_MAX, 250)
    X_score_reshaped = X_score.reshape(-1, 1)
    _, std_dev_score = gp.predict(X_score_reshaped, return_std=True)
    
    # Calculate curvature for the visual curve
    curvatures = [calculate_curvature(gp, x) for x in X_score]
    scores = (ALPHA * std_dev_score) + (BETA * np.array(curvatures))
    
    fig_score = go.Figure()
    
    fig_score.add_trace(go.Scatter(
        x=X_score, y=scores, mode='lines',
        line=dict(color='orange', width=3),
        fill='tozeroy', fillcolor='rgba(255, 165, 0, 0.1)',
        name='Total Score'
    ))
    
    # Component breakdown
    fig_score.add_trace(go.Scatter(
        x=X_score, y=ALPHA * std_dev_score, mode='lines',
        line=dict(color='cyan', dash='dot', width=1),
        name='Uncertainty Component'
    ))
    
    fig_score.add_trace(go.Scatter(
        x=X_score, y=BETA * np.array(curvatures), mode='lines',
        line=dict(color='magenta', dash='dot', width=1),
        name='Curvature Component'
    ))

    fig_score.add_vline(x=next_x, line_width=2, line_dash="dash", line_color="green", 
                       annotation_text="Next Best Point", annotation_position="top right")

    fig_score.update_layout(
        xaxis_title="X Coordinate", yaxis_title="Acquisition Score",
        template="plotly_white", height=300,
        margin=dict(l=0, r=0, t=30, b=0),
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1)
    )
    
    st.plotly_chart(fig_score, use_container_width=True)

# ==========================================
# 6. FINAL ANALYSIS & QUERY
# ==========================================
st.divider()
st.header("🏁 Final Model Analysis")
c_ana1, c_ana2 = st.columns(2)

with c_ana1:
    st.subheader("Optimization Results")
    # Find the maximum of the GP Mean
    max_idx = np.argmax(Y_gp_mean)
    x_max_val = X_plot[max_idx]
    y_max_val = Y_gp_mean[max_idx]
    
    st.success(f"### Predicted Maximum Found at:\n## X = {x_max_val:.4f} ± {peak_x_std:.4f}")
    st.metric("Predicted Peak Value (Y)", f"{y_max_val:.4f}")
    st.write("The ± value represents the **$1\sigma$ uncertainty on the peak location ($X$)** based on 100 Monte Carlo samples.")

with c_ana2:
    st.subheader("Query the Model")
    query_x = st.number_input("Enter an X value to query:", min_value=X_MIN, max_value=X_MAX, value=(X_MIN+X_MAX)/2.0, step=0.1)
    
    q_res, q_std = gp.predict(np.array([[query_x]]), return_std=True)
    q_total = q_res[0] + estimated_shape(query_x)
    
    st.info(f"Predicted Y at X={query_x:.4f}:")
    st.write(f"### Y = {q_total:.4f} ± {q_std[0]:.4f}")
    st.progress(min(max((q_total / (y_max_val if y_max_val !=0 else 1)), 0.0), 1.0), text="Height relative to peak")