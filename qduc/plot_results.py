#!/usr/bin/env python3
"""
Professional Plotly visualization of QDUC nuclear reaction cross section data
with 90's color scheme
"""
import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

# Load data
data = np.loadtxt('output.dat')
angle = data[:, 0]
mean = data[:, 1]
plus1sigma = data[:, 2]
plus2sigma = data[:, 3]
minus1sigma = data[:, 4]
minus2sigma = data[:, 5]

# 90's inspired color scheme
colors = {
    'mean': '#FF1493',      # Deep Pink (very 90's)
    'sigma1': '#00CED1',    # Dark Turquoise
    'sigma2': '#9370DB',    # Medium Purple
    'fill1': 'rgba(0, 206, 209, 0.3)',   # Turquoise with transparency
    'fill2': 'rgba(147, 112, 219, 0.2)', # Purple with transparency
    'grid': '#E0E0E0',
    'bg': '#FFFFFF'
}

# Create figure
fig = go.Figure()

# Add ±2σ band (outermost)
fig.add_trace(go.Scatter(
    x=angle,
    y=plus2sigma,
    mode='lines',
    line=dict(width=0),
    showlegend=False,
    hoverinfo='skip'
))

fig.add_trace(go.Scatter(
    x=angle,
    y=minus2sigma,
    mode='lines',
    line=dict(width=0),
    fillcolor=colors['fill2'],
    fill='tonexty',
    name='±2σ (95% CI)',
    hovertemplate='<b>±2σ Band</b><br>Angle: %{x}°<br>Upper: %{customdata[0]:.4f} mb/sr<br>Lower: %{customdata[1]:.4f} mb/sr<extra></extra>',
    customdata=np.column_stack([plus2sigma, minus2sigma])
))

# Add ±1σ band
fig.add_trace(go.Scatter(
    x=angle,
    y=plus1sigma,
    mode='lines',
    line=dict(width=0),
    showlegend=False,
    hoverinfo='skip'
))

fig.add_trace(go.Scatter(
    x=angle,
    y=minus1sigma,
    mode='lines',
    line=dict(width=0),
    fillcolor=colors['fill1'],
    fill='tonexty',
    name='±1σ (68% CI)',
    hovertemplate='<b>±1σ Band</b><br>Angle: %{x}°<br>Upper: %{customdata[0]:.4f} mb/sr<br>Lower: %{customdata[1]:.4f} mb/sr<extra></extra>',
    customdata=np.column_stack([plus1sigma, minus1sigma])
))

# Add mean line (on top)
fig.add_trace(go.Scatter(
    x=angle,
    y=mean,
    mode='lines',
    line=dict(color=colors['mean'], width=3),
    name='Mean Cross Section',
    hovertemplate='<b>Mean</b><br>Angle: %{x}°<br>σ: %{y:.4f} mb/sr<extra></extra>'
))

# Update layout with 90's aesthetic
fig.update_layout(
    title={
        'text': '<b>¹³²Sn(d,p)¹³³Sn Nuclear Reaction Cross Section</b><br>' +
                '<sub>KDUQ Uncertainty Quantification (416 Parameter Sets) | E<sub>d</sub> = 7.65 MeV</sub>',
        'x': 0.5,
        'xanchor': 'center',
        'font': {'size': 20, 'color': '#2C3E50', 'family': 'Arial Black, sans-serif'}
    },
    xaxis=dict(
        title='<b>Center of Mass Angle (degrees)</b>',
        titlefont=dict(size=16, color='#34495E', family='Arial, sans-serif'),
        tickfont=dict(size=12, color='#34495E'),
        gridcolor=colors['grid'],
        showgrid=True,
        zeroline=True,
        zerolinecolor='#95A5A6',
        zerolinewidth=2,
        range=[0, 180]
    ),
    yaxis=dict(
        title='<b>Differential Cross Section (mb/sr)</b>',
        titlefont=dict(size=16, color='#34495E', family='Arial, sans-serif'),
        tickfont=dict(size=12, color='#34495E'),
        gridcolor=colors['grid'],
        showgrid=True,
        zeroline=True,
        zerolinecolor='#95A5A6',
        zerolinewidth=2
    ),
    plot_bgcolor=colors['bg'],
    paper_bgcolor='#F8F9FA',
    hovermode='x unified',
    legend=dict(
        x=0.98,
        y=0.98,
        xanchor='right',
        yanchor='top',
        bgcolor='rgba(255, 255, 255, 0.9)',
        bordercolor='#34495E',
        borderwidth=2,
        font=dict(size=12, color='#2C3E50', family='Arial, sans-serif')
    ),
    width=1200,
    height=700,
    margin=dict(l=80, r=80, t=120, b=80)
)

# Add annotations
fig.add_annotation(
    text='<i>Uncertainty bands represent variation across KDUQ optical potential parameters</i>',
    xref='paper', yref='paper',
    x=0.5, y=-0.12,
    showarrow=False,
    font=dict(size=11, color='#7F8C8D', family='Arial, sans-serif'),
    xanchor='center'
)

# Save as HTML
output_html = 'cross_section_plot.html'
fig.write_html(output_html)
print(f"✓ Interactive plot saved to: {output_html}")

# Also save as static image (requires kaleido)
try:
    output_png = 'cross_section_plot.png'
    fig.write_image(output_png, width=1200, height=700, scale=2)
    print(f"✓ High-resolution PNG saved to: {output_png}")
except Exception as e:
    print(f"Note: PNG export requires 'kaleido' package: pip install kaleido")
    print(f"  (HTML file is fully interactive and can be opened in any browser)")

# Show in browser
fig.show()

print("\n" + "="*60)
print("PLOT SUMMARY")
print("="*60)
print(f"Angular range:     0° to 180° ({len(angle)} points)")
print(f"Cross section:     {mean.min():.4f} to {mean.max():.4f} mb/sr")
print(f"Max uncertainty:   ±{(plus1sigma - mean).max():.4f} mb/sr (1σ)")
print(f"                   ±{(plus2sigma - mean).max():.4f} mb/sr (2σ)")
print(f"Peak angle:        {angle[np.argmax(mean)]:.1f}°")
print(f"Peak cross section: {mean.max():.4f} mb/sr")
print("="*60)
