#!/bin/bash
# QDUC Web App Launcher

echo "╔════════════════════════════════════════════════════════════╗"
echo "║         QDUC Nuclear Reaction Calculator - Web App         ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Check if Flask is installed
if ! python3.11 -c "import flask" 2>/dev/null; then
    echo "⚠️  Flask not found. Installing..."
    pip3 install flask
    echo ""
fi

# Start the server
echo "🚀 Starting QDUC web server..."
echo ""
echo "📱 Open your browser to: http://localhost:5000"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""
echo "─────────────────────────────────────────────────────────────"
echo ""

python3.11 qduc_server.py
