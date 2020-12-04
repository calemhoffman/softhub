# ~ import pandas as pd

# ~ data = pd.read_csv('output_list.txt', header = None)
# ~ print data

# ~ import pandas as pd
# ~ import plotly.express as px

# ~ df = pd.read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_apple_stock.csv')

# ~ fig = px.line(df, x = 'AAPL_x', y = 'AAPL_y', title='Apple Share Prices over time (2014)')
# ~ fig.show()

# ~ import dash
# ~ import dash_core_components as dcc
# ~ import dash_html_components as html
# ~ from dash.dependencies import Input, Output
# ~ import plotly.express as px
# ~ import pandas as pd

# ~ df = pd.read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_apple_stock.csv')

# ~ app = dash.Dash(__name__)

# ~ app.layout = html.Div([
    # ~ dcc.Graph(id="graph"),
    # ~ html.Button("Switch Axis", id='btn', n_clicks=0)
# ~ ])

# ~ @app.callback(
    # ~ Output("graph", "figure"), 
    # ~ [Input("btn", "n_clicks")])
# ~ def display_graph(n_clicks):
    # ~ if n_clicks % 2 == 0:
        # ~ x, y = 'AAPL_x', 'AAPL_y'
    # ~ else:
        # ~ x, y = 'AAPL_y', 'AAPL_x'

    # ~ fig = px.line(df, x=x, y=y)    
    # ~ return fig

# ~ app.run_server(debug=True)

