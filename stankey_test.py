
from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot
import plotly.graph_objs as go
import json
from urllib.request import urlopen

url = 'https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy.json'
response = urlopen(url)
data = json.loads(response.read())


data_trace = dict(
    type='sankey',
    width = 1118,
    height = 772,
    domain = dict(
      x =  [0,1],
      y =  [0,1]
    ),
    orientation = "h",
    valueformat = ".0f",
    valuesuffix = "TWh",
    node = dict(
      pad = 15,
      thickness = 15,
      line = dict(
        color = "black",
        width = 0.5
      ),
      label =  data['data'][0]['node']['label'],
      color =  data['data'][0]['node']['color']
    ),
    link = dict(
      source =  data['data'][0]['link']['source'],
      target =  data['data'][0]['link']['target'],
      value =  data['data'][0]['link']['value'],
      label =  data['data'][0]['link']['label']
  ))

layout =  dict(
    title = "Energy forecast for 2050<br>Source: Department of Energy & Climate Change, Tom Counsell via <a href='https://bost.ocks.org/mike/sankey/'>Mike Bostock</a>",
    font = dict(
      size = 10
    )
)

fig = dict(data=[data_trace], layout=layout)
plot(fig, validate=False)

'''
#Manual test of what the data might look like...
data = dict(
    type='sankey',
    node = dict(
      pad = 15,
      thickness = 20,
      line = dict(
        color = "black",
        width = 0.5
      ),
      label = ["Enrolled Y0", "Enrolled With Scholarship Y0", 
              "Enrolled Y2", "Enrolled With Scholarship Y2", "Not Enrolled Y2", "Degree Y2",
              "Enrolled Y4", "Enrolled With Scholarship Y4", "Not Enrolled Y4", "Degree Y4"],
      color = ["blue", "orange", "blue", "orange", "red", "green", "blue", "orange", "red", "green"]
    ),
    link = dict(
      source = [30,31,0,1,1, 2,2,4,3,4],
      target = [2,3,4,4,2, 9,8,8,9,8],
      value = [9,4,2,1,1, 7,3,1,4,2],
  ))

layout =  dict(
    title = "Rough Draft of Scholarship Impact",
    font = dict(
      size = 10
    )
)

fig = dict(data=[data], layout=layout)
plot(fig)
'''