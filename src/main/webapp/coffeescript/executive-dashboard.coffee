newStarts = document.getElementById("new-starts-chart").getContext('2d')

newStartsChart = new Chart(newStarts, {
  type: 'pie',
  data: {
    labels: ["Today", "MTD", "YTD"],
    datasets: [{
      label: '# of Customers',
      data: [],
      backgroundColor: [
        '#1f1d65',
        '#0092d1',
        '#cc0069'
      ]
    }]
  },
  options: {
    responsive:true,
    maintainAspectRatio: false,
    aspectRatio: 1,
    legend: {
      display: false
    },
    plugins: {
      datalabels: {
        formatter: (value, context) ->
          value + '%'
        color: [
          '#ffffff',
          '#000000',
          '#000000'
        ],
        font: {
          size: 18
        }
      }
    }
  }
})

$(document).on "update-chart-data", (event) ->
  console.log "got it"

  chartName = event.chartName
  newData = event.newData
  newLabels = event.newLabels

  updateData(chartName, newData, newLabels)

updateData = (chartName, newData, newLabels) ->
  switch chartName
    when "newStarts"
      newStartsChart.data.datasets[0].data = newData
      newStartsChart.update()

$('.update-data').click()