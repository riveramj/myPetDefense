newStarts = document.getElementById("new-starts-chart").getContext('2d')

newStartsChart = new Chart(newStarts, {
  type: 'pie',
  data: {
    labels: ["YTD", "MTD", "Today"],
    datasets: [{
      label: '# of Customers',
      data: [],
      backgroundColor: [
        '#cc0069',
        '#0092d1',
        '#1f1d65',
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
        display: (context) ->
          context.dataset.data[context.dataIndex] != 0
        formatter: (value, context) ->
          value + '%'
        color: [
          '#000000',
          '#000000',
          '#ffffff',
        ],
        font: {
          size: 18
        }
      }
    }
  }
})

$(document).on "update-chart-data", (event) ->
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