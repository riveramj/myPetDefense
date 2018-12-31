activeInactive = document.getElementById("active-inactive-chart").getContext('2d')

activeInactiveChart = new Chart(activeInactive, {
  type: 'pie',
  data: {
    labels: ["Active", "Inactive"],
    datasets: [{
      label: '# of Customers',
      data: [],
      backgroundColor: [
        '#B3E7E6',
        '#E7E7E7'
      ]
    }]
  },
  options: {
    responsive:true,
    maintainAspectRatio: false,
    aspectRatio: 1,
    title: {
      display: true,
      text: 'Total Users'
    },
    legend: {
      position: 'left'
    }
  }
})

totalActive = document.getElementById("total-active-chart").getContext('2d')
totalActiveChart = new Chart(totalActive, {
  type: 'line',
  data: {
    labels: [],
    datasets: [{
      fill: false,
      lineTension: 0,
      label: '# of Customers',
      data: [0,0,0,0,0,0],
      pointBackgroundColor: '#259490',
      borderColor: '#B3E7E6'
    }]
  },
  options: {
    responsive: true,
    maintainAspectRatio: false,
    legend: {
      display: false
    },
    title: {
      display: true,
      text: 'Total Active Users'
    },
    scales: {
      yAxes: [{
        ticks: {
          beginAtZero: true
        }
      }]
    }
  }
})

signup = document.getElementById("signup-chart").getContext('2d')
signupChart = new Chart(signup, {
  type: 'line',
  data: {
    labels: [],
    datasets: [{
      fill: false,
      lineTension: 0,
      label: '# of Customers',
      data: [0,0,0,0,0,0],
      pointBackgroundColor: '#259490',
      borderColor: '#B3E7E6'
    }]
  },
  options: {
    responsive: true,
    maintainAspectRatio: false,
    legend: {
      display: false
    },
    title: {
      display: true,
      text: 'New User Signups'
    },
    scales: {
      yAxes: [{
        ticks: {
          beginAtZero: true
        }
      }]
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
    when "activeInactive"
      activeInactiveChart.data.datasets[0].data = newData
      activeInactiveChart.update()
    
    when "totalActive"
      totalActiveChart.data.datasets[0].data = newData
      totalActiveChart.data.labels = newLabels
      totalActiveChart.update()

    when "signup"
      console.log "in signup"
      signupChart.data.datasets[0].data = newData
      signupChart.data.labels = newLabels
      signupChart.update()

$('.update-data').click()
