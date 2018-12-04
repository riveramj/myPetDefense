activeInactive = document.getElementById("active-inactive-chart").getContext('2d')

activeInactiveChart = new Chart(activeInactive, {
  type: 'pie',
  data: {
    labels: ["Active", "Inactive"],
    datasets: [{
      label: '# of Customers',
      data: [100, 19],
      backgroundColor: [
        'rgba(255, 99, 132, 0.2)',
        'rgba(54, 162, 235, 0.2)'
     ]
    }]
  },
  options: {
    responsive:true,
    maintainAspectRatio: false,
    aspectRatio: 1
  }
})

mtdCommission = document.getElementById("total-active-chart").getContext('2d')
mtdCommissionChart = new Chart(mtdCommission, {
  type: 'bar',
  data: {
    labels: ["Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"],
    datasets: [{
      label: '# of Customers',
      data: [40, 29, 30, 50, 60, 50, 40, 41, 43, 49, 35, 42],
      backgroundColor: [
        'rgba(255, 99, 132, 0.2)',
        'rgba(54, 162, 235, 0.2)'
     ]
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
    }
  }
})
ytdCommission = document.getElementById("signup-chart").getContext('2d')
ytdCommissionChart = new Chart(ytdCommission, {
  type: 'line',
  data: {
    labels: ["Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"],
    datasets: [{
      fill: false,
      lineTension: 0,
      label: '# of Customers',
      data: [40, 29, 30, 50, 60, 50, 40, 41, 43, 49, 35, 42],
      pointBackgroundColor: 'rgb(255,0,0)',
      borderColor: 'rgb(0,0,255)'
    }]
  },
  options: {
    responsive:true,
    maintainAspectRatio: false,
    legend: {
      display: false
    },
    title: {
      display: true,
      text: 'New User Signups'
    }
  }
})
