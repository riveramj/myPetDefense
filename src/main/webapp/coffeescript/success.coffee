$(document).ready ->
  monthlyTotal = $("#monthly-total span").text().substring(1)

  # fbq('track', 'Purchase', {currency: "USD", value: monthlyTotal})