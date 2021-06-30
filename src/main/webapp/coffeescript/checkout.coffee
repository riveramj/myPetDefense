$(document).ready ->
  monthlyTotal = $("#monthly-total span").text().substring(1)

  # fbq('track', 'InitiateCheckout', {currency: "USD", value: monthlyTotal})