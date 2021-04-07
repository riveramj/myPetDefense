$(document).ready ->
  monthlyTotal = $("#monthly-total span").text().substring(1)

  $.getJSON('https://api.ipify.org?format=json', (data) ->
    $("#ip-address").val(data.ip)
  )

  # fbq('track', 'InitiateCheckout', {currency: "USD", value: monthlyTotal})