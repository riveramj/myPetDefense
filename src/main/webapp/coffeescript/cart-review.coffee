$(document).ready ->
  $.getJSON('https://api.ipify.org?format=json', (data) ->
    $(".ip-address").val(data.ip)
    $(".ip-address").blur()
  )
