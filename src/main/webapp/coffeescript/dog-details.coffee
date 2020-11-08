$(document).ready ->
  fbq('track', 'ViewContent', {content: "DogDetails"})

  $("#next-actions").on "click", '#add-pet', (event) ->
    dogSize = $(".dog-sizes li.selected .weight-number").text()

    dogValue = switch dogSize
      when "5-22 lb" then 24.99
      when "" then 0.00
      else 27.99

    dogSizeName = switch dogSize
      when "5-22 lb" then "Small Dog"
      when "23-44 lb" then "Medium Dog"
      when "45-88 lb" then "Large Dog"
      when "89-132 lb" then "XLarge Dog"
      else ""

    if dogValue > 0
      fbq('track', 'AddToCart', {currency: "USD", value: dogValue, content_name: dogSizeName})