$(document).on "order-submitted", (event) ->
  #$('.parent-email').val(event.email)
  $('.success-popover').addClass('success')

$(document).ready ->
  $("body").on "click", '.new-order', (event) ->
    $('.success-popover').removeClass('success')
    location.reload()

  $("body").on "click", '.continue-shopping', (event) ->
    window.scrollTo(0, 0)
