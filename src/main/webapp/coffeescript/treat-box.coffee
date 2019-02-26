$(document).ready ->
  $('#main').on 'click', '.desktop-close, .continue-shopping, .continue-checkout', (event) ->
    $('#shopping-cart').removeClass('opened')

  $('#main').on 'click', '.add-treat, .open-cart', (event) ->
    $('#shopping-cart').addClass('opened')

  $('#main').on 'click', '.desktop-close, .continue-shopping', (event) ->
    $('#shopping-cart').removeClass('opened')
    $('.pet-name').val('')
    $('.product-choice').prop("checked", false)
    $('.product').removeClass('selected')

  $('#main').on 'click', '.continue-as-guest', (event) ->
    $('.login-popover-container').remove()

  $('#main').on 'click', '.login-popover', (event) ->
    if (event.target.closest('.guest-log-in, .guest-continue') == null)
      $('.login-popover-container').remove()

