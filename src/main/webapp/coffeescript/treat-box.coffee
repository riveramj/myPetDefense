$(document).ready ->
  $('#main').on 'click', '.mobile-close, .continue-shopping, .continue-checkout', (event) ->
    $('#shopping-cart').removeClass('opened')

  $('#main').on 'click', '.add-to-cart button, .open-cart', (event) ->
    $('#shopping-cart').addClass('opened')

  $('#main').on 'click', '.mobile-close, .continue-shopping', (event) ->
    $('#shopping-cart').removeClass('opened')
    $('.pet-name').val('')
    $('.product-choice').prop("checked", false)
    $('.product').removeClass('selected')
