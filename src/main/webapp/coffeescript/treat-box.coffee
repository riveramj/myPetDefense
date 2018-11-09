$(document).ready ->
  $('#main').on 'click', '.mobile-close, .continue-shopping', (event) ->
    $('#shopping-cart').removeClass('opened')
    $('.pet-name').val('')
    $('.product-choice').prop("checked", false)
    $('.product').removeClass('selected')

  $('#main').on 'click', '.add-to-cart button', (event) ->
    $('#shopping-cart').addClass('opened')

  $('#main').on 'click', '.mobile-close, .continue-shopping', (event) ->
    $('#shopping-cart').removeClass('opened')
    $('.pet-name').val('')
    $('.product-choice').prop("checked", false)
    $('.product').removeClass('selected')
