

$('.product-small-images').slick({
  slidesToShow: 1,
  slidesToScroll: 1,
  dots: false,
  focusOnSelect: true,
  arrows: false
})

$(document).ready ->
  $('#main').on 'click', '.mobile-close, .continue-shopping', (event) ->
    $('#shopping-cart').removeClass('opened')
    $('.pet-name').val('')
    $('.product-choice').prop("checked", false)
    $('.product').removeClass('selected')

