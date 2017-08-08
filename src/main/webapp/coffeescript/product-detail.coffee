$(document).ready ->
  $('.product-slider').slick({
    dots: true,
    lazyLoad: 'ondemand',
    slidesToScroll: 1
  })

  $("#price-list").on "click", ".product", (event) ->
    $(".selected").removeClass("selected")
    $(event.target).closest('.product').addClass("selected")

  $('.add-to-cart').on 'click', (event) ->
    $('#shopping-cart').addClass('opened')

  $('#main').on 'click', '.mobile-close, .continue-shopping', (event) ->
    $('#shopping-cart').removeClass('opened')
    $('.pet-name').val('')
    $('.product').removeClass('selected')

