$(document).ready ->
  $('.product-slider').slick({
    dots: true,
    lazyLoad: 'ondemand',
    slidesToScroll: 1
  })

  $(document).on 'form-validation-error', (event) ->
    $target = $(event.fieldSelector)
    $targetContainer = $target
    $target.addClass "error"

  $("#price-list").on "click", ".product", (event) ->
    $('.product').removeClass('error')
    $(".selected").removeClass("selected")
    product = $(event.target).closest('.product')
    $(product).addClass("selected")
    $(product).find('.product-choice').prop("checked", true)

  $('.add-to-cart').on 'click', (event) ->
    if $('.product-choice').is(':checked')
      $('#shopping-cart').addClass('opened')

  $('#main').on 'click', '.mobile-close, .continue-shopping', (event) ->
    $('#shopping-cart').removeClass('opened')
    $('.pet-name').val('')
    $('.product-choice').prop("checked", false)
    $('.product').removeClass('selected')
