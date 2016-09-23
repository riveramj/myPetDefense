$(document).ready ->
  $(".expand-details").on "click", (event) ->
    event.preventDefault()

    $(this).toggleClass('expanded')
    $('.expand-details').closest('.choice-container').find('.product-features').toggleClass('hidden')
