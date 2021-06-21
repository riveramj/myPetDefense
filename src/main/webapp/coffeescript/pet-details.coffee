$(document).ready ->
  $("#dog").on "click", (event) ->
    $('.cat-choice').remove()
    $('.introduction').remove()
    $('.dog-choice').addClass('selected')

  $("#cat").on "click", (event) ->
    $('.dog-choice').remove()
    $('.introduction').remove()
    $('.cat-choice').addClass('selected')

  $("body").on "click", '#choose-care', (event) ->
    $(".validation-error").remove()
    $(".error").removeClass("error")

$(document).on "show-plans", (event) ->
  $('.dog-choice').addClass('details-complete').removeClass('selected')
  $('.cat-choice').addClass('details-complete').removeClass('selected')
  $('#content-container').addClass('choosing-plan')
  $('.pet-details').addClass('choosing-plan')
