$(document).ready ->
  $("#dog").on "click", (event) ->
    $('.cat-choice').remove()
    $('.introduction').remove()
    $('.dog-choice').addClass('selected')

  $("#cat").on "click", (event) ->
    $('.dog-choice').remove()
    $('.introduction').remove()
    $('.cat-choice').addClass('selected')
