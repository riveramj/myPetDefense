$(document).ready ->
  $("body").on "click", '#login', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")
