$(document).ready ->
  $("body").on "click", '#signup', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")
