$(document).ready ->

  $("body").on "click", '#create-item', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")
