$(document).ready ->
  $(".go-back").on "click", (event) ->
    window.history.back()

  $("body").on "click", '.cancel', (event) ->
    $(".cancel").prop('value', 'Please Wait').prop("disabled", true).addClass("processing")
    $(".cancel").submit()
