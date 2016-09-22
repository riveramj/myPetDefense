$(document).ready ->

  $("body").on "click", '#create-item', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")

  $("body").on "click", '.create-item', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")

  $("body").on "click", '.parent', (event) ->
    $(this).next('tr').toggleClass('collapsed')
    
