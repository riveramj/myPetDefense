$(document).ready ->
  $(".cancel-account").on "click", (event) ->
    $(".cancel-confirm").addClass("active")

  $(".cancel-confirm .close").on "click", (event) ->
    $(".cancel-confirm").removeClass("active")
