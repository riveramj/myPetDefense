$(document).ready ->
  $(".expand-details").on "click", (event) ->
    event.preventDefault()

    $(this).closest('.choice-container').toggleClass('collapsed').toggleClass('expanded')
