$(document).ready ->
  $message = $('.sliding-panel-content, .sliding-panel-fade-screen')

  $('button.missing-product').on 'touchstart click', (e) ->
    e.stopPropagation()
    e.preventDefault()

    $message.addClass 'is-visible'

  $message.on 'touchstart click', (e) ->
    e.stopPropagation()
    e.preventDefault()

    $message.removeClass 'is-visible'
