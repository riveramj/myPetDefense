$(document).ready ->
  $message = $('.sliding-panel-content, .sliding-panel-fade-screen')

  $('button.missing-product').on 'click, touchstart', (e) ->
    $message.addClass 'is-visible'

  $message.on 'click, touchstart', (e) ->
    $message.removeClass 'is-visible'
