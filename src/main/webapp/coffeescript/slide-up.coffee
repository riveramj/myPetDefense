$(document).ready ->
  $('button.missing-product, .sliding-panel-fade-screen, .sliding-panel-close, .sliding-panel-content').on 'click touchstart', (e) ->
    $('.sliding-panel-content,.sliding-panel-fade-screen').toggleClass 'is-visible'
    e.preventDefault()
    return
  return
