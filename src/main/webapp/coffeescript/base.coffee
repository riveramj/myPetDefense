window.myPetDefenseSite =
  event: (eventName, parameters) ->
    event = jQuery.Event(eventName, parameters)
    jQuery(document).trigger event

  validationError: (fieldSelector, error) ->
    myPetDefenseSite.event 'form-validation-error',
      fieldSelector: fieldSelector,
      error: error

  errorMessage: (fieldSelector, error) ->
    myPetDefenseSite.event 'error-message',
      fieldSelector: fieldSelector,
      error: error

# Alias setTimeout. nextTick is so we're clear when we're using a
# setTimeout just to delay an operation one tick.
window.myPetDefenseSite.nextTick = (fn) -> setTimeout fn

$(document).on 'form-validation-error', (event) ->
  $target = $(event.fieldSelector)
  $targetContainer = $target.closest('label').find('.field-label')

  $target.addClass "error"
  $targetContainer.append $("<div />").addClass("validation-error").text(event.error)

$(document).on 'error-message', (event) ->
  $(event.fieldSelector).text(event.error)

$(document).on 'testimonial-sent', (event) ->
  $('.testimonial-sent').removeClass("hidden")
  
$("body").on "click", '.submit, #send-message', (event) ->
  $(".validation-error").remove()
  $("input.error").removeClass("error")

$('form.current-step input').on 'keypress', (e) ->
  e.keyCode != 13

$(".close").on "click", (event) ->
  $(".modal").removeClass "active"

$(".modal").on "click", (event) ->
  if ($(event.target).is($(".modal")))
    $(".modal").removeClass "active"

$(document).on 'keyup', (event) ->
  if (event.keyCode == 27)
    $(".modal").removeClass "active"
    $(document).unbind 'keyup'

$(document).ready ->
  $("input, textarea").on "focus", (event) ->
    $target = $(event.target)
    $target.removeClass("error").parent().find(".validation-error").remove()

  $("body").on "click", '.dog-sizes li', (event) ->
    dogSize = $(event.target).closest('li')
    
    if !dogSize.hasClass('selected')
      $('.dog-sizes li').removeClass('selected')
      dogSize.addClass('selected')
      dogSize.find('input').click()
