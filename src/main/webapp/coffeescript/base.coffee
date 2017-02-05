window.myPetDefenseSite =
  event: (eventName, parameters) ->
    event = jQuery.Event(eventName, parameters)
    jQuery(document).trigger event

  validationError: (fieldSelector, error) ->
    myPetDefenseSite.event 'form-validation-error',
      fieldSelector: fieldSelector,
      error: error

$(document).on 'form-validation-error', (event) ->
  $target = $(event.fieldSelector)
  $targetContainer = $target.closest('label').find('.field-label')

  $target.addClass "error"
  $targetContainer.append $("<div />").addClass("validation-error").text(event.error)

$(document).on 'help-message-sent', (event) ->
  $('.message-sent').removeClass("hidden")
  $('.name, .email, .message').val("")
  
$("body").on "click", '.submit, #send-message', (event) ->
  $(".validation-error").remove()
  $("input.error").removeClass("error")

$('form.current-step input').on 'keypress', (e) ->
  e.keyCode != 13

$(document).ready ->
  $("input, textarea").on "focus", (event) ->
    $target = $(event.target)
    $target.removeClass("error").parent().find(".validation-error").remove()
