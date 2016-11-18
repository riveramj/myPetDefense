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

  $("body").on "click", '.submit', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")
