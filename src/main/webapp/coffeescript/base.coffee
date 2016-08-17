window.myPetDefenseSite =
  event: (eventName, parameters) ->
    event = jQuery.Event(eventName, parameters)
    jQuery(document).trigger event

  validationError: (fieldSelector, error) ->
    myPetDefenseSite.event 'form-validation-error',
      fieldSelector: fieldSelector,
      error: error
