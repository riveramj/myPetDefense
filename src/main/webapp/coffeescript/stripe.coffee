window.myPetDefenseSite =
  event: (eventName, parameters) ->
    event = jQuery.Event(eventName, parameters)
    jQuery(document).trigger event

  validationError: (fieldSelector, error) ->
    myPetDefenseSite.event 'form-validation-error',
      fieldSelector: fieldSelector,
      error: error

stripeCallback = (status, response) ->
  console.log "hi"
  if response.error
    console.log response.error + " errror"
    myPetDefenseSite.validationError("#card-number", response.error.message)
  else
    $("#stripe-token").val(response.id)
    $("#checkout").submit()

$(document).ready ->
  Stripe?.setPublishableKey? 'pk_test_JLczczIy7T5qGL8DOmwTc2O0'

  myPetDefenseSite.event("stripe-form-ready")

  $("body").on "click", '.checkout', (event) ->
    console.log "1"
    event.preventDefault()
    console.log "2"

    myPetDefenseSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

$(document).on 'stripe-form-ready', (event) ->
  $('#card-number').payment('formatCardNumber')
  $('#card-expiry').payment('formatCardExpiry')
  $("#card-cvc").payment('formatCardCVC')

$(document).on "validate-stripe-form", (event) ->
  console.log "nope"
  $(".validation-error").remove()
  $("input.error").removeClass("error")

  validationError = false

  unless $.payment.validateCardNumber($("#card-number").val())
    myPetDefenseSite.validationError("#card-number", "Invalid card number.")
    validationError = true

  unless $.payment.validateCardCVC($("#card-cvc").val())
    myPetDefenseSite.validationError("#card-cvc", "Invalid CVC.")
    validationError = true

  cardExpiry = $("#card-expiry").payment('cardExpiryVal')

  unless $.payment.validateCardExpiry(cardExpiry.month, cardExpiry.year)
    myPetDefenseSite.validationError("#card-expiry", "Invalid card expiration.")
    validationError = true

  unless validationError
    Stripe.createToken
      number: $('#card-number').val(),
      cvc: $('#card-cvc').val(),
      exp_month: cardExpiry.month,
      exp_year: cardExpiry.year,
      address_zip: $("#zip").val()
    , event.stripeCallback


