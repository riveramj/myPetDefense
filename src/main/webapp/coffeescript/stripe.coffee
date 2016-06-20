window.myPetDefenseSite =
  event: (eventName, parameters) ->
    event = jQuery.Event(eventName, parameters)
    jQuery(document).trigger event

stripeCallback = (status, response) ->
  if response.error
    console.log response.error + " errror"
  else
    $("#stripe-token").val(response.id)
    $("#checkout").submit()

$(document).ready ->
  Stripe?.setPublishableKey? 'pk_test_JLczczIy7T5qGL8DOmwTc2O0'

  $("body").on "click", '#checkout .checkout', (event) ->

    myPetDefenseSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

$(document).on "validate-stripe-form", (event) ->

  Stripe.createToken
    number: $('#card-number').val(),
    cvc: $('#card-cvc').val(),
    exp_month: $('#card-month-expiry').val(),
    exp_year: $('#card-year-expiry').val(),
    address_zip: $("#zip").val()
  , event.stripeCallback
