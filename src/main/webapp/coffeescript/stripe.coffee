stripeCallback = (status, response) ->
  if response.error
    myPetDefenseSite.validationError("#card-number", response.error.message)
  else
    $("#stripe-token").val(response.id)
    $(".checkout").submit()

$(document).ready ->
  Stripe?.setPublishableKey? 'pk_test_tlQUPJdasEEdNTcUsbcsHmG7'

  myPetDefenseSite.event("stripe-form-ready")

  $("body").on "click", '.checkout', (event) ->
    event.preventDefault()

    myPetDefenseSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

$(document).on 'stripe-form-ready', (event) ->
  $('#card-number').payment('formatCardNumber')
  $('#card-expiry').payment('formatCardExpiry')
  $("#card-cvc").payment('formatCardCVC')

$(document).on "validate-stripe-form", (event) ->
  $(".validation-error").remove()
  $("input.error").removeClass("error")
  
  validationError = false

  unless $.payment.validateCardNumber($("#card-number").val())
    myPetDefenseSite.validationError("#card-number", "Invalid")
    validationError = false
  
  unless $.payment.validateCardCVC($("#card-cvc").val())
    myPetDefenseSite.validationError("#card-cvc", "Invalid")
    validationError = true
  
  cardExpiry = $("#card-expiry").payment('cardExpiryVal')
  
  unless $.payment.validateCardExpiry(cardExpiry.month, cardExpiry.year)
    myPetDefenseSite.validationError("#card-expiry", "Invalid")
    validationError = true
  
  unless validationError
    Stripe.createToken
      name: $('#cardholder-name').val(),
      number: $('#card-number').val(),
      cvc: $('#card-cvc').val(),
      exp_month: cardExpiry.month,
      exp_year: cardExpiry.year,
      address_zip: $("#zip").val()
    , event.stripeCallback

