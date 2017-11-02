stripeCallback = (status, response) ->
  if response.error
    myPetDefenseSite.validationError("#card-number", response.error.message)
  else
    $("#stripe-token").val(response.id)
    $(".checkout").submit()

window.myPetDefenseSite.groupon = -> false
    
$(document).ready ->
  Stripe?.setPublishableKey? 'pk_test_JLczczIy7T5qGL8DOmwTc2O0'

  myPetDefenseSite.event("stripe-form-ready")

  $("body").on "click", '.checkout', (event) ->
    event.preventDefault()

    myPetDefenseSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

$(document).on 'groupon-only', (event) ->
  console.log "got it"
  console.log window.myPetDefenseSite.groupon ->
  window.myPetDefenseSite.groupon = -> true
  console.log window.myPetDefenseSite.groupon ->

$(document).on 'stripe-form-ready', (event) ->
  $('#card-number').payment('formatCardNumber')
  $('#card-expiry').payment('formatCardExpiry')
  $("#card-cvc").payment('formatCardCVC')

$(document).on "validate-stripe-form", (event) ->
  console.log myPetDefenseSite.groupon ->

  if (window.myPetDefenseSite.groupon ->)
    console.log "in groupon"
    $(".checkout").submit()
    return false

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

