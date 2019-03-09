stripe = Stripe 'pk_test_JLczczIy7T5qGL8DOmwTc2O0'
elements = stripe.elements()
card = elements.create('card',{
  'style': {
    'base': {
      'fontSize': '17px',
      'fontFamily': '"Lato", Helvetica, Arial, sans-serif'
    }
  }
})

newCard = false

if !$('.form-row').hasClass('hide-card')
  card.mount '#card-element'
  newCard = true
  
stripeCallback = (token) ->
  $("#stripe-token").val(token.id)
  $(".checkout, .update-billing").submit()

$(document).ready ->
  $("body").on "click", '.checkout, .update-billing', (event) ->
    event.preventDefault()
    $(".checkout.submit, input.update-billing").prop('value', 'Please Wait')
    $(".checkout.submit, input.update-billing").prop('value', 'Please Wait').prop("disabled", true).addClass("processing")

    myPetDefenseSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

  $(document).on "use-new-card", (event) ->
    $('.form-row').removeClass('hide-card')
    $('.existing-card').remove()
    card.mount '#card-element'
    newCard = true

$(document).on "form-validation-error", (event) ->
  $(".checkout.submit, input.update-billing").prop('value', 'Place Order').prop("disabled", false).removeClass("processing")

$(document).on "validate-stripe-form", (event) ->
  $(".validation-error").remove()
  $("input.error").removeClass("error")

  if newCard
    stripe.createToken(card).then((result) ->
      if (result.error)
        # Inform the user if there was an error
        $('#card-errors').val(result.error.message)
        $(".checkout.submit, input.update-billing").prop('value', 'Place Order').prop("disabled", false).removeClass("processing")
      else
        # Send the token to your server
        event.stripeCallback(result.token)
    )
  else
    event.stripeCallback("")
