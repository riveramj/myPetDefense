stripe = Stripe 'pk_live_hR6ryn2PwyeLHsRVTPX5EEol'
elements = stripe.elements()
card = elements.create('card',{
  'style': {
    'base': {
      'fontSize': '17px',
      'fontFamily': '"Lato", Helvetica, Arial, sans-serif'
    }
  }
})

card.mount '#card-element'

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

$(document).on "form-validation-error", (event) ->
  $(".checkout.submit, input.update-billing").prop('value', 'Place Order').prop("disabled", false).removeClass("processing")

$(document).on "validate-stripe-form", (event) ->
  $(".validation-error").remove()
  $("input.error").removeClass("error")

  stripe.createToken(card).then((result) ->
    if (result.error)
      # Inform the user if there was an error
      $('#card-errors').val(result.error.message)
      $(".checkout.submit, input.update-billing").prop('value', 'Place Order').prop("disabled", false).removeClass("processing")
    else
      # Send the token to your server
      event.stripeCallback(result.token)
  )
