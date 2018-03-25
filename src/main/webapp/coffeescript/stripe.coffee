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

window.myPetDefenseSite.groupon = -> false
    
$(document).ready ->
  $("body").on "click", '.checkout, .update-billing', (event) ->
    event.preventDefault()
    $(".checkout.submit").prop('value', 'Please Wait')
    $(".checkout.submit").prop('value', 'Please Wait').prop("disabled", true).addClass("processing")

    myPetDefenseSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

$(document).on 'groupon-only', (event) ->
  window.myPetDefenseSite.groupon = -> true

$(document).on "form-validation-error", (event) ->
  $(".checkout.submit").prop('value', 'Place Order').prop("disabled", false).removeClass("processing")

$(document).on "validate-stripe-form", (event) ->
  if (window.myPetDefenseSite.groupon ->)
    $(".checkout").submit()
    return false

  $(".validation-error").remove()
  $("input.error").removeClass("error")

  stripe.createToken(card).then((result) ->
    if (result.error)
      # Inform the user if there was an error
      $('#card-errors').val(result.error.message)
      $(".checkout.submit").prop('value', 'Place Order').prop("disabled", false).removeClass("processing")
    else
      # Send the token to your server
      event.stripeCallback(result.token)
  )
