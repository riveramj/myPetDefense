stripe = Stripe 'pk_test_JLczczIy7T5qGL8DOmwTc2O0'
elements = stripe.elements()
card = elements.create 'card'
card.mount '#card-element'

stripeCallback = (token) ->
  console.log "in callback"
  console.log token
  $("#stripe-token").val(token.id)
  $(".checkout").submit()

window.myPetDefenseSite.groupon = -> false
    
$(document).ready ->
  $("body").on "click", '.checkout', (event) ->
    event.preventDefault()

    myPetDefenseSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

$(document).on 'groupon-only', (event) ->
  window.myPetDefenseSite.groupon = -> true

$(document).on "validate-stripe-form", (event) ->
  if (window.myPetDefenseSite.groupon ->)
    $(".checkout").submit()
    return false

  $(".validation-error").remove()
  $("input.error").removeClass("error")

  stripe.createToken(card).then((result) ->
    console.log 'foo'
    if (result.error)
      # Inform the user if there was an error
      console.log result.error.message
      $('#card-errors').val(result.error.message)
    else
      console.log event.stripeCallback
      # Send the token to your server
      event.stripeCallback(result.token)
  )
