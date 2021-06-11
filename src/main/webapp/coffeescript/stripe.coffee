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

cartItems = []
total = {
  label: 'Due Today',
  amount: 0
}

newCard = false
updating = false
timeout = 10000

if window.location.pathname == "/checkout"
  if !$('.form-row').hasClass('hide-card')
    card.mount '#card-element'
    newCard = true

paymentRequest = stripe.paymentRequest({
  country: 'US',
  currency: 'usd',
  displayItems: cartItems,
  total: total,
  requestPayerName: true,
  requestPayerEmail: true,
  requestPayerPhone: true,
  requestShipping: true,
  shippingOptions: [
    {
      id: 'free-shipping',
      label: 'Free shipping',
      detail: 'Arrives in 4 to 6 days',
      amount: 0,
    },
  ],
})

elements = stripe.elements()
prButton = elements.create('paymentRequestButton', {
  paymentRequest: paymentRequest,
})

paymentRequest.on('shippingaddresschange', (ev) ->
  if (ev.shippingAddress.country != 'US')
    ev.updateWith({status: 'invalid_shipping_address'})
  else
    updating = true
    updateBillingAmount(JSON.stringify ev.shippingAddress)

    listenForServerUpdate(timeout).then(->
      console.log "promise resolved"
      updating = false

      ev.updateWith({
        status: 'success',
        displayItems: cartItems,
        total: total
      })
    )
)

paymentRequest.canMakePayment().then((result) ->
  if (result)
    prButton.mount('#payment-request-button')
  else
    document.getElementById('payment-request-button').style.display = 'none'
)
  
stripeCallback = (token) ->
  $("#stripe-token").val(token.id)
  $(".checkout, .update-billing, #create-customer").submit()

prButton. on "click", (event) ->
  paymentRequest.update({
    total: total,
    displayItems: cartItems,
  })

listenForServerUpdate = (timeout) ->
  console.log "start promise"
  start = Date.now()

  return new Promise (resolve, reject) ->
    console.log "top level"
    
    checkingServerUpdate = (resolve, reject) ->
      console.log "checking promise"

      if (updating)
        console.log "promise good"
        resolve "it worked"
      else if (timeout && (Date.now() - start) >= timeout)
        console.log "promise fail"
        reject  Error "timeout"
      else
        console.log "promise waiting"
        setTimeout(checkingServerUpdate.bind(this, resolve, reject), 2)


$(document).ready ->
  $("body").on "click", '.checkout, .update-billing, #create-customer', (event) ->
    event.preventDefault()
    $(".checkout.submit, input.update-billing, .create-customer").prop('value', 'Please Wait')
    $(".checkout.submit, input.update-billing, .create-customer").prop('value', 'Please Wait').prop("disabled", true).addClass("processing")

    myPetDefenseSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

$(document).on "update-cart-items", (event) ->
  console.log("in event")
  console.log(event)
  console.log("in event")

  updating = true
  cartItems = event.items
  total = event.total

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
      $('#card-errors').first().text("")
      $('#card-errors').removeClass('active')

      if (result.error)
        # Inform the user if there was an error
        $('#card-errors').val(result.error.message)
        $(".checkout.submit, input.update-billing").prop('value', 'Place Order').prop("disabled", false).removeClass("processing")
      else if (result.token.card.funding == "prepaid")
        $('#card-errors').addClass('active')
        $('#card-errors').first().text("Prepaid cards are not supported at this time. Please use another card.")

        $(".checkout.submit, input.update-billing").prop('value', 'Place Order').prop("disabled", false).removeClass("processing")
      else
        # Send the token to your server
        event.stripeCallback(result.token)
    )
  else
    event.stripeCallback("")
