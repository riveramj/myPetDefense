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
updated = true
paymentStatus = ""
successUrl = ""
timeout = 20000

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

paymentRequest.on('token', (event) ->
  console.log event
  console.log "1"

  attachPaymentMethod(
    event.token.id,
    JSON.stringify(event.payerEmail),
    JSON.stringify(event.shippingAddress)
  )

  listenForServerUpdate(timeout).then(->
    event.complete(paymentStatus)

    window.location.replace(successUrl)
  )
)

elements = stripe.elements()
prButton = elements.create('paymentRequestButton', {
  paymentRequest: paymentRequest,
})

paymentRequest.on('shippingaddresschange', (ev) ->
  if (ev.shippingAddress.country != 'US')
    ev.updateWith({status: 'invalid_shipping_address'})
  else
    updated = false
    updateBillingAmount(JSON.stringify ev.shippingAddress)

    listenForServerUpdate(timeout).then(->
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
  start = Date.now()

  return new Promise (resolve, reject) ->
    checkingServerUpdate = ->
      if (updated)
        updated = false
        resolve "it worked"
      else if (timeout && (Date.now() - start) >= timeout)
        reject  Error "timeout"
      else
        setTimeout(checkingServerUpdate.bind(this), 250)

    checkingServerUpdate ->

$(document).ready ->
  $("body").on "click", '.checkout, .update-billing, #create-customer', (event) ->
    event.preventDefault()
    $(".checkout.submit, input.update-billing, .create-customer").prop('value', 'Please Wait')
    $(".checkout.submit, input.update-billing, .create-customer").prop('value', 'Please Wait').prop("disabled", true).addClass("processing")

    myPetDefenseSite.event("validate-stripe-form",
      stripeCallback: stripeCallback
    )

$(document).on "update-cart-items", (event) ->
  cartItems = event.items
  total = event.total
  updated = true

$(document).on "stripe-payment-status", (event) ->
  console.log "in payment status"
  console.log event
  console.log "in payment status"

  paymentStatus = event.status
  successUrl = event.successUrl
  updated = true

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
