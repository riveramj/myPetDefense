stripe = Stripe 'pk_test_JLczczIy7T5qGL8DOmwTc2O0'

cartItems = []
total = {
  label: 'Due Today',
  amount: 0
}

updated = true
paymentStatus = ""
successUrl = ""
timeout = 20000

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

  if (event.token.card.funding == "prepaid")
    $(".payment-errors").text("Prepaid cards are not accepted. Please use a different card.")
    $(".payment-errors").removeClass("hidden")

    event.complete("fail")
  else
    attachPaymentMethod(
      event.token.id,
      JSON.stringify(event.payerEmail),
      JSON.stringify(event.shippingAddress)
    )

    listenForServerUpdate(timeout).then(->
      event.complete(paymentStatus)

      if paymentStatus == "success"
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

$(document).on "update-cart-items", (event) ->
  cartItems = event.items
  total = event.total
  updated = true

$(document).on "stripe-payment-status", (event) ->
  paymentStatus = event.status
  successUrl = event.successUrl
  errorMessage = event.errorMessage
  $(".payment-errors").text(errorMessage)
  $(".payment-errors").removeClass("hidden")
  updated = true

$(document).ready ->
  $.getJSON('https://api.ipify.org?format=json', (data) ->
    $(".ip-address").val(data.ip)
    $(".ip-address").blur()
  )
