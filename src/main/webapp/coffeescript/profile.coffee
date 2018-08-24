$(document).ready ->
  $(".go-back").on "click", (event) ->
    window.history.back()

  $("body").on "click", '.cancel', (event) ->
    $(".cancel").prop('value', 'Please Wait').prop("disabled", true).addClass("processing")
    $(".cancel").submit()

  $(".account-block").on "click", ".pause-account, .cancel-account", (event) ->
    accountAction = $(this)

    accountAction.children('.pause-cancel-select').addClass("selected")

    if accountAction.hasClass("pause-account")
      $('span.cancel').removeClass("selected")
    else
      $('span.pause').removeClass("selected")

    $('.continue-account-changes').prop("disabled","").addClass("selected").removeClass("disabled")
