$(document).ready ->
  $(".go-back").on "click", (event) ->
    window.history.back()

  $("body").on "click", '.cancel', (event) ->
    $(".cancel").prop('value', 'Please Wait').prop("disabled", true).addClass("processing")
    $(".cancel").submit()

  $(".account-block").on "click", ".pause-account, .cancel-account", (event) ->
    accountAction = $(this)

    accountAction.children('button').addClass("selected")

    if accountAction.hasClass("pause-account")
      $('button#cancel-account').removeClass("selected")
    else
      $('button#pause-account').removeClass("selected")

  $('.continue-account-changes').addClass("selected")
