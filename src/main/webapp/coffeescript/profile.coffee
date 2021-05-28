$(document).ready ->
  $(".upgrade").on "click", (event) ->
    window.location.href = "/account-overview"

  $(".go-back").on "click", (event) ->
    window.history.back()

  $("body").on "click", '.cancel, .save-changes, .modal-action.save, #add-pet', (event) ->
    $(event.target).html('<i class="fas fa-spinner fa-pulse"></i>')
    $(event.target).prop('value', 'Please Wait').prop("disabled", true).addClass("processing")
    $(event.target).submit()

  $(".account-block").on "click", ".resume-account, .pause-account, .cancel-account", (event) ->
    accountAction = $(this)

    accountAction.children('.pause-cancel-select').addClass("selected")

    if accountAction.hasClass("pause-account")
      $('span.cancel').removeClass("selected")
      $('span.resume').removeClass("selected")
    else if accountAction.hasClass("cancel-account")
      $('span.pause').removeClass("selected")
      $('span.resume').removeClass("selected")
    else
      $('span.pause').removeClass("selected")
      $('span.cancel').removeClass("selected")

    $('.continue-account-changes').prop("disabled","").addClass("selected").removeClass("disabled")

  $(".modal-action.cancel").on "click", (event) ->
      window.location.href = "/account-overview"

$(document).on "form-validation-error", (event) ->
  $("#add-pet").prop('value', 'Add Pet').prop("disabled", false).removeClass("processing")