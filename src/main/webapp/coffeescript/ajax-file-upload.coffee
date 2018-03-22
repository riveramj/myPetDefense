# Be aware that this code is connected with logic in IEAjaxFileUploadFix on server-side
# especially 'liftIFrameUpload' is used to identify iframe file upload
window.myPetDefenseSite.ajaxifyFileUpload = ($form) ->
  iframeName = "#{$form.attr 'id'}-upload-iframe"
  iframe =
    $("<iframe name=\"#{iframeName}\" />")
      .css('display', 'none')
      .load ->
        try
          $.globalEval $(this).contents().text()
          setupForm()
        catch error
          alert("File upload failed. Please contact support.")
          console?.log(error)
          $form.trigger('form-submission-completed')

  setupForm = ->
    # Return if we've already set the form up.
    return if $form.children('[type=hidden][value=_]').length

    $form
      .attr('target', iframeName)
      .removeAttr('onsubmit')
      .attr('action', "/ajax_request/#{lift_page}?liftIFrameUpload=#{lift_page}")
      .attr('method', 'post')
      .attr('encoding', 'multipart/form-data')
      .attr('enctype', 'multipart/form-data')
      .find('input:submit, button[type!=button], #nextBtn')
        .removeAttr('onclick')
        .click( ->
          submitName = $(this).attr 'name'

          # Add the button as a hidden element on click.
          $form.append($('<input type="hidden" name="' + submitName + '" value="_" />'))

          # Once the submission is done, remove the hidden
          # element. This is so future submissions won't have this
          # problem.
          myPetDefenseSite.nextTick ->
            $form
              .children('input[type=hidden][name="' + submitName + '"]')
              .remove();
        )
      .end()

  $('body').append iframe
  setupForm()


$(document).ready ->
  myPetDefenseSite.ajaxifyFileUpload $('#upload-reviews-form')
  myPetDefenseSite.ajaxifyFileUpload $('#upload-tracking-numbers-form')

