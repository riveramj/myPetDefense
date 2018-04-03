$(document).ready ->
  $(document)
    .on 'promotion-code-message', (event) ->
      if event.status == "success"
        $('.promotion-info').removeClass("promo-error").addClass("promo-success")
      else
        $('.promotion-info').removeClass("promo-success").addClass("promo-error")
