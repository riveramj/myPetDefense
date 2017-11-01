$(document).ready ->
  $(document)
    .on 'promotion-code-message', (event) ->
      if event.status == "success"
        $('.promotion-info').removeClass("promo-error").addClass("promo-success")
      else
        console.log "in error"
        $('.promotion-info').removeClass("promo-success").addClass("promo-error")

    .on 'groupon-code-message', (event) ->
      if event.status == "success"
        $('.groupon-info').removeClass("groupon-error").addClass("groupon-success")
      else if event.status == "frontline-error"
        $('.groupon-info').removeClass("groupon-success").addClass("groupon-frontline-error")
      else
        $('.groupon-info').removeClass("groupon-success").addClass("groupon-error")
