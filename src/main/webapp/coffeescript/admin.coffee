$(document).ready ->
  $("body").on "click", '#create-item', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")

  $("body").on "click", '.create-item', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")

  $("body").on "click", '.parent', (event) ->
    $(this).toggleClass('expanded')

    $(event.target).parent().toggleClass('active')

  $("body").on "click", '.parent-tab', (event) ->
    selectedTab = $(this)
    selectedTabClass = "." + selectedTab.parent().attr('class')

    selectedTab.parent().siblings().removeClass('active')
    selectedTab.parents('.parent-nav').siblings().removeClass('active')
    
    selectedTab.parent().addClass('active')
    selectedTab.parents('.parent-nav').siblings(selectedTabClass).addClass('active')
  $('.dashboard-nav').on "click", 'button', (event) ->
    $(event.target).parent().siblings().removeClass('active')
    $(event.target).parent().addClass('active')

  $("body").on "click", ".copy-address", (event) ->
    firstName = $(event.target).parent().siblings().find('.first-name').val()
    lastName = $(event.target).parent().siblings().find('.last-name').val()
    address1 = $(event.target).parent().siblings().find('.address-1').val()
    address2 = $(event.target).parent().siblings().find('.address-2').val()
    city = $(event.target).parent().siblings().find('.city').val()
    state = $(event.target).parent().siblings().find('.state').val()
    zip = $(event.target).parent().siblings().find('.zip').val()

    address = "#{firstName} #{lastName}\n#{address1}\n#{address2}\n#{city},#{state} #{zip}"
    
    dummyInput = $('<textarea>').val(address).appendTo('body').select()

    document.execCommand('copy')
   
    dummyInput.remove()

    alert "Address copied to clipboard"

  $("body").on "click", '.sku-entry', (event) ->
    target = $(event.target)

    if !target.hasClass('add-one') &&
        !target.hasClass('subtract-one') &&
        !target.hasClass('expand-row')
      target.parent('.sku-entry').find('.expand-row').click()
