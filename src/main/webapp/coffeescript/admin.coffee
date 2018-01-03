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
