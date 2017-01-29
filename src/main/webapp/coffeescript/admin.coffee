$(document).ready ->
  $("body").on "click", '#create-item', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")

  $("body").on "click", '.create-item', (event) ->
    $(".validation-error").remove()
    $("input.error").removeClass("error")

  $("body").on "click", '.parent', (event) ->
    $(this).next('tr').toggleClass('collapsed')
    $(event.target).parent().toggleClass('active')

  $(".parent-tab").on "click", (event) ->
    selectedTab = $(this)
    selectedTabClass = "." + selectedTab.parent().attr('class')

    selectedTab.parent().siblings().removeClass('active')
    selectedTab.parents('.parent-nav').siblings().removeClass('active')
    
    selectedTab.parent().addClass('active')
    selectedTab.parents('.parent-nav').siblings(selectedTabClass).addClass('active')

    
