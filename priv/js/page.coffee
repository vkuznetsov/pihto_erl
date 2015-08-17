class Events
  constructor: ->
    @observers = {}

  on: (event, callback) ->
    if @observers[event]
      @observers[event].push(callback)
    else
      @observers[event] = [callback]

  fire: (event, argument) ->
    if @observers[event]
      callback(argument) for callback in @observers[event]

events = new Events

#======================================================================================================================

class Slice
  constructor: (data) ->
    @data = data

  image_ids: ->
    @data

class Collection
  constructor: ->
    @index = 0

  load: (tag) ->
    $.getJSON "/images?tag=" + tag, (data) ->
      @slice = new Slice(data)
      @index = 0
      events.fire('collection.loaded', this)

#======================================================================================================================

class Gallery
  constructor: (collection) ->
    @collection = collection

  show: ->
    $('#thumbs').html("")
    @collection.slice.image_ids().forEach (image) ->
      $("#thumbs").append(
        "<div class=\"thumb-box\">" +
        "  <span class=\"thumb-box-overlay\">" +
        "    <span class=\"thumb-box-overlay-background\"></span>" +
        "    <a class=\"glyphicon glyphicon-new-window\" href=\"/image/origin/" + image + "\" target=\"_blank\"></a>" +
        "    <a class=\"glyphicon glyphicon-picture\" href=\"/image/src/" + image + "\" target=\"_blank\"></a>" +
        "  </span>" +
        "  <a class=\"openslide\" href=\"/st/image/" + image + "\" data-image=\"" + image + "\" target=\"_blank\">" +
        "    <img class=\"thumb\" src=\"/st/thumb/" + image + "\" alt=\"image\">" +
        "  </a>" +
        "</div>"
      )

    $('.openslide').click ->
      image_id = $(this).data().image
      document.slideshow.show(image_id)
      $('#slideshow-window').modal()
      return false

#======================================================================================================================

class Slideshow
  constructor: (collection) ->
    @collection = collection

  show: (image_id) ->
    @image_id = image_id

    preload = new Image
    preload.src = '/st/image/' + image_id

    if (preload.complete)
        $("#slideshow-image").attr("src", preload.src)
        preload.onload = ->
    else
        $("#slideshow-image").attr("src", "/img/loader.gif")
        preload.onload = ->
          $("#slideshow-image").attr("src", preload.src)
          preload.onload = ->

    $("#slideshow-window").modal()

  next: ->
    current_index = @collection.slice.image_ids().indexOf(@image_id)
    next_image_id = @collection.slice.image_ids()[current_index + 1]
    if next_image_id
      this.show(next_image_id)

  prev: ->
    current_index = @collection.slice.image_ids().indexOf(@image_id)
    prev_image_id = @collection.slice.image_ids()[current_index - 1]
    if prev_image_id
      this.show(prev_image_id)

#======================================================================================================================

events.on "collection.loaded", (collection) ->
  gallery = new Gallery(collection)
  gallery.show()
  document.slideshow = new Slideshow(collection)

#======================================================================================================================

$(document).ready ->
  $('#slideshow-window').keydown (e) ->
      if e.which == 39
        document.slideshow.next()
      else if e.which == 37
        document.slideshow.prev()

  $("#btn_search").click ->
    collection = new Collection
    collection.load($("#query").val());

  collection = new Collection
  collection.load("cat")


