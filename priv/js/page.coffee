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

#======================================================================================================================

class Collection
  constructor: (query)->
    @query = query

  load: ->
    $.getJSON "/images?tag=" + @query, (data) ->
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

    this.load_data(image_id)

    $("#slideshow-window").modal()

  load_data: (image_id) ->
    slideshow = this
    $.getJSON "/images/" + image_id, (data) ->
      slideshow.update_data(data)

  update_data: (data) ->
    this.data = data

    $("#slideshow-window #title").html(data.title)

    $("#slideshow-window #origin").attr("href", data.origin)
    $("#slideshow-window #origin").html(data.origin)

    $("#slideshow-window #tags").html(
        (data.tags || ["notag"]).map((tag) ->
           "<a class=\"label label-info\" href=\"?q=" + tag + "\">" + tag + "</a>"
        ).join("&nbsp;")
    )

    $("#slideshow-window #comment").html(data.comment)

    date = if data.added_at then new Date(data.added_at).toString() else ""
    $("#slideshow-window #added_at").html(date);

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

class Page
  constructor: ->
    query = this.get_query_parameter("q")
    this.load(query) if query

  load: (query) ->
    collection = new Collection(query)
    collection.load()

  get_query_parameter: (name) ->
    parameters = window.location.search.split(/[?&]/).slice(1)
    for i in [0...parameters.length]
      [parameter_name, parameter_value] = parameters[i].split("=")
      return parameter_value if parameter_name == name

#======================================================================================================================

events.on "collection.loaded", (collection) ->
  gallery = new Gallery(collection)
  gallery.show()
  document.slideshow = new Slideshow(collection)

#======================================================================================================================

$(document).ready ->
  $('#slideshow-window').keydown (e) ->
    switch e.which
      # Rigth
      when 39 then document.slideshow.next()
      # Left
      when 37 then document.slideshow.prev()
      # E
      when 69 then $('#edit_form').modal()

  page = new Page

  $("#btn_search").click ->
    query = $("#query").val()
    if query
      page.load(query)
      window.history.pushState(query, query, "/t?q=" + query)
    false

  $('#edit_form').on('show.bs.modal', (event) ->
    modal = $(this)
    data = document.slideshow.data

    modal.find('#form_title').val(data.title)
    modal.find('#form_tags').val(data.tags.join(', '))
    modal.find('#form_comment').val(data.comment)

    modal.find('.btn-danger').unbind("click").click ->
      if confirm("Are you sure?")
        $.ajax({
          url: "/images/" + document.slideshow.image_id,
          type: "DELETE",
          success: -> alert("Deleted")
          error: -> alert("Error")
        })

    modal.find('.btn-primary').unbind("click").click ->
      newdata = $.extend({}, data)
      newdata.title = modal.find('#form_title').val();
      newdata.tags = modal.find('#form_tags').val().split(/\s*,\s*/).filter((e) -> e.match(/\w+/))
      newdata.comment = modal.find('#form_comment').val();

      $.ajax({
        url: "/images/" + document.slideshow.image_id,
        type: "POST",
        data: JSON.stringify(newdata),
        contentType: "application/json",
        dataType: "json",
        success: (data, status) -> document.slideshow.update_data(newdata);
        error: -> alert("Error")
      })

      modal.modal('hide');
  )
