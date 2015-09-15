Collection = React.createClass
  componentDidMount: ->
    self = this
    $.getJSON "/images?tag=" + @props.query, (data) ->
      images = data.map((image_id)-> {uid: image_id})
      self.setState {images: images, page: 1}

  images: ->
    @state.images

  render: ->
    if @state && @state.images
      <div>
        <Thumbnails images={@state.images} />
      </div>
    else
      <div/>

Thumbnails = React.createClass
  showSlideshow: (index) ->
    React.render(<Slideshow images={@props.images} index={index} />, document.getElementById('slideshow'))

  render: ->
    showSlideshow = @showSlideshow

    create_item = (image, index) ->
      <Thumb key={image.uid} image={image} index={index} showSlideshow={showSlideshow} />

    <div>{@props.images.map(create_item)}</div>

Thumb = React.createClass
  showSlideshow: (e)->
    e.preventDefault();
    @props.showSlideshow(@props.index)

  render: ->
    image = @props.image
    <div className="thumb-box">
      <span className="thumb-box-overlay">
        <span className="thumb-box-overlay-background"></span>
        <a className="glyphicon glyphicon-new-window" href={Routes.image_origin(image)} target="_blank"></a>
        <a className="glyphicon glyphicon-picture" href={Routes.image_source(image)} target="_blank"></a>
      </span>
      <a className="openslide" href={Routes.image_thumbnail(image)} onClick={@showSlideshow} target="_blank">
        <img className="thumb" src={Routes.image_thumbnail(image)} alt="image"/>
      </a>
    </div>

Slideshow = React.createClass
  getInitialState: ->
    {index: @props.index}

  componentDidMount: ->
    self = this

    $('#slideshow-window').keydown (e) ->
      switch e.which
        # Rigth
        when 39 then self.setState(index: self.state.index + 1)
        # Left
        when 37 then self.setState(index: self.state.index - 1)
        # E
        when 69 then $('#edit_form').modal()
    $('#slideshow-window').modal()

  render: ->
    <img src={Routes.image_big(@props.images[@state.index])}/>

Routes = {
  image_thumbnail: (image) -> "/st/thumb/" + image.uid,
  image_big: (image) -> "/st/thumb/" + image.uid,
  image_origin: (image) -> "/image/origin/" + image.uid,
  image_source: (image) -> "/image/src/" + image.uid
}

$(document).ready ->
  React.render(<Collection query="cat" />, document.getElementById('thumbs'))
