var images_collection, image_index;

//======================================================================================================================
// Events
//======================================================================================================================
var __event_observers = {};

function on_event(event_name, callback) {
  if (__event_observers[event_name] == undefined) {
    __event_observers[event_name] = [callback];
  }
  else {
    __event_observers[event_name].push(callback);
  }
}
function fire_event(event_name, argument) {
  if (__event_observers[event_name] != undefined) {
    __event_observers[event_name].forEach(function(observer_callback) {
      observer_callback(argument);
    });
  }
}

//======================================================================================================================
// Collection
//======================================================================================================================
var __collection = {data: [], image_index: 0};

function collection_load(tag) {
  $.getJSON("=images?tag=" + tag, function(data) {
    images_collection = data;
    image_index = 0;
    
    __collection.data = data;
    __collection.image_index = 0;
    
    fire_event("collection.loaded", __collection);
  });

  $("#page_title").html(tag);  
}

//======================================================================================================================
// Viewer
//======================================================================================================================

on_event("collection.switch", function(image) {
  view_update_image(image);
  view_update_title(image);
});

function load(tag) {
  $.getJSON("/images?tag=" + tag, function(data) {
    images_collection = data;
    image_index = 0;

    show();
    update_switcher();
  });

  $("#page_title").html(tag);
}

function next() {
    if (image_index + 1 >= images_collection.length) return;

    $('#edit_form').modal('hide');

    image_index += 1;
    show();
    update_switcher();
}

function prev() {
    if (image_index == 0) return;

    $('#edit_form').modal('hide');

    image_index -= 1;
    show();
    update_switcher();
}

function update_switcher() {
  $("#switcher").html(image_index + 1 + " of " + images_collection.length)
}

function show() {
    var image_data = images_collection[image_index];

    update_image(image_data);
    update_title(image_data);
    update_origin(image_data);
    update_tags(image_data);
    update_added_at(image_data);
    update_comment(image_data);
}

function preload_image(imgSrc, callback){
  var objImagePreloader = new Image();

  objImagePreloader.src = imgSrc;
  if(objImagePreloader.complete){
    callback();
    objImagePreloader.onload=function(){};
  }
  else{
    objImagePreloader.onload = function() {
      callback();
      //    clear onLoad, IE behaves irratically with animated gifs otherwise
      objImagePreloader.onload=function(){};
    }
  }
}

function update_image(image_data) {
    $("#image_link").attr("href", image_data.url);

    var preload = new Image();

    preload.src = image_data.url;

    if (preload.complete) {
        $("#image").attr("src", image_data.url);
        preload.onload = function() {};
    }
    else {
        $("#image").attr("src", "/img/loader.gif");
        preload.onload = function() {
            $("#image").attr("src", image_data.url);
            preload.onload = function() {};
        }
    }
}

function update_title(image_data) {
    $("#title").html(image_data.title);
}

function update_origin(image_data) {
    $("#origin").attr("href", image_data.origin);
    $("#origin").html(image_data.origin);
}

function update_tags(image_data) {
    $("#tags").html(
        (image_data.tags || ["notag"]).map(function(tag) {
            return "<a class=\"label label-info\" href=\"=tag=" + tag + "\">" + tag + "</a>"
        }).join("&nbsp;")
    );
}

function update_added_at(image_data) {
    $("#added_at").html(image_data.added_at);
}

function update_comment(image_data) {
    $("#comment").html(image_data.comment);
}

function save_image(image_data) {
    $.ajax({
        url: "/images/" + image.uid,
        type: "POST",
        data: image_data,
        contentType: "application/x-www-form-urlencoded",
        dataType: "json",
        success: function(data, status) {

        },

        error: function() {
            alert("Error");
        }
    });
}

$(document).ready(function() {
    $("#switch-prev").click(function() {
        prev();
    });

    $("#switch-next").click(function() {
        next();
    });

    load("notag");
});

$(document).keydown(function(e) {
    if (e.which == 37) prev();
    else if (e.which == 39) next();
})

$("#btn_search").click(function() {
    load($("#query").val());
});


$('#edit_form').on('show.bs.modal', function (event) {
  var modal = $(this);
  var image_data = images_collection[image_index];
  var form_title = modal.find('#form-title');
  var form_tags = modal.find('#form-tags');
  var form_comment = modal.find('#form-comment');

  form_title.val(image_data.title);
  form_tags.val(image_data.tags.join(', '));
  form_comment.val(image_data.comment);

  modal.find('.btn-primary').unbind("click").click(function() {
    image_data.title = form_title.val();
    image_data.tags = form_tags.val().split(/\s*,\s*/).filter(function(e) { return e.match(/\w+/) });
    image_data.comment = form_comment.val();

    save_image(image_data);

    show();
    modal.modal('hide');
  })
});