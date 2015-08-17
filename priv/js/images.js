var images_collection, image_index;

function load(tag) {
  $.getJSON("/images?tag=" + tag, function(data) {
    images_collection = data;
    image_index = 0;

//    show();
    update_switcher();

    $("#page_title").html(tag);

    load_thumbs(images_collection);
  });
}

function load_thumbs(images) {
  $('#thumbs').html("");
  images.forEach(function(image) {
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
    );
  });

  $('.openslide').click(function() {
    var image_id = $(this).data().image;
    $('.slideshow-image').attr("src", "/st/image/" + image_id);
    $('#slideshow-window').modal();
    return false;
  });
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
    var date;

    if (image_data.added_at) {
      date = new Date(image_data.added_at).toString();
    }
    else {
      date = "";
    }

    $("#added_at").html(date);
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

    load("cat");
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

function slideshow(image_id) {
    var preload = new Image();

    preload.src = '/st/image/' + image_id;

    if (preload.complete) {
        showSlideshowImage(preload);
        preload.onload = function() {};
    }
    else {
        $("#image").attr("src", "/img/loader.gif");
        preload.onload = function() {
          showSlideshowImage(preload);
          preload.onload = function() {};
        }
    }

}

function showSlideshowImage(image) {
  var max_width = $(window).width();
  var max_height = $(window).height();

  var width = image.width > max_width ? max_width : image.width;
  var height = image.height > max_height ? max_height : image.height;

  var top = max_height / 2 - height / 2;
  var left = max_width / 2 - width / 2;

  $('.slideshow img').attr("src", image.src);
  $('.slideshow').css({
    'top': top,
    'left': left,
    'max-width': max_width,
    'max-height': max_height,
    'visibility': 'visible'
  });
}

$('.slideshow').click(function () {
  $('.slideshow').css({visibility: 'hidden'});
});
