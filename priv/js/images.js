var images_collection, image_index;

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

    image_index += 1;
    show();
    update_switcher();
}

function prev() {
    if (image_index == 0) return;

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

function update_image(image_data) {
    $("#image").attr("src", image_data.url);
    $("#image_link").attr("href", image_data.url);
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
        (image_data.tags || ["notags"]).map(function(tag) {
            return "<a href=\"/tag/" + tag + "\">" + tag + "</a>"
        }).join(",&nbsp;")
    );
}

function update_added_at(image_data) {
    $("#added_at").html(image_data.added_at);
}

function update_comment(image_data) {
    $("#comment").html(image_data.comment);
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
