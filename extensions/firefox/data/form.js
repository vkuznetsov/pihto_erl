var title = document.getElementById("title");
var tags = document.getElementById("tags");
var comment = document.getElementById("comment");
var btn_save = document.getElementById("btn_save");
var btn_cancel = document.getElementById("btn_cancel");
var current_image_data;

self.port.on("load", function(image_data) {
  title.value = image_data.title;
  current_image_data = image_data;
});

self.port.on("error", function(message) {
  alert(message);
});

btn_save.addEventListener("click", function() {
  current_image_data.title = title.value;
  current_image_data.tags = tags.value.split(/\s*,\s*/).filter(function(e) { return e.match(/\w+/) });
  current_image_data.comment = comment.value;

  self.port.emit("save", current_image_data);
});

btn_cancel.addEventListener("click", function() {
  self.hide();
});

