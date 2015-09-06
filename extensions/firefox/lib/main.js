var contextMenu = require("sdk/context-menu");
var Request = require("sdk/request").Request;
var URL = require("sdk/url").URL;
var data = require("sdk/self").data;

var form = require("sdk/panel").Panel({
  contentURL: data.url("form.html"),
  contentScriptFile: data.url("form.js")
});

form.on("show", function() {
  form.port.emit("show");
});

form.port.on("save", function(image_data) {
  Request({
    url: "http://localhost:8080/images",
    content: JSON.stringify(image_data),
    contentType: "application/json",
    onComplete: function (response) {
      console.log("Save: " + image_data.url + "\nResponse: " + response.status);

      if (response.status == 200 || response.status == 204) form.hide();
      else form.port.emit("error", "Application Error " + response.status);
    }
  }).post();
});

var menuItem = contextMenu.Item({
  label: "Save image to addpic",
  context: contextMenu.SelectorContext("img"),
  contentScript: 'self.on("click", function (image, data) {' +
                 '  self.postMessage({' +
                 '    "url": image.src,' +
                 '    "title": document.title,' +
                 '    "comment": "",' +
                 '    "origin": document.URL,' +
                 '    "referrer": document.referrer,' +
                 '    "width": image.naturalWidth,' +
                 '    "height": image.naturalHeight,' +
                 '    "added_at": new Date().toISOString()' +
                 '  });' +
                 '});',
  onMessage: function (image_data) {
    form.show();

    form.port.emit("load", image_data);
  }
});
