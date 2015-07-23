var contextMenu = require("sdk/context-menu");
var Request = require("sdk/request").Request;
var URL = require("sdk/url").URL;

var menuItem = contextMenu.Item({
  label: "Save image to addpic",
  context: contextMenu.SelectorContext("img"),
  contentScript: 'self.on("click", function (image, data) {' +
                 '  self.postMessage({' +
                 '    "url": image.src,' +
                 '    "title": document.title,' +
                 '    "comment": "",' +
                 '    "origin": document.URL,' +
                 '    "width": image.naturalWidth,' +
                 '    "height": image.naturalHeight' +
                 '  });' +
                 '});',
  onMessage: function (data) {
    console.log(data);
    Request({
      url: "http://localhost:8080/save",
      content: data,
      onComplete: function (response) {
        console.log("Saved: " + data.url);
      }
    }).post();
  }
});