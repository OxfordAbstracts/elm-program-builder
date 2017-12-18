
/* global Elm Pikaday */

function init(options, dev) {
  var elmProgrammeBuilder;
  if (dev) {
    elmProgrammeBuilder = Elm;
  } else {
    elmProgrammeBuilder = require('./elm');
  }
  var app = elmProgrammeBuilder.Main.embed(options.node, {
    eventId: options.eventId,
    host: options.host,
    showPreviewUi: options.showPreviewUi,
    showPublishPage: options.showPublishPage,
    showBasicPage: options.showBasicPage
  });

  app.ports.openDatepicker.subscribe(function(id) {
    // adds pikaday to input for newly added date
    var pikadayInputs = document.getElementsByClassName('pikaday-input');
    if (id) {
      return setTimeout(function() {
        var pickNew = document.getElementById('pikaday-instance-' + id);
        new Pikaday({
          field: pickNew,
          format: 'D MMM YYYY',
          defaultDate: new Date(Date.now()),
          minDate: new Date(),
          onSelect: function() {
            sendDates('changePickedDates');
          }
        });
      }, 100);
    }[].forEach.call(pikadayInputs, function(pikadayInput) {
      new Pikaday({
        field: pikadayInput,
        format: 'D MMM YY',
        minDate: new Date(),
        defaultDate: new Date(pikadayInput.value),
        onSelect: function() {
          sendDates('changePickedDates');
        }
      });
    });

    document.getElementById('save-dates-btn').addEventListener('click', function() {
      sendDates('changeDates');
    });

    function sendDates(port) {
      var pikadayDateArray = [].map.call(pikadayInputs, function(pikaDayInput) {
        return pikaDayInput.value;
      });
      app.ports[port].send(pikadayDateArray);
    }
  });

  app.ports.showDeleteConfirmation.subscribe(function(sessionId) {
    var confirmed = confirm('Are you sure? This will permanently delete the session');
    if(confirmed){
      app.ports['deleteSession'].send(sessionId);
    }
  });

  app.ports.showDeleteInformationConfirmation.subscribe(function(savedInfoId) {
    var confirmed = confirm('Are you sure? This will permanently the information');
    if(confirmed){
      app.ports['deleteInformation'].send(savedInfoId);
    }
  });
// thanks to https://github.com/phylor/elm-image-upload/blob/master/index.html
  app.ports.fileSelected.subscribe(function (id) {
    var node = document.getElementById('file-to-save-' + id);
    if (node === null || !node.files[0]) {
      return app.ports.fileContentRead.send(null);

    }
   // If your file upload field allows multiple files, you might
   // want to consider turning this into a `for` loop.
    var file = node.files[0];
    var reader = new FileReader();
   // FileReader API is event based. Once a file is selected
   // it fires events. We hook into the `onload` event for our reader.
    reader.onload = (function(event) {
     // The event carries the `target`. The `target` is the file
     // that was selected. The result is base64 encoded contents of the file.
      var base64encoded = event.target.result;
     // We build up the `FilePortData` object here that will be passed to our Elm
     // runtime through the `fileContentRead` subscription.
      var portData = {
        id: id,
        contents: base64encoded,
        filename: file.name,
        infoTitle: document.getElementById('info-title-' + id).value,
        infoDescription: document.getElementById('info-description-' + id).value
      };
     // We call the `fileContentRead` port with the file data
     // which will be sent to our Elm runtime via Subscriptions.
      app.ports.fileContentRead.send(portData);
    });
   // Connect our FileReader with the file that was selected in our `input` node.
    reader.readAsDataURL(file);
  });

  app.ports.fileChanged.subscribe(function (savedInfoId) {
    var node = document.getElementById('file-to-change-' + savedInfoId);
    if (node === null || !node.files[0]) {
      return app.ports.changedFileContentRead.send(null);
    }
   // If your file upload field allows multiple files, you might
   // want to consider turning this into a `for` loop.
    var file = node.files[0];
    var reader = new FileReader();
   // FileReader API is event based. Once a file is selected
   // it fires events. We hook into the `onload` event for our reader.
    reader.onload = (function(event) {
     // The event carries the `target`. The `target` is the file
     // that was selected. The result is base64 encoded contents of the file.
      var base64encoded = event.target.result;
     // We build up the `FilePortData` object here that will be passed to our Elm
     // runtime through the `fileContentRead` subscription.
      var portData = {
        id: savedInfoId,
        contents: base64encoded,
        filename: file.name
      };

     // We call the `fileContentRead` port with the file data
     // which will be sent to our Elm runtime via Subscriptions.
      app.ports.changedFileContentRead.send(portData);
    });
   // Connect our FileReader with the file that was selected in our `input` node.
    reader.readAsDataURL(file);
  });
}

module.exports.init = init;
