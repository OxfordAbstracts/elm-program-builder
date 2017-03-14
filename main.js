function init(options, dev){
  var elmProgrammeBuilder;
  if (dev){
    elmProgrammeBuilder = Elm;
  } else {
    elmProgrammeBuilder = require('elm-program-builder/elm');
  }
  var app = elmProgrammeBuilder.Main.embed(options.node, {
      eventId: options.eventId,
      host: options.host,
      showPreviewUi: options.showPreviewUi,
      showPublishPage: options.showPublishPage
  });

  app.ports.openDatepicker.subscribe(function(id) {
    //adds pikaday to input for newly added date
    var pikadayInputs = document.getElementsByClassName('pikaday-input');
    if(id){
      return setTimeout(function(){
        var pickNew = document.getElementById('pikaday-instance-' + id);
        new Pikaday({
          field: pickNew,
          format: 'D MMM YYYY',
          defaultDate: new Date(Date.now()),
          minDate: new Date(),
          onSelect: function(){
            sendDates('changePickedDates');
          }
        });
      }, 100)
    }

    [].forEach.call(pikadayInputs, function(pikadayInput) {
      new Pikaday({
        field: pikadayInput,
        format: 'D MMM YY',
        minDate: new Date(),
        defaultDate: new Date(pikadayInput.value),
        onSelect: function(){
          sendDates('changePickedDates');
        }
      });
    });

    document.getElementById('save-dates-btn').addEventListener('click', function(e){
      sendDates('changeDates');
    });


  function sendDates (port) {
    var pikadayDateArray =
    [].map.call(pikadayInputs, function(pikaDayInput){
      return pikaDayInput.value;
    });
     app.ports[port].send(pikadayDateArray)
   }
  });
}
module.exports.init = init;
