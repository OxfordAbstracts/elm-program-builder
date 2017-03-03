(function(){
  var node = document.querySelector("div");
  var app = Elm.Main.embed(node, {
      eventId: window.location.href.split('/events/')[1] ? window.location.href.split('/events/')[1].split('/')[0] : "",
      host: window.location.origin
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
          minDate: new Date()
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
})()
