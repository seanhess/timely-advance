
// Load Elm App
var app = Elm.Main.init({
  node: document.getElementById('elm')
});


// Config loaded?
console.log("Javascript!", CONFIG)

// Plaid Link
var handler = Plaid.create({
  apiVersion: 'v2',
  clientName: 'Plaid Quickstart',
  env: 'sandbox',
  product: [ 'transactions' ],
  key: CONFIG.plaid.publicKey,
  // webhook: 'https://your-domain.tld/plaid-webhook',
  onLoad: function() {
    console.log("LOADED")
  },
  onSuccess: function(public_token) {
    console.log("GOT PUBLIC TOKEN", public_token)
    app.ports.plaidLinkDone.send(public_token);
    // $.post('/get_access_token', {
    //   public_token: public_token
    // }, function(data) {
    //   $('#container').fadeOut('fast', function() {
    //     $('#item_id').text(data.item_id);
    //     $('#access_token').text(data.access_token);
    //     $('#intro').hide();
    //     $('#app, #steps').fadeIn('slow');
    //   });
    // });
  },
  onExit: function(err, metadata) {
    console.log("On Exit", err, metadata)
    app.ports.plaidLinkExit.send();
  }
});


// to open it!
// handler.open();


// PORTS
console.log("PORTS", app.ports)
app.ports.plaidLinkOpen.subscribe(function(data) {
  console.log("Plaid Link Open")
  handler.open()
})
