
// Config loaded?
console.log("Javascript!", CONFIG)

// Load Elm App
var app = Elm.Main.init({
  node: document.getElementById('elm'),
  flags: CONFIG.loaded
});




// Plaid Link
var handler = Plaid.create({
  apiVersion: 'v2',
  clientName: 'Plaid Quickstart',
  env: CONFIG.plaid.env, // 'development',
  product: CONFIG.plaid.products, // [ 'transactions', 'auth' ],
  key: CONFIG.plaid.publicKey,
  webhook: CONFIG.plaid.webhook,
  onLoad: function() {
    // console.log("LOADED")
  },
  onSuccess: function(public_token) {
    // console.log("GOT PUBLIC TOKEN", public_token)
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
    // console.log("On Exit", err, metadata)
    app.ports.plaidLinkExit.send();
  }
});


// to open it!
// handler.open();


// PORTS
// console.log("PORTS", app.ports)
app.ports.plaidLinkOpen.subscribe(function(data) {
  console.log("Plaid Link Open")
  handler.open()
})

app.ports.appInitialized.subscribe(function(version) {
  console.log("Timely", "version:", version)
})
