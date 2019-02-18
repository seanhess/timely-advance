
function plaid(options) {

  function open() {
    console.log("FAKE PLAID OPEN")
    options.onLoad()

    setTimeout(function() {
      var result = prompt("Please enter public_token", "fake-plaid-token")

      if (result) {
        options.onSuccess(result)
      }
      options.onExit(null, {})
    }, 400)
  }

  return {open: open}
}

var Plaid = { create: plaid }
