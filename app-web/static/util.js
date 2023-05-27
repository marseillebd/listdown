// live binding helper
function ld_addLiveListener(selector, eventType, callback) {
  const context = this === window ? document : this
  context.addEventListener(eventType, (evt) => {
    const elem = evt.target.closest(selector)
    if (elem !== null) callback.call(elem, evt)
  })
}

Element.prototype.ld_addLiveListener = ld_addLiveListener
Document.prototype.ld_addLiveListener = ld_addLiveListener