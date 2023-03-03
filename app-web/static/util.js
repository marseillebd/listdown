// live binding helper
Element.prototype.ld_addLiveListener = (selector, eventType, callback) => {
  const context = this || document
  context.addEventListener(eventType, (evt) => {
    const elem = evt.target.closest(selector)
    if (elem !== null) callback.call(elem, evt)
  })
}
