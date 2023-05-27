class ModeViews {
    constructor(saveStateElem, formElem) {
        
    }


}

// update autosave queue in response to form input/submit events
document.addEventListener("DOMContentLoaded", () => {
    const sets = []
    var startMode = null
    document.querySelectorAll("[data-ld-mode-set]").forEach((setElem) => {
        const set = setElem.dataset.ldModeSet
        if (sets.findIndex((x) => x === set) === -1) {
            sets.push(set)
        }
        if (setElem.dataset.ldModeIs) {
            startMode = setElem.dataset.ldModeIs
        }
    })

    if (startMode) {
        document.querySelectorAll("[data-ld-mode-view-on]").forEach((viewElem) => {
            viewElem.hidden = viewElem.dataset.ldModeViewOn !== startMode
        })
    }
    document.ld_addLiveListener("[data-ld-mode-set]", "click", (evt) => {
        const newMode = evt.target.dataset.ldModeSet
        document.querySelectorAll("[data-ld-mode-view-on]").forEach((viewElem) => {
            viewElem.hidden = viewElem.dataset.ldModeViewOn !== newMode
        })
    })

})
