class AutosaveQueue {
    constructor(saveStateElem, formElem) {
        // TODO some way to turn autosave off
        this.saveStateElem = saveStateElem
        this.formElem = formElem
        this.updated = false
        this.inFlight = null // a promise
        // TODO this timer should probably reset itself when there's text still beign edited
        // that is, after an input evert, wait for 3 seconds after last input, then save
        this.timer = setInterval(() => {
            if (this.saveStateElem.dataset.state === "error") { return }
            this.submit()
        }, 10*1000 /*10s*/)
    }

    update(evt) {
        this.updated = true
        if (this.saveStateElem.dataset.state === "saving") { return }
        this.saveStateElem.dataset.state = "unsaved"
    }

    submit(evt) {
        if (!this.updated) { return }
        if (this.inFlight) { return }
        // administrivia
        this.saveStateElem.dataset.state = "saving"
        if (evt !== undefined) {
            evt.preventDefault()
        }
        // submit form by AJAX
        let promise = fetch(
            this.formElem.action,
            { method: this.formElem.method
            , body: new FormData(this.formElem)
            })
        promise = promise.catch((error) => this.error("could not contact server"))
        promise = promise.then((response) => {
            if (!response.ok) { this.error(response.body) }
            this.updated = false
            this.inFlight = null
            this.saveStateElem.dataset.state = "saved"
        })
        this.inFlight = promise
    }

    error(msg) {
        console.log(msg) // DEBUG
        this.saveStateElem.dataset.state = "error"
        this.inFlight = null
        if (msg === undefined) { return }
        this.saveStateElem.querySelectorAll(".error-message").forEach((msgElem) => {
            msgElem.textContent = msg
        })
    }
}

// update autosave queue in response to form input/submit events
document.addEventListener("DOMContentLoaded", () => {
    document.querySelectorAll(".save-state").forEach((saveStateElem) => {
        const formId = saveStateElem.dataset.for
        const formElem = document.getElementById(formId)
        const queue = new AutosaveQueue(saveStateElem, formElem)
        formElem.addEventListener("input", (evt) => queue.update(evt))
        formElem.addEventListener("submit", (evt) => queue.submit(evt))
        formElem.addEventListener("blur", (evt) => queue.submit())
    })
})
