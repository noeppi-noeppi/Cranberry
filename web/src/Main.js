function viewportFlags() {
    return {
        w: window.innerWidth,
        h: window.innerHeight
    }
}

var app = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: {
        viewport: viewportFlags(),
        prefersDarkTheme: window.matchMedia('(prefers-color-scheme:dark)').matches
    }
})

window.addEventListener('resize', function() { app.ports.viewport.send(viewportFlags()) });
