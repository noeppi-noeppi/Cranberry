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
        oidc: oidc ? window.location.search : null,
        prefersDarkTheme: window.matchMedia('(prefers-color-scheme:dark)').matches
    }
})

if (oidc) {
    window.history.pushState({},"", "/_");
}

window.addEventListener('resize', function() { app.ports.viewport.send(viewportFlags()) });
