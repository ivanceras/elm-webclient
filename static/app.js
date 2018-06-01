// the db_url and grouped is retrieved from the indexed db
function init(){
    app = Elm.Main.fullscreen(
        { db_url: null,
          db_name: null,
          api_endpoint: null,
          grouped: true,
        }
    );

    app.ports.title.subscribe(function(title) {
        document.title = title;
    });
}
window.onload = init
