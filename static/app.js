// the db_url and grouped is retrieved from the indexed db
function init(){
    var username = localStorage.getItem('username');
    var password = localStorage.getItem('password');
    console.log("username:", username);
    console.log("password:", password);
    var cred = null;
    if (username != null && password != null) {
        cred = {username: username, password: password}
    }
    app = Elm.Main.fullscreen(
        { login_required: false,
          db_name: null,
          api_endpoint: null,
          grouped: true,
          cred: cred 
        }
    );

    app.ports.title.subscribe(function(title) {
        document.title = title;
    });

    app.ports.setUsername.subscribe(function(username) {
        localStorage.setItem('username', username);
    });
    app.ports.setPassword.subscribe(function(password) {
        localStorage.setItem('password', password);
    });
}
window.onload = init
