function init(){
    var username = null;
    var password = null;
    var windowListIsHidden = false;
    var hasStorage = storageAvailable('localStorage');
    if (hasStorage){
        username = localStorage.getItem('username');
        password = localStorage.getItem('password');
        windowListIsHidden = localStorage.getItem('is_window_list_hidden') || false;
    }
    else{
        console.error("localStorage is not supported");
    }
    var cred = null;
    if (username != null && password != null) {
        cred = {username: username, password: password}
    }
    app = Elm.Main.fullscreen(
        { login_required: false,
          db_name: null,
          api_endpoint: null,
          grouped: true,
          cred: cred,
          is_window_list_hidden: windowListIsHidden  
        }
    );

    app.ports.title.subscribe(function(title) {
        document.title = title;
    });

    app.ports.setUsername.subscribe(function(username) {
        if (hasStorage){
            localStorage.setItem('username', username);
        }
        else{
            console.error("localStorage is not supported");
        }
    });
    app.ports.setPassword.subscribe(function(password) {
        if (hasStorage){
            localStorage.setItem('password', password);
        }
        else{
            console.error("localStorage is not supported");
        }
    });

    app.ports.setWindowListIsHidden.subscribe(function(isHidden) {
        if (hasStorage){
            localStorage.setItem('is_window_list_hidden', isHidden);
        }
        else{
            console.error("localStorage is not supported");
        }
    });
}
window.onload = init

//https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API
function storageAvailable(type) {
    try {
        var storage = window[type],
            x = '__storage_test__';
        storage.setItem(x, x);
        storage.removeItem(x);
        return true;
    }
    catch(e) {
        return e instanceof DOMException && (
            // everything except Firefox
            e.code === 22 ||
            // Firefox
            e.code === 1014 ||
            // test name field too, because code might not be present
            // everything except Firefox
            e.name === 'QuotaExceededError' ||
            // Firefox
            e.name === 'NS_ERROR_DOM_QUOTA_REACHED') &&
            // acknowledge QuotaExceededError only if there's something already stored
            storage.length !== 0;
    }
}
