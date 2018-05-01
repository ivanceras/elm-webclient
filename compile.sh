mkdir -p ../public
rsync -vahP --delete static ../public/
elm-make src/Main.elm --yes --output ../public/static/diwata.js
