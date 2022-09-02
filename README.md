# Money Tracker
A simple project that I've created to learn [Elm language](http://elm-lang.org/).

## How to run
When started, the app is available inside Elm reactor on port 8000.

```bash
docker container run -it --rm -v ${pwd}:/elm -p 8000:8000 kraftdorian/elm:0.19.1-alpine3.16 /bin/ash
cd /elm
elm reactor
```