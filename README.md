# diaryweb

** Work in progress **

This is a web app for writing a diary.

## Build for development

It's not deployable yet, so these are development build instructions.

### Frontend

The frontend is written in Elm. Build as follows:

```
cd frontend
elm make src/Main.elm --output=/dev/null
```

### Backend

The backend is written in Haskell. Install the Haskell Tool Stack and then build as follows:

```
cd backend
stack build
```
