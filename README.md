# MrLasers Webpack Starter for Elm

Steps for creating a new Elm project using webpack with hot reloading and all that jazz.

# Setup

## Node Project

1. Create new directory
   - Open directory in VSCode
1. Initialize yarn project
   ```
   yarn set version stable
   yarn init -y
   ```
1. Initialize elm project
   ```
   elm init
   ```
   Confirm that you'd like to create `elm.json` file
1. Add webpack to project
   ```
   yarn add -D webpack webpack-cli webpack-dev-server
   ```
1. Add webpack loaders and plugins
   ```
   yarn add -D html-webpack-plugin elm-webpack-loader elm-hot-webpack-loader
   ```
1. Create file structure
   ```
   +- src
   |  +- elm
   |   - Main.elm
   |  +- static
   |     +- styles
   |        - styles.css
   |     - index.html
   |     - index.js
   |- webpack.config.js
   ```

## Webpack

1. Add `scripts` to `package.json`
   ```json
   "scripts": {
     "dev": "webpack serve --mode=development",
     "build": "webpack --mode-production"
   }
   ```
1. Create `webpack.config.js`

   ```js
   const Path = require("path");
   const HtmlWebpackPlugin = require("html-webpack-plugin");

   module.exports = {
     entry: "./src/static/index.js",
     output: {
       path: Path.resolve(__dirname, "dist"),
       filename: "[name].js",
     },
     resolve: {
       extensions: [".js", ".elm"],
     },
     module: {
       noParse: /\.elm$/,
       rules: [
         {
           test: /\.elm$/,
           exclude: [/elm-stuff/, /node-modules/],
           use: ["elm-hot-webpack-loader", "elm-webpack-loader"],
         },
       ],
     },
     plugins: [
       new HtmlWebpackPlugin({
         template: "./src/static/index.html",
         filename: "index.html",
         inject: "body",
       }),
     ],
     devServer: {
       static: {
         directory: Path.resolve(__dirname, "dist"),
       },
       hot: true,
     },
   };
   ```

## Static Files

1. Enter HTML in `index.html`
   ```html
   <!DOCTYPE html>
   <html lang="en">
     <head>
       <meta charset="UTF-8" />
       <meta http-equiv="X-UA-Compatible" content="IE=edge" />
       <meta name="viewport" content="width=device-width, initial-scale=1.0" />
       <title>My Cool Elm App</title>
     </head>
     <body>
       <div id="app">Loading app...</div>
     </body>
   </html>
   ```
1. Enter CSS in `styles.css`

   ```css
   body {
     box-sizing: border-box;
   }

   body *,
   *::before,
   *::after {
     box-sizing: inherit;
   }
   ```

1. Enter JS in `index.js`

   ```js
   const { Elm } = require("../elm/Main.elm");

   Elm.Main.init({ node: document.getElementById("app") });
   ```

## Elm

1. Update `elm.json`

   ```json
    "source-directories": ["src/elm"]
   ```

1. Enter starter Elm Architecture code in `Main.elm`

   ```elm
    module Main exposing (main)

    import Browser
    import Html exposing (..)
    import Html.Attributes exposing (..)
    import Html.Events exposing (onClick)


    type Msg
        = Increment
        | Decrement


    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Increment ->
                ( model + 1, Cmd.none )

            Decrement ->
                ( model - 1, Cmd.none )


    type alias Model =
        Int


    initialModel : Model
    initialModel =
        0


    view : Model -> Html Msg
    view model =
        div []
            [ h1 [] [ text "Hello, World!" ]
            , div [ class "counter" ]
                [ div [] [ text (String.fromInt model) ]
                , button [ onClick Decrement ] [ text "-" ]
                , button [ onClick Increment ] [ text "+" ]
                ]
            ]


    main : Program () Model Msg
    main =
        Browser.element
            { init = \flags -> ( initialModel, Cmd.none )
            , view = view
            , update = update
            , subscriptions = \model -> Sub.none
            }

   ```
