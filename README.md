# MrLasers Webpack Starter for Elm

Steps for creating a new Elm project using webpack with hot reloading and all that jazz.

# Setup

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
1. Add `scripts` to `package.json`
   ```json
   "scripts": {
     "dev": "webpack serve --mode=development",
     "build": "webpack --mode-production"
   }
   ```
1. Create file structure
   ```
   +- src
   | +- elm
   |   - Main.elm
   | +- static
   |   +- styles
   |     - styles.css
   |   - index.html
   |   - index.js
   ```
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

1. Update `elm.json`

   ```json
    "source-directories": ["src/elm"]
   ```

1. Enter starter Elm Architecture code in `Main.elm`
   ```elm
   -- elm code goes here
   ```
1. Create `webpack.config.js`

   ```js
   /* webpack config goes here */
   ```

1. Enter JS in `index.js`
   ```js
   /* JS code goes here */
   ```
