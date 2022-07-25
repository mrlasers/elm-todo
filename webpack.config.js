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
