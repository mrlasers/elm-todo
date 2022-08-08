const Path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const ExtractTextPlugin = require('extract-text-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const { merge } = require('webpack-merge')

const isDev = process.env.npm_lifecycle_event === 'build' ? false : true

const commonConfig = {
  // entry: ['./src/static/index.js', './src/static/styles/main.scss'],

  output: {
    path: Path.resolve(__dirname, 'dist'),
    filename: '[name].js',
  },
  resolve: {
    extensions: ['.js', '.elm'],
  },
  module: {
    noParse: /\.elm$/,
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './src/static/index.html',
      filename: 'index.html',
      inject: 'body',
    }),
  ],
}
const devConfig = {
  entry: [
    'webpack-dev-server/client?http://localhost:8080',
    './src/static/index.js',
    './src/static/styles/main.scss',
  ],
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node-modules/],
        use: ['elm-hot-webpack-loader', 'elm-webpack-loader'],
      },
      {
        test: /\.sc?ss$/,
        use: ['style-loader', 'css-loader', 'sass-loader'],
      },
    ],
  },
  output: {
    publicPath: '/', // this needed for multi-segment fallback
  },
  devServer: {
    static: {
      directory: Path.resolve(__dirname, 'dist'),
    },
    hot: true,
    historyApiFallback: true,
  },
}

const prodConfig = {
  entry: ['./src/static/index.js', './src/static/styles/main.scss'],
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node-modules/],
        use: ['elm-webpack-loader'],
      },
      {
        test: /\.sc?ss$/,
        use: [
          MiniCssExtractPlugin.loader,
          // 'style-loader',
          'css-loader',
          'sass-loader',
        ],
        // use: MiniCssExtractPlugin.extract({
        //   fallback: 'style-loader',
        //   use: ['css-loader', 'sass-loader'],
        // }),
      },
    ],
  },
  plugins: [new MiniCssExtractPlugin()],
}

module.exports = (env, argv) =>
  merge(commonConfig, argv.mode === 'development' ? devConfig : prodConfig)
