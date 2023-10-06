# elm-material

[![Elm package](https://img.shields.io/elm-package/v/lattyware/elm-material?logo=elm)](https://package.elm-lang.org/packages/lattyware/elm-material/latest/)
![Material Design version](https://img.shields.io/github/package-json/dependency-version/lattyware/elm-material/%40material%2Fweb?logo=materialdesign&label=Material%20Web)

An elm library for [Material Web](https://github.com/material-components/material-web), allowing you to easily use the components from within Elm.

## Usage

You will need to set up the web components in JS before using the Elm library. This repository provides a node package that can be installed and used, with a convinience import that includes all the components, but generally it is better to just use the `@material/web` package directly, however it is strongly recommended to use the version this was developed against in [package.json](package.json), or there may be incompatabilities. Importing only the relevant modules for your use case will help reduce overall application size.

You may also wish to configure a bundler for minification and/or polyfilling for the web components, as per [Material Web's guide](https://material-web.dev/about/quick-start/#building).

Use the library in Elm as per [the documentation](https://package.elm-lang.org/packages/lattyware/elm-material/latest/).

Generally the API is designed to reflect the web component's capabilities, but with additional safety from the Elm layer, it is set up in a pipeline style to allow for more structure, but still easy access to a wide array of options.

The one main addition is the that of replaced links. In Elm applications links are intercepted and their action of loading a page is replaced with messages internal to Elm. Unfortunately in web components, this interception won't happen. To deal with this, where the material components integrate links, this package offers "replaced links", where the link has an attached event handler in Elm that intercepts the click on the component explicitly.

## Styling

Styling is done through CSS Custom Properties, and so is no different for Elm than for the web components directly, following [Material Web's guide](https://material-web.dev/theming/material-theming/) should work well.
