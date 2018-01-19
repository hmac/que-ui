import React from "react";
import ReactDOM from "react-dom";
import thunkMiddleware from "redux-thunk";
import { ConnectedRouter, routerMiddleware } from "react-router-redux";
import { Provider } from "react-redux";
import { createStore, applyMiddleware } from "redux";
import createHistory from 'history/createBrowserHistory';

import App from "./App";
import reducer from "./Reducer"

const history = createHistory();

const store = createStore(
  reducer,
  applyMiddleware(
    thunkMiddleware,
    routerMiddleware(history)
  )
);

ReactDOM.render(
  <Provider store={store}>
    <ConnectedRouter history={ history }>
      <App />
    </ConnectedRouter>
  </Provider>,
  document.getElementById("root")
);
