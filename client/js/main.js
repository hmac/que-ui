import React from 'react';
import Router from 'react-router';
const {Route, DefaultRoute, Redirect} = Router;

import AppHandler from 'js/AppHandler';
import QueueSummaryRoute from 'js/QueueSummaryRoute';
import JobListRoute from 'js/JobListRoute';
import FailuresRoute from 'js/FailuresRoute';
import JobRoute from 'js/JobRoute';
import WorkersRoute from 'js/WorkersRoute';

const routes = (
  <Route path="/" handler={AppHandler}>
    <Route name="queue-summary" path="/queue/:queueName" handler={QueueSummaryRoute} />
    <Route name="job-list" path="/jobs" handler={JobListRoute} />
    <Route name="failures" path="/failures" handler={FailuresRoute} />
    <Route name="job" path="/job/:jobId" handler={JobRoute} />
    <Route name="workers" path="/workers" handler={WorkersRoute} />
    <Redirect from="/" to="/queue/_default"/>
  </Route>
);

function fetchRouteData(routes, params, query) {
  const data = {};
  return Promise.all(routes
    .filter(route => route.handler.fetchData)
    .map(route => {
      return route.handler.fetchData(params, query).then(d => {data[route.name] = d;});
    })
  ).then(() => data);
}

function callIfVisible(fn, interval) {
  setTimeout(() => {
    if (document.visibilityState !== 'hidden') {
      fn();
    } else {
      callIfVisible(fn, interval);
    }
  }, interval);
}

let currentState = null;
Router.run(routes, function(Handler, state) {
  currentState = state;

  const rerender = function() {
    if (currentState !== state) {
      return;
    }

    fetchRouteData(state.routes, state.params, state.query).then(data => {
      // If state is not the currentState, then this data was requested for
      // another page (i.e. we have navigated away, so this can be discarded)
      if (currentState === state) {
        React.render(<Handler data={data}/>, document.getElementById('container'));
        callIfVisible(rerender, 1000);
      }
    });
  };
  rerender();
});
