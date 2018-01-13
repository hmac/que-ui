import React, { Component } from "react";
import { Route, Link, Switch } from "react-router-dom";

import QueueSummary from "./QueueSummary.js";
import Jobs from "./Jobs.js";
import Failures from "./Failures.js";
import Job from "./Job.js";
import Workers from "./Workers.js";
import LastUpdated from "./LastUpdated.js"

class App extends Component {
  render() {
    return (
      <section className="app">
        <header className="app-header">
          <h1 className="u-pull-left u-cf app-header__title">Que Web UI</h1>
          <nav className="u-pull-left app-header__nav">
            <Link to="/queue-summary" className="app-header__nav__link">Queue Summary</Link>
            <Link to="/jobs" className="app-header__nav__link">All Jobs</Link>
            <Link to="/failures" className="app-header__nav__link">Failed Jobs</Link>
            <Link to="/workers" className="app-header__nav__link">Workers</Link>
          </nav>
          <LastUpdated className="app-header__nav__link u-pull-right"/>
          <br className="u-cf" />
        </header>
        <section className="app-content">
          <Switch>
            <Route path="/queue-summary" component={QueueSummary}/>
            <Route path="/jobs/:id" component={Job}/>
            <Route path="/jobs" component={Jobs}/>
            <Route path="/failures" component={Failures}/>
            <Route path="/workers" component={Workers}/>
            <Route path="/" component={QueueSummary}/>
          </Switch>
        </section>
      </section>
    );
  }
}

export default App;
