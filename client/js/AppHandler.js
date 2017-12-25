import React from 'react';
import Router from 'react-router';

export default class AppHandler extends React.Component {
  render() {
    return (
      <section className='app'>
        <header className='app-header'>
          <h1 className='u-pull-left u-cf app-header__title'>Que Web UI</h1>
          <nav className='u-pull-left app-header__nav'>
            <Router.Link className="app-header__nav__link" to="queue-summary" params={{queueName: '_default'}}>Queue summary</Router.Link>
            <Router.Link className="app-header__nav__link" to="job-list">Job list</Router.Link>
            <Router.Link className="app-header__nav__link" to="failures">Failures</Router.Link>
            <Router.Link className="app-header__nav__link" to="workers">Workers</Router.Link>
            <br className='u-cf' />
          </nav>
        </header>
        <section className='app-content'>
          <Router.RouteHandler {...this.props} />
        </section>
      </section>
    );
  }
}
