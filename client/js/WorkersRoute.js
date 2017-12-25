import React from 'react';
import utils from 'js/utils';

export default class WorkersRoute extends React.Component {
  static fetchData(params) {
    return utils.apiRequest('workers');
  }

  static contextTypes = {
    router: React.PropTypes.func
  };

  render() {
    if (this.workers.length === 0) {
      return <div className="cleanscreen-message">No workers currently active</div>;
    }

    const rows = this.workers.map(worker => {
      return (
        <div key={worker.pid} className="grid-row row">
          <a className="grid-row-link twelve columns" onClick={this.viewJob.bind(this, worker.job_id)}>
            <span className="grid-cell-text four columns">
              {worker.job_class}
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                {worker.pid}
              </span>
              <span className="grid-cell-label">pid</span>
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                {worker.queue || '(default)'}
              </span>
              <span className="grid-cell-label">queue</span>
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                {worker.job_id}
              </span>
              <span className="grid-cell-label">job id</span>
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                {worker.processing_time}
              </span>
              <span className="grid-cell-label">time</span>
            </span>
          </a>
        </div>
      );
    });

    return <div className="grid">{rows}</div>;
  }

  viewJob(jobId) {
    this.context.router.transitionTo('job', { jobId: jobId });
  }

  get workers() {
    return this.props.data['workers'];
  }
}
