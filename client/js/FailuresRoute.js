import React from 'react';
import utils from 'js/utils';

export default class FailuresRoute extends React.Component {
  static fetchData(params) {
    return utils.apiRequest('failures');
  }

  static contextTypes = {
    router: React.PropTypes.func
  };

  render() {
    if (this.jobClassFailures.length === 0) {
      return <div className="cleanscreen-message">No failed jobs!</div>;
    }

    const rows = this.jobClassFailures.map(jobClass => {
      return (
        <div key={jobClass.job_class} className="grid-row row">
          <a className="grid-row-link seven columns" onClick={this.viewFailedJobs.bind(this, jobClass.job_class)}>
            <span className="grid-cell-text six columns">
              {jobClass.job_class}
            </span>
            <span className="three columns">
              <span className="grid-cell-value">
                {utils.formatNumber(jobClass.pending_retry)}
              </span>
              <span className="grid-cell-label">pending retry</span>
            </span>
            <span className="three columns">
              <span className="grid-cell-value">
                {utils.formatNumber(jobClass.inactive)}
              </span>
              <span className="grid-cell-label">failed</span>
            </span>
          </a>
          <span className="five columns">
            <button onClick={this.retryJobs.bind(this, jobClass)} disabled={jobClass.inactive == 0}>
              Retry {jobClass.inactive} jobs
            </button>
            <button className="danger" disabled={jobClass.inactive == 0} onClick={this.deleteJobs.bind(this, jobClass)}>
              Delete {jobClass.inactive} jobs
            </button>
          </span>
        </div>
      );
    });

    return <div className="grid">{rows}</div>;
  }

  viewFailedJobs(jobClass) {
    const query = {job_class: jobClass, failed: 'true'};
    this.context.router.transitionTo('job-list', {}, query);
  }

  retryJobs(jobClass) {
    if (confirm(`Are you sure you want to retry all ${jobClass.inactive} failed jobs?`)) {
      const path = `failures/${encodeURIComponent(jobClass.job_class)}/retry_failed`;
      utils.apiRequest(path, { method: 'POST' });
    }
  }

  deleteJobs(jobClass) {
    if (confirm(`Are you sure you want to delete all ${jobClass.inactive} failed jobs?`)) {
      const path = `failures/${encodeURIComponent(jobClass.job_class)}/delete_failed`;
      utils.apiRequest(path, { method: 'POST' });
    }
  }

  get jobClassFailures() {
    return this.props.data['failures'];
  }
}
