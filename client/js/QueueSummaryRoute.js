import React from 'react';
import Router from 'react-router';
const { Link } = Router;
import utils from 'js/utils';

export default class QueueSummaryRoute extends React.Component {
  static fetchData(params) {
    const path = `queue-summary/${params.queueName}`;
    return utils.apiRequest(path);
  }

  static contextTypes = {
    router: React.PropTypes.func
  };

  constructor(props) {
    super(props);
    this.state = { queue: '_default' };
  }

  render() {
    if (this.queueChunks.length === 0) {
      return <div className="cleanscreen-message">No jobs in the queue!</div>;
    }

    const rows = this.queueChunks.map(job => {
      const jobKey = `${job.job_class}--${job.priority}`;
      return (
        <div key={jobKey} className="grid-row row">
          <a className="grid-row-link twelve columns" onClick={this.viewJobs.bind(this, job)}>
            <span className="seven columns grid-cell-text">
              {job.job_class}
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                {utils.formatNumber(job.count)}
              </span>
              <span className="grid-cell-label">count</span>
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                {utils.formatNumber(job.priority)}
              </span>
              <span className="grid-cell-label">priority</span>
            </span>
            <span className="one columns">
              {job.count_working > 0 ? <div className="spinner">Working</div> : null}
            </span>
          </a>
        </div>
      );
    });

    return <div className="grid">{rows}</div>;
  }

  viewJobs(job) {
    const query = {job_class: job.job_class, priority: job.priority};
    this.context.router.transitionTo('job-list', {}, query);
  }

  get queueChunks() {
    return this.props.data['queue-summary'];
  }
}
