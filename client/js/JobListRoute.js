import React from 'react';
import utils from 'js/utils';

export default class JobListRoute extends React.Component {
  static fetchData(params, query) {
    const queryString = Object.keys(query).map(key => {
      return `${encodeURIComponent(key)}=${encodeURIComponent(query[key])}`;
    }).join('&');
    const path = 'jobs?' + queryString;
    return utils.apiRequest(path);
  }

  static contextTypes = {
    router: React.PropTypes.func
  };

  render() {
    if (this.jobList.length === 0) {
      return <div className="cleanscreen-message">No matching jobs found!</div>;
    }

    const rows = this.jobList.map(job => {
      const jobKey = `${job.job_class}--${job.priority}`;
      const status = this.jobStatus(job);
      return (
        <tr key={job.job_id} className="table-row table-row--link" onClick={this.viewJob.bind(this, job.job_id)}>
          <td className={'table-cell job-list-id--' + this.jobStatus(job) + ' u-center'}>{job.job_id}</td>
          <td className="table-cell">{job.job_class}</td>
          <td className="table-cell"><code>{JSON.stringify(job.args)}</code></td>
          <td className="table-cell">{job.queue || '(default)'}</td>
          <td className="table-cell u-center">{utils.formatNumber(job.priority)}</td>
          <td className="table-cell u-center">{utils.formatNumber(job.error_count)}</td>
        </tr>
      );
    });

    return (
      <table className="table">
        <thead>
          <tr className="table-row">
            <th className="table-heading job-list-id">Job ID</th>
            <th className="table-heading job-list-class">Job Class</th>
            <th className="table-heading job-list-args">Arguments</th>
            <th className="table-heading job-list-queue">Queue</th>
            <th className="table-heading job-list-priority">Priority</th>
            <th className="table-heading job-list-errors">Errors</th>
          </tr>
        </thead>
        <tbody>
          {rows}
        </tbody>
      </table>
    );
  }

  viewJob(jobId) {
    this.context.router.transitionTo('job', { jobId: jobId });
  }

  jobStatus(job) {
    if (!job.retryable) {
      return 'inactive';
    }

    if (job.scheduled_for_future) {
      return 'scheduled';
    }

    return 'queued';
  }

  get jobList() {
    return this.props.data['job-list'];
  }
}
