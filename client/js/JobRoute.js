import React from 'react';
import moment from 'moment';
import utils from 'js/utils';

export default class JobRoute extends React.Component {
  static fetchData(params) {
    return utils.apiRequest(`jobs/${params.jobId}`);
  }

  constructor(props) {
    super(props);
    this.state = { job: props.data.job, destroyed: false };
  }

  componentWillReceiveProps(newProps) {
    if (!newProps.data.job) {
      this.setState({ destroyed: true });
    } else {
      this.setState({ job: newProps.data.job });
    }
  }

  render() {
    if (!this.job) {
      return (
        <div className="cleanscreen-message">
          Job not found!<br/>
          This probably means the job has been processed or deleted.
        </div>
      );
    }

    const runAt = moment(this.job.run_at);
    const failedAt = moment(this.job.failed_at);
    const status = this.jobStatus();

    let lastError = null;
    if (this.job.last_error) {
      lastError = (
        <div className="row">
          <span className="one columns"></span>
          <span className="job-detail__key two columns">Last error</span>
          <span className="job-detail__value eight columns">
            <pre>{this.job.last_error}</pre>
          </span>
        </div>
      );
    }

    let retryButton = null;
    if (status === 'inactive') {
      retryButton = <button onClick={this.retryJob.bind(this)}>Retry</button>;
    }

    let deleteButton = null;
    if (status != 'destroyed') {
      deleteButton = <button className="danger" onClick={this.deleteJob.bind(this)}>Delete</button>;
    }

    return (
      <div className="job-detail">
        <div className="row">
          <span className="job-detail__id two columns">
            Job #{this.job.job_id}
          </span>
          <span className="job-detail__title eight columns">
            {this.job.job_class}
          </span>
          <span className="two columns">
            <span className={"job-detail__status job-detail__status--" + status}>
              {status}
            </span>
          </span>
        </div>
        <div className="row">
          <span className="one columns"></span>
          <span className="job-detail__key two columns">Run at</span>
          <span className="job-detail__value three columns" title={runAt.format('L LTS ZZ')}>
            {runAt.fromNow()}
          </span>
          <span className="job-detail__key two columns">Queue</span>
          <span className="job-detail__value three columns">{this.job.queue || '(default)'}</span>
        </div>
        <div className="row">
          <span className="one columns"></span>
          <span className="job-detail__key two columns">Retryable</span>
          <span className="job-detail__value three columns">{this.job.retryable ? 'yes' : 'no'}</span>
          <span className="job-detail__key two columns">Priority</span>
          <span className="job-detail__value three columns">{this.job.priority}</span>
        </div>
        <div className="row">
          <span className="one columns"></span>
          <span className="job-detail__key two columns">Failures</span>
          <span className="job-detail__value three columns">{this.job.error_count}</span>
          <span className="job-detail__key two columns">Last failed at</span>
          <span className="job-detail__value three columns" title={failedAt.isValid() ? failedAt.format('L LTS ZZ') : '–'}>
            {failedAt.isValid() ? failedAt.fromNow() : '–'}
          </span>
        </div>
        <div className="row">
          <span className="one columns"></span>
          <span className="job-detail__key two columns">Arguments</span>
          <span className="job-detail__value eight columns">
            <code>{JSON.stringify(this.job.args)}</code>
          </span>
        </div>
        {lastError}
        <div className="row">
          <span className="twelve columns" style={{textAlign: 'center'}}>
            {retryButton}
            {deleteButton}
          </span>
        </div>
      </div>
    );
  }

  retryJob() {
    if (confirm('Are you sure you want to retry this job?')) {
      const path = `jobs/${this.job.job_id}/retry`;
      utils.apiRequest(path, { method: 'POST' });
    }
  }

  deleteJob() {
    if (confirm('Are you absolutely sure you want to delete this job?')) {
      const path = `jobs/${this.job.job_id}/delete`;
      utils.apiRequest(path, { method: 'POST' });
    }
  }

  get job() {
    return this.state.job;
  }

  jobStatus() {
    if (this.state.destroyed) {
      return 'destroyed';
    }

    if (!this.job.retryable) {
      return 'inactive';
    }

    if (this.job.scheduled_for_future) {
      return 'scheduled';
    }

    return 'queued';
  }

  get retryable() {
    return !this.job.retryable && !this.job.scheduled_for_future;
  }
}
