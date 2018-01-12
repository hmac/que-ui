import React, { Component } from "react";
import { connect } from "react-redux";
import jobStatus from "./JobStatus"
import { fetchJob } from "./Actions";

const Retry = (status) => {
  if (status !== "failed") { return null; }
  return <button>Retry</button>;
};

const Destroy = (status) => {
  if (status !== "destroyed") { return null; }
  return <button className="danger">Destroy</button>;
};

class Job_ extends Component {
  renderError(error) {
    if (!error) { return null; }
    return (
      <div className="row">
        <span className="one columns"></span>
        <span className="job-detail__key two columns">Last error</span>
        <span className="job-detail__value eight columns">
          <pre>{ error }</pre>
        </span>
      </div>
    );
  }

  componentDidMount() {
    this.props.refresh(this.props.id);
    this.timer = setInterval(() => this.props.refresh(this.props.id), 1000);
  }

  componentWillUnmount() {
    clearInterval(this.timer);
  }

  render() {
    const job = this.props.job;
    if (!job) {
      return (
        <div className="cleanscreen-message">
          <p>Job not found!</p>
          <p>This probably means the job has been processed or deleted.</p>
        </div>
      );
    }

    const status = jobStatus(job);

    return (
      <div className="job-detail">
        <div className="row">
          <span className="job-detail__id two columns">
            { job.job_id }
          </span>
          <span className="job-detail__title eight columns">
            { job.job_class }
          </span>
          <span className="two columns">
            <span className={"job-detail__status job-detail__status--" + status}>
              { status }
            </span>
          </span>
        </div>
        <div className="row">
          <span className="one columns"></span>
          <span className="job-detail__key two columns">Run at</span>
          <span className="job-detail__value three columns" title={ job.run_at || "-" }>
            { new Date(job.run_at).toUTCString() }
          </span>
          <span className="job-detail__key two columns">Queue</span>
          <span className="job-detail__value three columns">{ job.queue || "(default)" }</span>
        </div>
        <div className="row">
          <span className="one columns"></span>
          <span className="job-detail__key two columns">Retryable</span>
          <code className="job-detail__value three columns">{ job.retryable ? "true" : "false" }</code>
          <span className="job-detail__key two columns">Priority</span>
          <span className="job-detail__value three columns">{ job.priority }</span>
        </div>
        <div className="row">
          <span className="one columns"></span>
          <span className="job-detail__key two columns">Failures</span>
          <span className="job-detail__value three columns">{ job.error_count }</span>
          <span className="job-detail__key two columns">Last failed at</span>
          <span className="job-detail__value three columns" title={ job.failed_at || "=" }>
            { new Date(job.failed_at).toUTCString() }
          </span>
        </div>
        <div className="row">
          <span className="one columns"></span>
          <span className="job-detail__key two columns">Arguments</span>
          <span className="job-detail__value eight columns">
            <code>{JSON.stringify(job.args)}</code>
          </span>
        </div>
        { this.renderError(job.last_error) }
        <div className="row">
          <span className="twelve columns" style={{textAlign: "center"}}>
            <Retry status={ status }/>
            <Destroy status={ status }/>
          </span>
        </div>
      </div>
    );
  }
}

const Job = connect(
  ({ job }, { match: { params: { id } } }) => { return { job, id }; },
  (dispatch) => { return { refresh: (id) => dispatch(fetchJob(id)) }; }
)(Job_);

export default Job;
