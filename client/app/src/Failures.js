import React, { Component } from "react";
import { connect } from "react-redux";
import { Link } from "react-router-dom";
import { fetchFailedJobs, retryJobs, destroyJobs } from "./Actions.js";

class Failures_ extends Component {
  render() {
    const failures = this.props.failures;
    if (failures.length === 0) {
      return <div className="cleanscreen-message">No failed jobs</div>;
    }
    const key = ({ job_class, priority }) => `${job_class}--${priority}`;
    const retry = this.props.retry;
    const destroy = this.props.destroy;
    return (
      <div className="grid">
        {
          failures.map((failure) => {
            return <Failure
              key={key(failure)}
              retry={ retry }
              destroy={ destroy }
              {...failure}
            />;
          })
        }
      </div>
    );
  }

  componentWillMount() {
    this.props.refresh();
  }

  componentDidMount() {
    this.timer = setInterval(this.props.refresh, 1000);
  }

  componentWillUnmount() {
    clearInterval(this.timer);
  }
}

const Failure = ({ job_class, pending_retry, inactive, retry, destroy }) => {
  const search = new URLSearchParams({ job_class, failed: true }).toString();
  return (
    <div className="grid-row row">
      <Link to={ { pathname: "/jobs", search } } className="grid-row-link seven columns">
        <span className="six columns grid-cell-text">
          {job_class}
        </span>
        <span className="three columns">
          <span className="grid-cell-value">{pending_retry}</span>
          <span className="grid-cell-label">pending retry</span>
        </span>
        <span className="three columns">
          <span className="grid-cell-value">{inactive}</span>
          <span className="grid-cell-label">failed</span>
        </span>
      </Link>
      <span className="five columns">
        <button disabled={ inactive === 0 } onClick={ () => retry(job_class) }>
          Retry { inactive } jobs
        </button>
        <button className="danger" disabled={ inactive === 0 } onClick={ () => destroy(job_class) }>
          Delete { inactive } jobs
        </button>
      </span>
    </div>
  );
};

const Failures = connect(
  ({ failures }) => { return { failures }; },
  (dispatch) => {
    return {
      refresh: () => dispatch(fetchFailedJobs),
      retry: (job_class) => dispatch(retryJobs(job_class)),
      destroy: (job_class) => dispatch(destroyJobs(job_class))
    };
  }
)(Failures_);

export default Failures;
