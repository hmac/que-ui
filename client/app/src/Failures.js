import React, { Component } from "react";
import { connect } from "react-redux";
import { fetchFailedJobs } from "./Actions.js";

class Failures_ extends Component {
  render() {
    const failures = this.props.failures;
    if (failures.length === 0) {
      return <div className="cleanscreen-message">No failed jobs</div>;
    }
    const key = ({ job_class, priority }) => `${job_class}--${priority}`;
    return (
      <div className="grid">
        {
          failures.map((failure) => {
            return <Failure key={key(failure)} {...failure} />;
          })
        }
      </div>
    );
  }

  componentDidMount() {
    this.timer = setInterval(this.props.refresh, 1000);
  }

  componentWillUnmount() {
    clearInterval(this.timer);
  }
}

const Failure = ({ job_class, pending_retry, inactive }) => {
  return (
    <div className="grid-row row">
      <a className="grid-row-link seven columns">
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
      </a>
      <span className="five columns">
        <button disabled={ inactive === 0 }>
          Retry { inactive } jobs
        </button>
        <button className="danger" disabled={ inactive === 0 }>
          Delete { inactive } jobs
        </button>
      </span>
    </div>
  );
};

const Failures = connect(
  ({ failures }) => { return { failures }; },
  (dispatch) => { return { refresh: () => dispatch(fetchFailedJobs) }; }
)(Failures_);

export default Failures;
