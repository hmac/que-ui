import React, { Component } from "react";
import { connect } from "react-redux";
import { push } from "react-router-redux";
import { fetchWorkers } from "./Actions";

class Workers_ extends Component {
  render() {
    const workers = this.props.workers;
    if (workers.length === 0) {
      return (
        <div className="cleanscreen-message">No workers currently active</div>
      );
    }
    return workers.map(worker => {
      const path = `/jobs/${ worker.job_id }`;
      return (
        <div key={ worker.pid } className="grid-row row">
          <a className="grid-row-link twelve columns" onClick={() => this.props.navigate(path)}>
            <span className="grid-cell-text four columns">
              { worker.job_class }
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                { worker.pid }
              </span>
              <span className="grid-cell-label">pid</span>
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                { worker.queue || "(default)" }
              </span>
              <span className="grid-cell-label">queue</span>
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                { worker.job_id }
              </span>
              <span className="grid-cell-label">job id</span>
            </span>
            <span className="two columns">
              <span className="grid-cell-value">
                { worker.processing_time }
              </span>
              <span className="grid-cell-label">time</span>
            </span>
          </a>
        </div>
      );
    });
  }

  componentWillMount() {
    this.props.refresh();
  }

  componentDidMount() {
    this.timer = setInterval(() => this.props.refresh(), 1000);
  }

  componentWillUnmount() {
    clearInterval(this.timer);
  }
}

const Workers = connect(
  ({ workers }) => { return { workers }; },
  (dispatch) => {
    return {
      refresh: () => dispatch(fetchWorkers),
      navigate: (path) => dispatch(push(path))
    };
  }
)(Workers_);

export default Workers;
