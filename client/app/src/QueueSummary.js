import React, { Component } from "react";
import { connect } from "react-redux";
import { Link } from "react-router-dom";
import { fetchSummaries } from "./Actions";

class QueueSummary_ extends Component {
  render() {
    const summaries = this.props.summaries;
    const key = ({ job_class, priority }) => `${job_class}--${priority}`;
    return (
      <div className="grid">
        {
          summaries.map((summary) => {
            return <SummaryItem key={key(summary)} {...summary} />;
          })
        }
      </div>
    );
  }

  componentDidMount() {
    this.props.refresh();
    this.timer = setInterval(this.props.refresh, 1000);
  }

  componentWillUnmount() {
    clearInterval(this.timer);
  }
}

const SummaryItem = ({job_class, priority, count, count_working}) => {
  const search = new URLSearchParams({job_class, priority}).toString();
  return (
    <div className="grid-row row">
      <Link to={ { pathname: "/jobs", search } } className="grid-row-link twelve columns">
        <span className="seven columns grid-cell-text">
          {job_class}
        </span>
        <span className="two columns">
          <span className="grid-cell-value">{count}</span>
          <span className="grid-cell-label">count</span>
        </span>
        <span className="two columns">
          <span className="grid-cell-value">{priority}</span>
          <span className="grid-cell-label">priority</span>
        </span>
        <span className="one columns">
          {count_working > 0 && <div className="spinner">Working</div>}
        </span>
      </Link>
    </div>
  );
};

const QueueSummary = connect(
  ({ summaries }) => { return { summaries }; },
  (dispatch) => { return { refresh: () => dispatch(fetchSummaries) }; }
)(QueueSummary_);

export default QueueSummary;
