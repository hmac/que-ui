import React, { Component } from "react";
import { connect } from "react-redux";
import { fetchJobs } from "./Actions";
import jobStatus from "./JobStatus";
import { push } from "react-router-redux";

class Jobs_ extends Component {
  render() {
    const jobs = this.props.jobs;
    if (jobs.length === 0) {
      return <div className="cleanscreen-message">No matching jobs found!</div>;
    }

    const rows = jobs.map((job) => <Job key={job.job_id} navigate={this.props.navigate} {...job} />);
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

  componentDidMount() {
    this.props.refresh(this.props.location.search);
    this.timer = setInterval(() => this.props.refresh(this.props.location.search), 1000);
  }

  componentWillUnmount() {
    clearInterval(this.timer);
  }
}

const Job = ({navigate, ...job}) => {
  const status = jobStatus(job);
  return (
    <tr
      key={job.job_id} className="table-row table-row--link"
      onClick={ () => { navigate(`/jobs/${ job.job_id }`); } }
    >
      <td className={"table-cell job-list-id--" + status + " u-center"}>{job.job_id}</td>
      <td className="table-cell">{job.job_class}</td>
      <td className="table-cell"><code>{JSON.stringify(job.args)}</code></td>
      <td className="table-cell">{job.queue || "(default)"}</td>
      <td className="table-cell u-center">{job.priority}</td>
      <td className="table-cell u-center">{job.error_count}</td>
    </tr>
  );
};

const Jobs = connect(
  ({ jobs }) => { return { jobs }; },
  (dispatch) => {
    return {
      refresh: (search) => dispatch(fetchJobs(search)),
      navigate: (path) => dispatch(push(path))
    };
  }
)(Jobs_);

export default Jobs;
